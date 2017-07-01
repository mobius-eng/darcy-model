(in-package cl-user)

(defpackage :darcy-model
  (:use #:cl #:unsaturated #:conductivity #:darcy-utils #:inlet-discharge #:cl-lsoda)
  (:export
   #:darcy #:space-step #:conductivities #:unsaturated-models #:inlet-discharge
   #:darcy-size #:darcy-points #:darcy-boundaries #:darcy-depth
   #:darcy-water-volume
   #:conductivity-at
   #:pressure-at
   #:darcy-inlet-discharge #:darcy-outlet-discharge
   #:full-conductivity
   #:richards-equation
   #:darcy-evolve))

(in-package darcy-model)
;; * Darcy model

(declaim (optimize (speed 3) (safety 0)))

;; ** Darcy Model implementation
;; *** Class definition
(defclass darcy ()
  ((space-step
    :initarg :space-step
    :initform 0.05d0
    :type double-float
    :accessor space-step
    :documentation "Spatial step size (m)")
   (conductivities
    :initarg :conductivities
    :accessor conductivities
    :initform nil
    :type (simple-array conductivity *)
    :documentation "Saturated conductivities part of Darcy's model")
   (unsaturated-models
    :initarg :unsaturated-models
    :initform nil
    :accessor unsaturated-models
    :type (simple-array Unsaturated *)
    :documentation "Unsaturated (sub-)models of Darcy's model")
   (inlet-discharge
    :initarg :inlet-discharge
    :initform (make-instance 'constant-inlet-discharge
                :inlet-flow-rate (/ 10d0 1000d0 3600d0))
    :accessor inlet-discharge
    :documentation "Inlet specific discharge (m/s)"))
  (:documentation
   "Representation of the full 1D discretized Darcy model."))

;; *** Printing
(defmethod print-object ((obj darcy) out)
  (with-slots (space-step conductivities unsaturated-models inlet-discharge) obj
    (print-unreadable-object (obj out :type t)
      (format out "~@<~:_dz = ~A ~:_conductivity = ~A ~:_unsaturated = ~A \
~:_inlet discharge = ~A ~:>"
              space-step conductivities unsaturated-models inlet-discharge))))

;; *** Utility reader methods
(defmethod darcy-size ((model darcy))
  "Returns number of mesh points"
  (declare (optimize (speed 3)))
  (length (the simple-array (unsaturated-models model))))

(defmethod darcy-points ((model darcy))
  "Returns mesh points (centres of volumes)"
  (with-slots ((dz space-step)) model
    (apply #'vector
           (loop for i from 0 below (darcy-size model)
              collect (+ (/ dz 2) (* dz i))))))

(defmethod darcy-boundaries ((model darcy))
  "Returns the mesh boundaries of model volumes"
  (with-slots ((dz space-step)) model
    (apply #'vector
           (loop for i from 0 upto (darcy-size model)
              collect (* dz i)))))

(defmethod darcy-depth ((model darcy))
  "Depth (or height) of the packing"
  (with-slots ((dz space-step)) model
    (* dz (darcy-size model))))


(defmethod darcy-water-volume ((model darcy) saturation &optional (area 1d0))
  (with-slots (unsaturated-models space-step) model
    (init-array
     (lambda (i)
       (* (water-content (aref unsaturated-models i) (aref saturation i))
          (darcy-depth model)
          area
          space-step))
     (length unsaturated-models)
     'double-float)))

;; *** Utility operating functions
(defmethod pressure ((model darcy))
  (with-slots (unsaturated-models) model
    (lambda (effsat)
      (apply #'vector
             (loop for u across unsaturated-models
                for s across effsat
                collect (funcall (pressure u) s))))))

(defmethod relative-conductivity ((model darcy))
  (with-slots (unsaturated-models) model
    (lambda (effsat)
      (apply #'vector
             (loop for u across unsaturated-models
                for s across effsat
                collect (funcall (relative-conductivity u) s))))))

(defmethod full-conductivity ((model darcy))
  (with-slots (unsaturated-models conductivities) model
    (lambda (effsat)
      (apply #'vector
             (loop for u across unsaturated-models
                for c across conductivities
                for s across effsat
                collect (* (funcall (relative-conductivity u) effsat)
                           (saturated-conductivity c)))))))


(declaim (inline darcy-inlet-discharge))
(defun darcy-inlet-discharge (model time)
  (funcall (the function (inlet-discharge model)) time))

(declaim (inline darcy-outlet-discharge))
(defun darcy-outlet-discharge (model saturation)
  (let* ((u (unsaturated-models model))
         (c (conductivities model))
         (n (1- (length u))))
    (* (funcall (relative-conductivity (aref u n)) (aref saturation n))
       (saturated-conductivity (aref c n)))))

(declaim (inline darcy-flux))
(defun darcy-flux (conductivity pressure-next pressure-prev step)
  (declare (type double-float conductivity pressure-next pressure-prev step)
           (optimize (speed 3)))
  (- (* conductivity (- (/ (- pressure-next pressure-prev) step) 1d0))))

(declaim (inline water-content-range))
(defun water-content-range (model index)
  (with-slots (unsaturated-models) model
    (declare (type (simple-array unsaturated *) unsaturated-models))
    (- (the double-float
            (saturated-water-content (aref unsaturated-models index)))
       (the double-float
            (residual-water-content (aref unsaturated-models index))))))

;; *** Richard's equation
(defun richards-equation (model)
  "Produces RHS of Richards equation for saturation for MODEL"
  (declare (optimize (speed 3)) (type darcy model))
  (let ((size (darcy-size model))
        (space-step (space-step model)))
    (declare (type fixnum size) (type double-float space-step))
    (let ((pressure (map 'vector #'pressure
                         (the simple-array (unsaturated-models model))))
          (relcond (map 'vector #'relative-conductivity
                        (the simple-array (unsaturated-models model))))
          (conductivities (conductivities model)))
      (declare (type (simple-array function *) pressure relcond)
               (type (simple-array conductivity *) conductivities))
      (flet ((conductivity-at (s n)
               (declare (type (simple-array double-float *) s)
                        (type fixnum n))
               (* (the double-float (funcall (aref relcond n) (aref s n)))
                  ;; (the double-float (funcall (are relcond n)
                  ;;                            (* (+ (aref s n) (aref s (1+ n)))
                  ;;                               0.5d0)))
                  (the double-float
                       (saturated-conductivity (aref conductivities n)))))
             (pressure-at (s n)
               (funcall (aref pressure n) (aref s n))))
        (declare (inline conductivity-at pressure-at))
        (lambda (time saturation ds)
          (declare (type double-float time)
                   (type (simple-array double-float) saturation)
                   (type (vector double-float) ds)
                   (optimize (speed 3) (safety 0)))
          (let ((pressure-prev (pressure-at saturation 0))
                (pressure-next 0d0)
                (flux-prev (darcy-inlet-discharge model time))
                (flux-next 0d0)
                (conductivity (conductivity-at saturation 0)))
            (declare (type double-float
                           pressure-prev pressure-next flux-prev
                           flux-next conductivity))
            (dotimes (index (1- size))
              ;; (declare (type fixnum index))
              (setf pressure-next (pressure-at saturation (1+ index)))
              (setf flux-next (darcy-flux conductivity
                                          pressure-next
                                          pressure-prev
                                          space-step))
              (setf (aref ds index)
                    (- (* (/ (water-content-range model index))
                          (/ (- flux-next flux-prev) space-step))))
              (setf pressure-prev pressure-next)
              (setf flux-prev flux-next)
              (setf conductivity (conductivity-at saturation (1+ index))))
            (setf flux-next conductivity)
            (setf (aref ds (1- size))
                  (- (* (/ (water-content-range model (1- size)))
                        (/ (- flux-next flux-prev) space-step))))))))))

;; *** Main interface: saturation evolution over time
(defun darcy-evolve (model init-saturation final-time fine-time-step output-time-step
                     &key suppress-output)
  "Produces time-dependent saturation evolution of the MODEL"
  (let ((saturations (list (cons 0d0 (copy-seq init-saturation))))
        (inlet nil)
        (outlet nil)
        (last-time-output 0d0))
    (flet ((collect-output (time y)
             (unless suppress-output
               (vom:info "time = ~F complete~%" time))
             (push (cons time (darcy-inlet-discharge model time)) inlet)
             (push (cons time (darcy-outlet-discharge model y)) outlet)
             (when (>= (- time last-time-output) output-time-step)
               (push (cons time (copy-seq y)) saturations)
               (setf last-time-output time))))
      (lsoda-evolve (richards-equation model)
                    0.0d0
                    init-saturation
                    (loop for time from (coerce fine-time-step 'double-float)
                       below (+ final-time (* 0.1d0 fine-time-step))
                       by (coerce fine-time-step 'double-float)
                       collect time)
                    #'collect-output)
      (list (nreverse inlet) (nreverse saturations) (nreverse outlet)))))
