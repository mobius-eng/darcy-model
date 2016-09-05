(in-package darcy-model)
;; * Darcy model

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
(defmethod pressure ((model darcy) effsat)
  (with-slots (unsaturated-models) model
    (apply #'vector
           (loop for u across unsaturated-models
              for s across effsat
              collect (pressure u s)))))

(defmethod relative-conductivity ((model darcy) effsat)
  (with-slots (unsaturated-models) model
    (apply #'vector
           (loop for u across unsaturated-models
              for s across effsat
                collect (relative-conductivity u s)))))

(defmethod full-conductivity ((model darcy) effsat)
  (with-slots (unsaturated-models conductivities) model
    (apply #'vector
           (loop for u across unsaturated-models
              for c across conductivities
              for s across effsat
              collect (* (relative-conductivity u effsat)
                         (saturated-conductivity c))))))

(declaim (inline conductivity-at))
(defun conductivity-at (model effsat index)
  (with-slots (unsaturated-models conductivities) model
    (declare (type (simple-array unsaturated *) unsaturated-models)
             (type (simple-array conductivity *) conductivities)
             (type (simple-array double-float *) effsat))
    (* (the double-float
            (relative-conductivity (aref unsaturated-models index)
                                   (aref effsat index)))
       (the double-float
            (saturated-conductivity (aref conductivities index))))))

(declaim (inline pressure-at))
(defun pressure-at (model effsat index)
  (with-slots (unsaturated-models) model
    (declare (type (simple-array unsaturated *) unsaturated-models)
             (type (simple-array double-float *) effsat))
    (pressure (aref unsaturated-models index)
              (aref effsat index))))

(declaim (inline darcy-inlet-discharge))
(defun darcy-inlet-discharge (model time)
  (funcall (the function (inlet-discharge model)) time))

(declaim (inline darcy-outlet-discharge))
(defun darcy-outlet-discharge (model saturation)
  (conductivity-at model saturation (1- (length saturation))))

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
    (lambda (time saturation ds)
      (declare (type double-float time)
               (type (simple-array double-float) saturation)
               (type (vector double-float) ds))
      (let ((pressure-prev (pressure-at model saturation 0))
            (pressure-next 0d0)
            (flux-prev (darcy-inlet-discharge model time))
            (flux-next 0d0)
            (conductivity (conductivity-at model saturation 0)))
        (declare (type double-float
                       pressure-prev pressure-next flux-prev
                       flux-next conductivity))
        (dotimes (index (1- size))
          ;; (declare (type fixnum index))
          (setf pressure-next (pressure-at model saturation (1+ index)))
          (setf flux-next (darcy-flux conductivity
                                      pressure-next
                                      pressure-prev
                                      space-step))
          (setf (aref ds index)
                (- (* (/ (water-content-range model index))
                      (/ (- flux-next flux-prev) space-step))))
          (setf pressure-prev pressure-next)
          (setf flux-prev flux-next)
          (setf conductivity (conductivity-at model saturation (1+ index))))
        (setf flux-next conductivity)
        (setf (aref ds (1- size))
              (- (* (/ (water-content-range model (1- size)))
                    (/ (- flux-next flux-prev) space-step))))))))

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
               (format t "~&time = ~F complete~%" time))
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
