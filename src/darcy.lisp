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

(defmethod darcy-depth ((model darcy))
  "Depth (or height) of the packing"
  (with-slots ((dz space-step)) model
    (* dz (darcy-size model))))

;; *** Utility operating functions
(defun darcy-flux! (flux-vector pressure conductivity space-step)
  "Fills in FLUX-VECTOR using K(psi) * (partial_z(psi) - 1)"
  (declare (optimize (speed 3))
           (type (simple-array double-float *) flux-vector pressure conductivity)
           (type double-float space-step))
  (let ((size (length pressure)))
    (flet ((flux (k h-prev h-next)
             (declare (type double-float k h-prev h-next))
             (- (* k (- (/ (- h-next h-prev) space-step) 1d0)))))
      (loop for i from 1 below size
         do (setf (aref flux-vector i)
                  (flux (aref conductivity (1- i))
                        (aref pressure (1- i))
                        (aref pressure i)))))))

;; *** Richard's equation
(defun richards-equation (model)
  "Produces RHS of Richards equation for saturation for MODEL"
  (declare (optimize (speed 3))
           (type darcy model))
  (let* ((size (darcy-size model))
         (flux (make-array (1+ (the fixnum size)) :element-type 'double-float))
         (full-conductivity (make-array size :element-type 'double-float))
         (pressure (make-array size :element-type 'double-float)))
    (declare (type (simple-array double-float) flux full-conductivity pressure)
             (type fixnum size))
    (with-slots ((unsaturated unsaturated-models)
                 space-step
                 (conductivity conductivities)) model
      (declare (type double-float space-step))
      (declare (type simple-array unsaturated conductivity))
      (lambda (time s ds)
        (declare (type double-float time)
                 (type (simple-array double-float) s)
                 (type (vector double-float) ds))
        ;; fill in FULL-CONDUCTIVITY & PRESSURE vectors
        (do ((i 0 (1+ i)))
            ((>= i size))
          (declare (type fixnum i))
          (setf (aref full-conductivity i)
                (* (the double-float (saturated-conductivity
                                      (aref conductivity i)))
                   (the
                    double-float
                    (relative-conductivity (aref unsaturated i)
                             (aref s i)))))
          (setf (aref pressure i)
                (pressure (aref unsaturated i) (aref s i))))
        (darcy-flux! flux pressure full-conductivity space-step)
        (setf (aref flux 0)
              (funcall (the function (inlet-discharge model)) time))
        (setf (aref flux size)
              (aref full-conductivity (1- (the fixnum size))))
        (loop for i from 0 below size
           do (setf (aref ds i)
                    (- (* (/ (- (the double-float (saturated-water-content
                                                   (aref unsaturated i)))
                                (the double-float (residual-water-content
                                                   (aref unsaturated i)))))
                          (/ (the double-float (- (aref flux (1+ i))
                                                  (aref flux i)))
                             space-step)))))))))

;; *** Main interface: saturation evolution over time
(defun darcy-evolve (model init-saturation final-time output-time-step
                     &key suppress-output)
  "Produces time-dependent saturation evolution of the MODEL"
  (let ((output (list (cons 0d0 (copy-seq init-saturation)))))
    (flet ((collect-output (time y)
             (unless suppress-output
               (format t "~&time = ~F complete~%" time))
             (push (cons time (copy-seq y)) output)))
      (lsoda-evolve (richards-equation model)
                    0.0d0
                    init-saturation
                    (loop for time from (coerce output-time-step 'double-float)
                       below (+ final-time (* 0.1d0 output-time-step))
                       by (coerce output-time-step 'double-float)
                       collect time)
                    #'collect-output)
      (nreverse output))))

