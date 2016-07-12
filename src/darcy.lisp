(in-package darcy-model)

;; (defgeneric full-conductivity (model effsat)
;;   (declare (optimize (speed 3)))
;;   (:documentation "Full conductivity of the model dependent on effective saturation"))

(defclass constant-inlet-discharge ()
  ((inlet-flow-rate
    :initarg :inlet-flow-rate
    :initform (/ 10d0 1000d0 3600d0)
    :accessor inlet-flow-rate
    :documentation "Flow rate in m/s"))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((obj constant-inlet-discharge) &key)
  (closer-mop:set-funcallable-instance-function
   obj
   #'(lambda (x)
       (declare (ignore x)
                (optimize (speed 3)))
       (inlet-flow-rate obj))))

(defclass fluctuating-inlet-discharge (constant-inlet-discharge)
  ((fluctuation-frequency
    :initarg :fluctuation-frequency
    :accessor :fluctuation-frequency
    :documentation "Frequency in 1/s")
   (fluctuation-delay
    :initarg :fluctuation-delay
    :accessor fluctuation-delay
    :documentation "Delay in s"))
  (:metaclass closer-mop:funcallable-standard-class))


(defmethod initialize-instance :after ((obj fluctuating-inlet-discharge) &key)
  (closer-mop:set-funcallable-instance-function
   obj
   #'(lambda (time)
       (declare (optimize (speed 3))
                (type double-float time))
       (with-slots ((m inlet-flow-rate)
                    (f fluctuation-frequency)
                    (d fluctuation-delay)) obj
         (declare (type double-float m f d))
         (* m (sin (* 2d0 pi f (- time d))))))))

(defclass noisy-inlet-discharge ()
  ((inlet-discharge-noise
    :initarg :inlet-discharge-noise
    :accessor inlet-discharge-noise
    :type (double-float 0d0 1d0)
    :documentation "White noise for the discharge")
   (base-inlet-discharge
    :initarg :base-inlet-discharge
    :accessor base-inlet-discharge
    :documentation "Base inlet discharge for which the noise will be added"))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((obj noisy-inlet-discharge) &key)
  (closer-mop:set-funcallable-instance-function
   obj
   #'(lambda (time)
       (with-slots ((noise inlet-discharge-noise)
                    (base base-inlet-discharge)) obj
         (* (funcall base time) (+ 1d0 (random (* 2d0 noise)) (- noise)))))))

(defclass darcy ()
  ((space-step
    :initarg :space-step
    :initform 0.05
    :accessor space-step
    :documentation "Spatial step size (m)")
   (conductivities
    :initarg :conductivities
    :accessor conductivities
    :initform nil
    :documentation "Saturated conductivities part of Darcy's model")
   (unsaturated-models
    :initarg :unsaturated-models
    :initform nil
    :accessor unsaturated-models
    :documentation "Unsaturated (sub-)models of Darcy's model")
   (inlet-discharge
    :initarg :inlet-discharge
    :initform (make-instance 'constant-inlet-discharge  :inlet-flow-rate (/ 10d0 1000d0 3600d0))
    :accessor inlet-discharge
    :documentation "Inlet specific discharge (m/s)")))

(defmethod print-object ((obj darcy) out)
  (with-slots (space-step conductivities unsaturated-models inlet-discharge) obj
    (print-unreadable-object (obj out :type t)
      (format out "~@<~:_dz = ~A ~:_conductivity = ~A ~:_unsaturated = ~A ~:_inlet discharge = ~A ~:>"
              space-step conductivities unsaturated-models inlet-discharge))))

;; it's impossible to use in this form
;; (defmethod full-conductivity ((model darcy) (effsat double-float))
;;   (* (relative-conductivity (darcy-unsaturated model) effsat)
;;      (saturated-conductivity (darcy-conductivity model))))

(defun conductivity (model saturation index)
  "Gets the full conductivity at INDEX of the DARCY MODEL for SATURATION"
  (declare (type darcy model)
           (type (simple-array (double-float 0d0 1d0) *) saturation)
           (type fixnum index))
  (* (relative-conductivity (aref (unsaturated-models model) index)
                            (aref saturation index))
     (saturated-conductivity (aref (conductivities model) index))))


(defun darcy-flux! (flux-vector pressure conductivity space-step)
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

(defmethod darcy-size ((model darcy))
  (declare (optimize (speed 3)))
  (length (the simple-array (unsaturated-models model))))

(defmethod darcy-points ((model darcy))
  (with-slots ((dz space-step)) model
    (loop for i from 0 below (darcy-size model)
         collect (+ (/ dz 2) (* dz i)))))

(defmethod darcy-depth ((model darcy))
  (with-slots ((dz space-step)) model
    (* dz (darcy-size model))))

(defun fill-array! (function array &rest arrays)
  (declare (optimize (speed 3))
           (type function function)
           (type (simple-array double-float) array))
  (let ((n (length arrays)))
    (case n
      ((0)
       (loop for i from 0 below (length array)
           do (setf (aref array i)
                    (funcall function (aref array i)))))
      ((2)
       (destructuring-bind (ar1 ar2) arrays
         (declare (type simple-array ar1 ar2))
         (loop for i from 0 below (length array)
            do (setf (aref array i)
                     (funcall function (aref ar1 i) (aref ar2 i))))))
      ((3)
       (destructuring-bind (ar1 ar2 ar3) arrays
         (declare (type simple-array ar1 ar2 ar3))
         (loop for i from 0 below (length array)
            do (setf (aref array i)
                     (funcall function (aref ar1 i) (aref ar2 i) (aref ar3 i))))))
      (otherwise
       (loop for i from 0 below (length array)
          do (setf (aref array i)
                   (apply function
                          (mapcar (lambda (ar)
                                    (declare (type simple-array ar))
                                    (aref ar i))
                                  arrays))))))))


(defun richards-equation (model)
  "Produces RHS of Richards equation for saturation for MODEL"
  (declare (optimize (speed 3))
           (type darcy model))
  (let* ((size (darcy-size model))
         (flux (make-array (1+ (the fixnum size)) :element-type 'double-float))
         (conductivity (make-array size :element-type 'double-float))
         (pressure (make-array size :element-type 'double-float)))
    (declare (type (simple-array double-float) flux conductivity pressure))
    (with-slots ((unsaturated unsaturated-models)
                 space-step
                 conductivities
                 inlet-discharge) model
      (declare (type double-float space-step))
      (declare (type simple-array unsaturated))
      (lambda (time s ds)
        (declare (type double-float time)
                 (type (simple-array double-float) s)
                 (type (vector double-float) ds))
        (fill-array! (lambda (c u s)
                       (* (the double-float (saturated-conductivity c))
                          (the double-float (relative-conductivity u s))))
                     conductivity
                     conductivities
                     unsaturated
                     s)
        (fill-array! #'pressure
                     pressure
                     unsaturated
                     s)
        (darcy-flux! flux pressure conductivity space-step)
        (setf (aref flux 0) (funcall (the function inlet-discharge) time))
        (setf (aref flux size) (aref conductivity (1- (the fixnum size))))
        (loop for i from 0 below size
           do (setf (aref ds i)
                    (- (* (/ (- (the double-float (saturated-water-content (aref unsaturated i)))
                                (the double-float (residual-water-content (aref unsaturated i)))))
                          (/ (the double-float (- (aref flux (1+ i)) (aref flux i)))
                             space-step)))))))))

(defun darcy-evolve (model init-saturation final-time output-time-step &key suppress-output)
  "Produces time-evolved saturation evolution of the MODEL"
  (let ((output nil))
    (flet ((collect-output (time y)
             (unless suppress-output
               (format t "~&time = ~F complete~%" time))
             (push (cons time (copy-seq y)) output)))
      (lsoda-evolve (richards-equation model)
                    0.0d0
                    init-saturation
                    (loop for time from output-time-step
                       below (+ final-time (* 0.1 output-time-step))
                       by output-time-step
                       collect time)
                    #'collect-output)
      (nreverse output))))

(defun uniform-darcy-model (&key conductivity unsaturated
                              (height 1d0) (mesh-points-number 20)
                              (inlet-discharge (make-instance 'constant-inlet-discharge
                                                 :inlet-flow-rate (/ 10d0 1000 3600))))
  (let ((dz (/ height mesh-points-number))
        (c (apply #'make-instance conductivity))
        (u (apply #'make-instance unsaturated)))
    (make-instance 'darcy
      :inlet-discharge inlet-discharge
      :unsaturated-models (fill-array u mesh-points-number)
      :conductivities (fill-array c mesh-points-number)
      :space-step dz)))


(defun perturb-plist (plist perturbations)
  (apply #'nconc
   (loop for (k v . rest) on plist by #'cddr
      if (getf perturbations k)
      collect (list k (* v (+ 1 (* 2 (getf perturbations k) (random 1d0)) (- (getf perturbations k)))))
      else
      collect (list k v))))

(defun perturbed-darcy-model (&key conductivity unsaturated
                                (height 1d0) (mesh-points-number 20)
                                (inlet-discharge (make-instance 'constant-inlet-discharge
                                                   :inlet-flow-rate (/ 10d0 1000 3600)))
                                perturbations)
  "Constructs perturbed Darcy's model according perturbations. Perturbations is a plist of the form
    (:PARAMETER PERTURBATION ...)
The value for the :PARAMETER (in either conductivity or unsaturated model) will be constructed as
    VALUE +/- PERTURBATION * 100%
where VALUE is the value from the argument list"
  (let ((dz (/ height mesh-points-number))
        (c (init-array (lambda (ignored)
                         (declare (ignore ignored))
                         (let ((arg-list (cons (first conductivity)
                                               (perturb-plist (rest conductivity) perturbations))))
                           (apply #'make-instance arg-list)))
                       mesh-points-number))
        (u (init-array (lambda (ignored)
                         (declare (ignore ignored))
                         (apply #'make-instance
                                (cons (first unsaturated)
                                      (perturb-plist (rest unsaturated) perturbations))))
                       mesh-points-number)))
    (make-instance 'darcy
      :inlet-discharge inlet-discharge
      :unsaturated-models u
      :conductivities c
      :space-step dz)))

