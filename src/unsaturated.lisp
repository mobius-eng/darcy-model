(in-package cl-user)

(defpackage :unsaturated
  (:use #:cl)
  (:export
   #:saturation #:pressure #:relative-conductivity
   #:unsaturated #:saturated-water-content #:residual-water-content #:bubbling-pressure
   #:unsaturated-alpha
   #:water-content
   #:mualem #:maulem-exponent
   #:van-genuchten #:van-genuchten-n #:van-genuchten-m
   #:brooks-corey-mualem  #:pore-size-distribution-index))

(in-package unsaturated)

;; * Unsaturated models
(declaim (optimize (speed 3) (safety 0)))

;; ** Unsaturated interface
(defgeneric saturation (model)
  (:documentation
   "Effective saturation of unsaturated model from capillary pressure"))

(defgeneric pressure (model)
  (:documentation
   "Capillary pressure of unsaturated model from effective saturation"))

(defgeneric relative-conductivity (model)
  (:documentation
   "Relative conductivity of unsaturated model from effective saturation")
  (declare (optimize (speed 3))))

;; ** Unsaturated model representation
;; *** Root abstrat class
(defclass unsaturated ()
  ((saturated-water-content
    :initarg :saturated-water-content
    :initform 0.3d0
    :type double-float
    :accessor saturated-water-content
    :documentation "Maximum saturation of the bed as V[liquid] / V[total]")
   (residual-water-content
    :initarg :residual-water-content
    :initform 0.05d0
    :type double-float
    :accessor residual-water-content
    :documentation "Residual saturation of the bed as V[liquid] / V[total]")
   (bubbling-pressure
    :initarg :bubbling-pressure
    :initform 0.333333d0
    :accessor bubbling-pressure
    :type bubbling-pressure
    :documentation "Bubbling pressure (m)"))
  (:documentation
   "Root class for unsaturated models"))

(defgeneric unsaturated-alpha (model)
  (declare (optimize (speed 3)))
  (:documentation "Inverse of the bubbling pressure (1/m)")
  (:method ((model unsaturated))
    (declare (optimize (speed 3)))
    (with-slots (bubbling-pressure) model
      (declare (type (double-float 0d0 *) bubbling-pressure))
      (abs (/ bubbling-pressure)))))

(defgeneric water-content (model saturation)
  (:method ((model unsaturated) saturation)
    (declare (type (double-float 0d0 1d0) saturation))
    (with-slots ((qs saturated-water-content)
                 (qr residual-water-content)) model
      (declare (type double-float qs qr))
      (+ qr (* (- qs qr) saturation)))))

;; *** Mualem theory model
(defclass mualem (unsaturated)
  ((mualem-exponent
    :initarg :mualem-exponent
    :initform 0.5d0
    :accessor mualem-exponent
    :documentation "Mualem-theory parameter (exponent)"))
  (:documentation
   "Root class for all Mualem-theory models"))

;; *** Van Genuchten model
(defclass van-genuchten (mualem)
  ((n :initarg :n
      :initform 1.5d0
      :accessor van-genuchten-n
      :documentation "Fitting parameter")
   (m :reader van-genuchten-m
      :documentation "Fitting parameter, always m = 1 - 1/n"))
  (:documentation
   "Van Genuchten unsaturated model"))

(defmethod initialize-instance :after ((obj van-genuchten) &key)
  (with-slots (n m) obj
    (setf n (coerce n 'double-float))
    (when (slot-boundp obj 'n) (setf m (- 1d0 (/ n))))))

(defmethod print-object ((obj van-genuchten) out)
  (with-slots (bubbling-pressure
               saturated-water-content
               residual-water-content
               mualem-exponent
               n m) obj
    (print-unreadable-object (obj out :type t)
      (format
       out
       "~@<~:_wc_sat = ~A wc_res = ~A ~:_Pb = ~A m (alpha = ~A 1/m) ~:_L = ~A ~:_n = ~A (m = ~A)~:>"
       saturated-water-content
       residual-water-content
       bubbling-pressure (unsaturated-alpha obj)
       mualem-exponent
       n m))))

(defmethod saturation ((model van-genuchten))
  (let ((n (van-genuchten-n model))
        (m (van-genuchten-m model))
        (bubbling-pressure (bubbling-pressure model)))
    (declare (type (double-float 0d0 *) n bubbling-pressure m))
    (lambda (pressure)
      (declare (optimize (speed 3)) (type (double-float * 0d0) pressure))
      (let ((arg (expt (/ (abs pressure) bubbling-pressure) n)))
        (declare (type (double-float 0d0) arg))
        (expt (1+ arg) (- m))))))

(defmethod pressure ((model van-genuchten))
  (let ((bubbling-pressure (bubbling-pressure model))
        (n (van-genuchten-n model))
        (m (van-genuchten-m model)))
    (declare (type (double-float 0d0 *) bubbling-pressure n m))
    (lambda (effsat)
      (declare (optimize (speed 3)) (type (double-float * *) effsat))
      (when (minusp effsat) (setf effsat 1d-6))
      (if (> effsat 1d0)
          0d0
          (let ((arg (1- (expt (the (double-float 0d0 1d0) effsat) (- (/ m))))))
            (declare (type (double-float 0d0 1d0) arg))
            (- (* (expt arg (/ n)) bubbling-pressure)))))))

(declaim (inline square))
(defun square (x)
  (declare (type double-float x))
  (* x x))

(defmethod relative-conductivity ((model van-genuchten))
  (let ((m (van-genuchten-m model))
        (l (mualem-exponent model)))
    (declare (type (double-float 0d0) m l))
    (lambda (effsat)
      (declare (optimize (speed 3)) (type double-float effsat))
      (when (< effsat 0d0) (setf effsat 0d0))
      (if (> effsat 1d0)
          1d0
          (let ((arg (- 1d0 (expt (the (double-float 0d0 1d0) effsat) (/ m)))))
            (declare (type (double-float 0d0 1d0) arg))
            (* (expt effsat l) (square (- 1d0 (expt arg m)))))))))

;; *** Brooks-Corey model
(defclass brooks-corey-mualem (mualem)
  ((pore-size-distribution-index
    :initarg :pore-size-distribution-index
    :initform 2d0
    :accessor pore-size-distribution-index))
  (:documentation
   "Brooks-Corey-Mualem unsaturated model"))

(defmethod initialize-instance :after ((obj brooks-corey-mualem) &key)
  (with-slots (pore-size-distribution-index) obj
    (setf pore-size-distribution-index
          (coerce pore-size-distribution-index 'double-float))))

(defmethod print-object ((obj brooks-corey-mualem) out)
  (with-slots ((L mualem-exponent)
               (lambda pore-size-distribution-index)
               (Pb bubbling-pressure)
               saturated-water-content
               residual-water-content) obj
    (print-unreadable-object (obj out :type t)
      (format
       out
       "~@<~:_wc_sat = ~A wc_res = ~A ~:_Pb = ~A m (alpha = ~A 1/m) ~:_L = ~A ~:_lambda = ~A~:>"
       saturated-water-content
       residual-water-content
       Pb (unsaturated-alpha obj)
       L
       lambda))))

(defmethod saturation ((model brooks-corey-mualem))
  (let ((lambda (pore-size-distribution-index model))
        (alpha (unsaturated-alpha model)))
    (declare (type (double-float 0d0 *) alpha lambda))
    (lambda (pressure)
      (declare (optimize (speed 3)) (type double-float pressure))
      (if (< pressure (- (/ alpha)))
          (expt (* alpha (abs pressure)) (- lambda))
          1d0))))

(defmethod pressure ((model brooks-corey-mualem))
  (let ((lambda (pore-size-distribution-index model))
        (alpha (unsaturated-alpha model)))
    (declare (type (double-float 0d0 *) alpha lambda))
    (lambda (effsat)
      (declare (optimize (speed 3)) (type (double-float 0d0 1d0) effsat))
      (- (/ (expt effsat (- (/ lambda))) alpha)))))

(defmethod relative-conductivity ((model brooks-corey-mualem))
  (let ((l (mualem-exponent model))
        (lambda (pore-size-distribution-index model)))
    (declare (type double-float l lambda))
    (lambda (effsat)
      (declare (optimize (speed 3)) (type (double-float 0d0 1d0) effsat))
      (expt effsat (+ 2d0 l (/ 2d0 lambda))))))
