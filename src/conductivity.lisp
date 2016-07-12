(in-package conductivity)

(defclass conductivity ()
  ((liquid-viscosity
    :initarg :liquid-viscosity
    :initform 1d-6
    :accessor liquid-viscosity
    :documentation
    "Liquid kinematic viscosity (dynamic viscosity / density), m2/s")
   (intrinsic-permeability
    :initarg :intrinsic-permeability
    :initform 1d-8
    :accessor intrinsic-permeability
    :documentation
    "Intrinsic permeability of the medium, independent of liquid properties, m2"))
  (:documentation
   "Saturated conductivity"))

(defmethod print-object ((obj conductivity) out)
  (with-slots (liquid-viscosity intrinsic-permeability) obj
    (print-unreadable-object (obj out :type t)
      (format out "~@<~:_liq kinem viscosity (nu) = ~A m2/s ~:_intrinsic permeability = ~A m2~:>"
              liquid-viscosity intrinsic-permeability))))

(defgeneric saturated-conductivity (conductivity)
  (:documentation "Saturated conductivity of the medium")
  (declare (optimize (speed 3))))

(defmethod saturated-conductivity ((c conductivity))
  (declare (optimize (speed 3)))
  (with-slots ((nu liquid-viscosity) (k intrinsic-permeability)) c
    (declare (type (double-float 0d0) nu k))
    (* 9.81d0 (/ nu) k)))
