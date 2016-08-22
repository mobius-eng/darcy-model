(in-package cl-user)

(defpackage :darcy-example
  (:use #:cl #:darcy-use))

(in-package darcy-example)

(defvar *zmax* 1.0d0)
(defvar *n* 20)

(defvar *m*
  (uniform-darcy-model
   :conductivity (update-configuration +default-conductivity+)
   :unsaturated (update-configuration +default-van-genuchten+)
   :mesh-points-number *n*
   :height *zmax*
   :inlet-discharge (update-configuration +default-constant-inlet-discharge+)))

(defvar *result*
  (time (darcy-evolve *m* (fill-array 0.001d0 *n* 'double-float) (* 6 3600d0) (/ 3600d0 6)
                      :suppress-output t)))
