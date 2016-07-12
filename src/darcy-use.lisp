(in-package cl-user)

(defpackage :darcy-use
  (:use #:cl #:darcy))

(in-package darcy-use)

(defvar *zmax* 6.0d0)
(defvar *n* 50)

;; (defvar *c* (fill-array (make-instance 'conductivity :intrinsic-permeability 1d-8) *n*))

;; (defvar *u* (fill-array
;;              (make-instance 'van-genuchten
;;                :alpha 3d0
;;                :n 4d0
;;                :l 0.5d0
;;                :max-saturation 0.3d0
;;                :res-saturation 0.1d0)
;;              *n*))

;; (defvar *m*
;;   (make-instance 'darcy :unsaturated *u* :conductivity *c* :space-step (/ *zmax* *n*)))

(defvar *m*
  (perturbed-darcy-model
   :conductivity '(conductivity :intrinsic-permeability 1d-8)
   :unsaturated '(van-genuchten
                  :bubbling-pressure 0.333d0
                  :n 4d0
                  :mualem-exponent 0.5d0
                  :saturated-water-content 0.3d0
                  :residual-water-content 0.1d0)
   :mesh-points-number *n*
   :height *zmax*
   :perturbations '(:bubbling-pressure 0.1d0
                    :intrinsic-permeability 0.2d0)))

(defvar *result*
  (time (darcy-evolve *m* (fill-array 0.001d0 *n* 'double-float) (* 6 3600d0) (/ 3600d0 6))))

(pressure (aref (unsaturated-models *m*) 0) 0.1d0)
