(in-package cl-user)

(defpackage darcy-utils
  (:use #:cl)
  (:export #:fill-array #:init-array))

(defpackage :unsaturated
  (:use #:cl)
  (:export
   #:saturation #:pressure #:relative-conductivity
   #:unsaturated #:saturated-water-content #:residual-water-content #:bubbling-pressure
   #:unsaturated-alpha
   #:mualem #:maulem-exponent
   #:van-genuchten #:van-genuchten-n #:van-genuchten-m
   #:brooks-corey-mualem  #:pore-size-distribution-index))

(defpackage :conductivity
  (:use #:cl)
  (:export #:conductivity #:liquid-viscosity #:intrinsic-permeability #:saturated-conductivity))

(defpackage :darcy-model
  (:use #:cl #:unsaturated #:conductivity #:darcy-utils #:alexandria #:cl-lsoda)
  (:export
   #:constant-inlet-discharge #:inlet-flow-rate
   #:fluctuating-inlet-discharge #:fluctuation-frequency #:fluctuation-delay
   #:noisy-inlet-discharge #:inlet-discharge-noise
   #:darcy #:space-step #:conductivities #:unsaturated-models #:inlet-discharge
   #:darcy-size #:darcy-points #:darcy-depth
   #:richards-equation
   #:darcy-evolve
   #:uniform-darcy-model
   #:perturbed-darcy-model))

;; Just re-export all individual packages via one
(defpackage :darcy
  (:use #:cl #:unsaturated #:conductivity #:darcy-model #:darcy-utils)
  (:export
   ;; darcy-utils
   #:fill-array #:init-array
   ;; unsaturated
   #:saturation #:pressure #:relative-conductivity
   #:unsaturated #:saturated-water-content #:residual-water-content #:bubbling-pressure
   #:unsaturated-alpha
   #:mualem #:maulem-exponent
   #:van-genuchten #:van-genuchten-n #:van-genuchten-m
   #:brooks-corey-mualem  #:pore-size-distribution-index
   ;; conductivity
   #:conductivity #:liquid-viscosity #:intrinsic-permeability #:saturated-conductivity
   ;; darcy-model
   #:constant-inlet-discharge #:inlet-flow-rate
   #:fluctuating-inlet-discharge #:fluctuation-frequency #:fluctuation-delay
   #:noisy-inlet-discharge #:inlet-discharge-noise
   #:darcy #:space-step #:conductivities #:unsaturated-models #:inlet-discharge
   #:darcy-size #:darcy-points #:darcy-depth
   #:richards-equation
   #:darcy-evolve
   #:uniform-darcy-model
   #:perturbed-darcy-model))
