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
   #:water-content
   #:mualem #:maulem-exponent
   #:van-genuchten #:van-genuchten-n #:van-genuchten-m
   #:brooks-corey-mualem  #:pore-size-distribution-index))

(defpackage :conductivity
  (:use #:cl)
  (:export #:conductivity #:liquid-viscosity
           #:intrinsic-permeability #:saturated-conductivity))

(defpackage :inlet-discharge
  (:use #:cl)
  (:export
   #:constant-inlet-discharge #:inlet-flow-rate
   #:fluctuating-inlet-discharge #:fluctuation-frequency #:fluctuation-delay
   #:noisy-inlet-discharge #:inlet-discharge-noise
   #:intermittent-inlet-discharge #:intermittent-periods-flow-rates))

(defpackage :darcy-model
  (:use #:cl #:unsaturated #:conductivity #:darcy-utils #:inlet-discharge #:cl-lsoda
        #:cl-slice)
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

(defpackage :darcy-use
  (:use #:cl #:unsaturated #:conductivity #:inlet-discharge
        #:darcy-model #:darcy-utils #:alexandria
        #:cl-slice)
  (:export ;; re-export utils - useful
   #:fill-array #:init-array)
  (:export ;; unsaturated
   #:saturation #:pressure #:relative-conductivity
   #:unsaturated #:saturated-water-content #:residual-water-content #:bubbling-pressure
   #:unsaturated-alpha
   #:water-content
   #:mualem #:maulem-exponent
   #:van-genuchten #:van-genuchten-n #:van-genuchten-m
   #:brooks-corey-mualem  #:pore-size-distribution-index)
  (:export ;; conductivity
   #:conductivity #:liquid-viscosity
   #:intrinsic-permeability #:saturated-conductivity)
  (:export ;; inlet discharge
   #:constant-inlet-discharge #:inlet-flow-rate
   #:fluctuating-inlet-discharge #:fluctuation-frequency #:fluctuation-delay
   #:noisy-inlet-discharge #:inlet-discharge-noise
   #:intermittent-inlet-discharge #:intermittent-periods-flow-rates)
  (:export ;; darcy model
   #:darcy #:space-step #:conductivities #:unsaturated-models #:inlet-discharge
   #:darcy-size #:darcy-points #:darcy-boundaries #:darcy-depth
   #:darcy-water-volume
   #:pressure-at #:conductivity-at
   #:full-conductivity
   #:richards-equation
   #:darcy-evolve)
  (:export ;; simulate
   #:simulation-results
   #:simulation-results-time
   #:simulation-results-mesh-points
   #:simulation-results-mesh-boundaries
   #:simulation-results-saturation
   #:simulation-results-discharge-time
   #:simulation-results-inlet-discharge
   #:simulation-results-outlet-discharge
   #:column-average-saturation
   #:consumed-water
   #:results-water-volume
   #:abs-water-balance-error
   #:water-balance-error
   #:darcy-simulation
   #:darcy-simulation-model
   #:darcy-simulation-results
   #:simulate))

