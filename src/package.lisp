(in-package cl-user)

(defpackage darcy-utils
  (:use #:cl)
  (:export #:fill-array #:init-array
           #:instantiate
           #:configuration #:configuration-class-name #:configuration-list
           #:enable-configuration-syntax
           #:update-configuration))

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
  (:use #:cl #:unsaturated #:conductivity #:darcy-utils #:inlet-discharge #:cl-lsoda)
  (:export
   #:darcy #:space-step #:conductivities #:unsaturated-models #:inlet-discharge
   #:darcy-size #:darcy-points #:darcy-depth
   #:richards-equation
   #:darcy-evolve))

(defpackage :darcy-use
  (:use #:cl #:unsaturated #:conductivity #:inlet-discharge #:darcy-model #:darcy-utils #:alexandria)
  (:export
   ;; #:+default-conductivity+
   ;; #:+default-van-genuchten+ #:+default-brooks-corey+
   ;; #:+default-constant-inlet-discharge+
   ;; #:+default-fluctuating-inlet-discharge+
   ;; #:+default-noisy-inlet-discharge+
   ;; #:+default-intermittent-inlet-discharge+
   ;; #:general-darcy-model
   ;; #:uniform-darcy-model
   ;; #:perturbed-darcy-model
   ;; #:perturb-configuration
   ;; Re-export everything else: ease of use:
   ;; darcy-utils
   #:fill-array #:init-array
   #:instantiate
   #:configuration #:configuration-class-name #:configuration-list
   #:enable-configuration-syntax
   #:update-configuration
   ;; unsaturated
      #:saturation #:pressure #:relative-conductivity
   #:unsaturated #:saturated-water-content #:residual-water-content #:bubbling-pressure
   #:unsaturated-alpha
   #:mualem #:maulem-exponent
   #:van-genuchten #:van-genuchten-n #:van-genuchten-m
   #:brooks-corey-mualem  #:pore-size-distribution-index
   ;; conductivity
   #:conductivity #:liquid-viscosity
   #:intrinsic-permeability #:saturated-conductivity
   ;; inlet discharge
   #:constant-inlet-discharge #:inlet-flow-rate
   #:fluctuating-inlet-discharge #:fluctuation-frequency #:fluctuation-delay
   #:noisy-inlet-discharge #:inlet-discharge-noise
   #:intermittent-inlet-discharge #:intermittent-periods-flow-rates
   ;; darcy-model
   #:darcy #:space-step #:conductivities #:unsaturated-models #:inlet-discharge
   #:darcy-size #:darcy-points #:darcy-depth
   #:richards-equation
   #:darcy-evolve))

