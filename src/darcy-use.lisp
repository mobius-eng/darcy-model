(in-package darcy-use)

;; * Ease of use for Darcy model
;; ** Default configurations
;; *** Enable configuration syntax
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-configuration-syntax))

;; *** Conductivity configurations
(defconstant +default-conductivity+
  #@(conductivity :liquid-viscosity 1d-6 :intrinsic-permeability 1d-9)
  "Default configuration for CONDUCTIVITY:
LIQUID-VISCOSITY of water @20C
INTRINSIC-PERMEABILITY of a coarse (ore like) porous medium")

;; *** Unsaturated configurations
;; Common parameters
(defconstant +unsaturated-basic+
  '(:saturated-water-content 0.3d0
    :residual-water-content 0.01d0
    :bubbling-pressure 0.333333333d0
    :mualem-exponent 0.5d0)
  "Common unsaturated model parameters: mostly tipical values")

;; Van Genucthen
(defconstant +default-van-genuchten+
  (let ((params (append +unsaturated-basic+ '(:n 1.5d0))))
    #&(van-genuchten params))
  "Default configuration for van Genuchten unsaturated model")

;; Brooks-Corey
(defconstant +default-brooks-corey+
  (let ((params (append +unsaturated-basic+ '(:pore-size-distribution-index 2d0))))
    #&(brooks-corey-mualem params))
  "Default configuation for Brooks-Corey-Mualem unsaturated-model")

;; *** Inlet specific dishcarge configurations
;; Constant
(defconstant +default-constant-inlet-discharge+
  #@(constant-inlet-discharge :inlet-flow-rate (/ 10d0 1000d0 3600d0))
  "Default configuration for constant inlet discharge:
equivalent of 10 L/(m2 h)")

;; Fluctuating
(defconstant +default-fluctuating-inlet-discharge+
  `(fluctuating-inlet-discharge
    :fluctuation-frequency ,(/ (* 24d0 3600d0))
    :fluctuation-delay 0d0
    :inlet-flow-rate ,(/ 10d0 1000d0 3600d0))
  "Default configuration for fluctuating inlet discharge:
MAX flow rate is equivalent of 10 L/(m2 h)
makes a sinusoide with period of 1 day")

;; Noisy
(defconstant +default-noisy-inlet-discharge+
  #@(noisy-inlet-discharge
    :inlet-discharge-noise 0.1d0
    :base-inlet-discharge +default-constant-inlet-discharge+)
  "Default configuration for noisy inlet discharge:
equivalent of 10 L/(m2 h) +/- 10%")

;; Intermittent
(defconstant +default-intermittent-inlet-discharge+
  #@(intermittent-inlet-discharge
     :intermittent-periods-flow-rates
     `((,(* 3600d0 24d0) ,+default-constant-inlet-discharge+)
       (,(* 3600d0 24d0) ,(update-configuration
                           +default-constant-inlet-discharge+
                           :inlet-flow-rate 0d0)))))

;; ** Darcy model generators
;; *** General
(defun general-darcy-model (&key
                              (conductivities
                               (fill-array
                                +default-conductivity+
                                20))
                              (unsaturated-models
                               (fill-array
                                +default-van-genuchten+
                                20))
                              (height 1d0)
                              (inlet-discharge
                               +default-constant-inlet-discharge+))
  "Creates a general Darcy model based on parameters provided.
CONDUCTIVITIES and UNSATURATED-MODELS must be vectors of CONFIGURATION
for each discrete volume"
  (let ((mesh-points-number (length conductivities)))
    (assert (= mesh-points-number (length unsaturated-models))
            ()
            "Conductivity and unsaturated model discretizations do not agree")
    (let ((dz (coerce (/ height mesh-points-number) 'double-float))
          (c (map 'vector #'instantiate conductivities))
          (u (map 'vector #'instantiate unsaturated-models))
          (q-in (instantiate inlet-discharge)))
      (make-instance 'darcy
        :inlet-discharge q-in
        :unsaturated-models u
        :conductivities c
        :space-step dz))))

;; *** Uniform Darcy model
(defun uniform-darcy-model (&key
                              (conductivity +default-conductivity+)
                              (unsaturated +default-van-genuchten+)
                              (height 1d0)
                              (mesh-points-number 20)
                              (inlet-discharge
                               +default-constant-inlet-discharge+))
  "Creates uniform Darcy model form single parameter for each discrete volume"
  (let ((dz (coerce (/ height mesh-points-number) 'double-float))
        (c (instantiate conductivity))
        (u (instantiate unsaturated))
        (q-in (instantiate inlet-discharge)))
    (make-instance 'darcy
      :inlet-discharge q-in
      :unsaturated-models (fill-array u mesh-points-number)
      :conductivities (fill-array c mesh-points-number)
      :space-step dz)))

(defun perturb-plist (plist perturbations)
  "Perturb PLIST values baed on PERTURBATIONS (plist as well)
If a KEY is PERTURBATIONS has a corresponding KEY in PLIST,
the value corresponding to KEY:
  1. Is assumed to be floating-point number
  2. Is perturbed producing VALUE +/- X * 100%, where X is
     corresponding value in PERTURBATIONS
PERTURBATIONS: plist of the form (:KEY1 X1 :KEY2 X2...),
where each XI is a floating-point number between 0.0 and 1.0"
  (loop for (k v) on plist by #'cddr
     append
       (let ((perturbation (getf perturbations k nil)))
         (if perturbation
             (list k (* v (+ 1
                             (* 2 perturbation (random 1d0))
                             (- perturbation))))
             (list k v)))))

(defun perturb-configuration (configuration perturbations)
  "Perturb CONFIGURATION according to PERTURBATIONS. If a KEY
from PERTURBATIONS is not found in CONFIGURATION, it is ignored.
Produces new CONFIGURATION"
  (let ((configuration-list (perturb-plist
                             (configuration-list configuration)
                             perturbations)))
    ;; (format t "~&~A~%" configuration-list)
    (apply #'update-configuration configuration configuration-list)))

(defun perturbed-darcy-model (&key
                                (conductivity +default-conductivity+)
                                (unsaturated +default-van-genuchten+)
                                (height 1d0)
                                (mesh-points-number 20)
                                (inlet-discharge
                                 +default-constant-inlet-discharge+)
                                perturbations)
  "Constructs perturbed Darcy's model according perturbations.
Perturbations is a plist of the form
    (:PARAMETER PERTURBATION ...)
The value for the :PARAMETER (in either conductivity or unsaturated model)
will be constructed as
    VALUE +/- PERTURBATION * 100%
where VALUE is the value from the argument list"
  (let ((dz (coerce (/ height mesh-points-number) 'double-float))
        (c (init-array (lambda (ignored)
                         (declare (ignore ignored))
                         (instantiate (perturb-configuration conductivity
                                                             perturbations)))
                       mesh-points-number))
        (u (init-array (lambda (ignored)
                         (declare (ignore ignored))
                         (instantiate (perturb-configuration unsaturated
                                                             perturbations)))
                       mesh-points-number))
        (q-in (instantiate inlet-discharge)))
    (make-instance 'darcy
      :inlet-discharge q-in
      :unsaturated-models u
      :conductivities c
      :space-step dz)))
