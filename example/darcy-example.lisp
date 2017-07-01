(in-package cl-user)

(defpackage :darcy-example
  (:use #:cl #:darcy-use))

(in-package darcy-example)

(defvar *conductivity*
  (make-instance 'conductivity))

(defvar *van-genuchten*
  (make-instance 'van-genuchten))

(defvar *inlet* (make-instance 'constant-inlet-discharge))

(defvar *size* 20)

(defvar *space-step* 0.05d0)

(defvar *darcy-model*
  (make-instance 'darcy
    :inlet-discharge *inlet*
    :unsaturated-models (fill-array *van-genuchten* *size*)
    :conductivities (fill-array *conductivity* *size*)
    :space-step *space-step*))

(defvar *s0* 0.01d0)

(defvar *final-time* (* 3600d0 12))
(defvar *output-time-interval* (* 3600d0 1))

(defvar *simulation*
  (make-instance 'darcy-simulation
    :darcy *darcy-model*
    :initial-saturation *s0*
    :final-time *final-time*
    :output-time-interval *output-time-interval*
    :fine-time-interval (/ *output-time-interval* 1)
    :plot-time-interval *output-time-interval*))

(simulate *simulation*)

(defvar *results* (darcy-simulation-results *simulation*))


(column-average-saturation *results*)

(consumed-water *results*)

(results-water-volume *darcy-model* *results*)

(abs-water-balance-error *darcy-model* *results*)

(water-balance-error *darcy-model* *results*)
