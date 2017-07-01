(in-package cl-user)

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
   #:darcy-simulation-output-time-interval
   #:darcy-simulation-plot-time-interval
   #:simulate))

(in-package darcy-use)

;; * Simulation probem
(defclass simulation-results ()
  ((time
    :initarg :time
    :accessor simulation-results-time)
   (mesh-points
    :initarg :mesh-points
    :accessor simulation-results-mesh-points)
   (mesh-boundaries
    :initarg :mesh-boundaries
    :accessor simulation-results-mesh-boundaries)
   (saturation
    :initarg :saturation
    :accessor simulation-results-saturation)
   (inlet-discharge
    :initarg :inlet-discharge
    :accessor simulation-results-inlet-discharge)
   (outlet-discharge
    :initarg :outlet-discharge
    :accessor simulation-results-outlet-discharge)
   (discharge-time
    :initarg :discharge-time
    :accessor simulation-results-discharge-time)
   (model
    :initarg :model
    :accessor simulation-results-model)))

(defun integrate (function-values boundaries)
  (loop for f across function-values
     for index from 0
     sum (* f (- (aref boundaries (1+ index))
                 (aref boundaries index)))))

(defun average (values boundaries)
  (/ (integrate values boundaries)
     (- (slice boundaries -1) (slice boundaries 0))))

(defun column-average-saturation (results)
  (with-accessors ((s simulation-results-saturation)
                   (x simulation-results-mesh-boundaries))
      results
    (init-array
     (lambda (i)
       (average (slice s i t) x))
     (array-dimension s 0)
     'double-float)))

(defun consumed-water (results &optional (area 1d0))
  (with-accessors ((in simulation-results-inlet-discharge)
                   (out simulation-results-outlet-discharge)
                   (time simulation-results-discharge-time))
      results
    (integrate (init-array (lambda (i) (* area (- (aref in i) (aref out i))))
                           (length in)
                           'double-float)
               time)))

(defun results-water-volume (results &optional (area 1d0))
  (with-accessors ((s simulation-results-saturation)
                   (model simulation-results-model))
      results
    (init-array
     (lambda (i)
       (reduce #'+
               (darcy-water-volume model (slice s i t) area)))
     (array-dimension s 0)
     'double-float)))

(defun abs-water-balance-error (results)
  (let ((consumed-water (consumed-water results))
        (water-volume (results-water-volume results)))
    (abs (- (slice water-volume -1) (slice water-volume 0) consumed-water))))

(defun water-balance-error (results)
  (with-accessors ((in simulation-results-inlet-discharge)
                   (out simulation-results-outlet-discharge)
                   (time simulation-results-discharge-time))
      results
    (let ((abs-error (abs-water-balance-error results))
          (inlet-overall (integrate in time))
          (outlet-overall (integrate out time)))
      (/ abs-error (+ inlet-overall outlet-overall)))))

;; ** Definition
(defclass darcy-simulation ()
  ((darcy
    :initarg :darcy
    :accessor darcy-simulation-model
    :documentation
    "Darcy model instance to simulation")
   (final-time
    :initarg :final-time
    :documentation
    "Final time of simulation (in seconds)")
   (output-time-interval
    :initarg :output-time-interval
    :documentation
    "Interval with which produce the output"
    :accessor darcy-simulation-output-time-interval)
   (fine-time-interval
    :initarg :fine-time-interval
    :documentation
    "Interval with which produce the discharge output")
   (plot-time-interval
    :initarg :plot-time-interval
    :documentation
    "Interval for plots: usualy longer than OUTPUT-TIME-INTERVAL"
    :accessor darcy-simulation-plot-time-interval)
   (initial-saturation
    :initarg :initial-saturation
    :documentation
    "Initial saturation for simulation")
   (simulation-result
    :initform nil
    :accessor darcy-simulation-results
    :documentation
    "Storage for the result of simulation"))
  (:documentation
   "Full representation of the Darcy model simulation"))

;; ** Printing
(defmethod print-object ((object darcy-simulation) out)
  (with-slots (darcy final-time output-time-interval
                     plot-time-interval initial-saturation simulation-result)
      object
    (print-unreadable-object (object out :type t)
      (format out "~@<~:_~A ~:_final-time = ~A ~:_output-time = ~A\
 ~:_initial-saturation = ~:_~A~:>"
              darcy final-time output-time-interval initial-saturation))))

;; ** Simulation
(defmethod simulate ((model darcy-simulation))
  (with-slots (darcy final-time output-time-interval fine-time-interval
                     plot-time-interval initial-saturation
                     simulation-result)
      model
    (destructuring-bind (inlet saturation outlet)
        (darcy-evolve darcy
                      (fill-array (coerce initial-saturation 'double-float)
                                  (darcy-size darcy)
                                  'double-float)
                      final-time
                      fine-time-interval
                      output-time-interval)
      (let ((points (darcy-points darcy))
            (boundaries (darcy-boundaries darcy))
            (time (apply #'vector (loop for (time . s) in saturation
                                     collect time)))
            (discharge-time (apply #'vector
                                   (cons
                                    0d0
                                    (loop for (time . val) in inlet
                                       collect time)))))
        (let ((saturation-matrix (make-array (list (length time) (length points))
                                             :element-type 'double-float))
              (inlet-discharge (fill-array 0d0 (1- (length discharge-time)) 'double-float))
              (outlet-discharge (fill-array 0d0 (1- (length discharge-time)) 'double-float)))
          (loop for time-index from 0 below (length time)
               for (time . s) in saturation
             do (loop for space-index from 0 below (length points)
                   do (setf (aref saturation-matrix time-index space-index)
                            (aref s space-index))))
          (loop for time-index from 0
             for (t1 . in) in inlet
             for (t2 . out) in outlet
             do (setf
                 (aref inlet-discharge time-index) in
                 (aref outlet-discharge time-index) out))
          (setf simulation-result
                (make-instance 'simulation-results
                  :time time
                  :mesh-points points
                  :mesh-boundaries boundaries
                  :saturation saturation-matrix
                  :discharge-time discharge-time
                  :inlet-discharge inlet-discharge
                  :outlet-discharge outlet-discharge
                  :model darcy)))))))
