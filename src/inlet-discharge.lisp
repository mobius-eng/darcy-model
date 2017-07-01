(in-package cl-user)

(defpackage :inlet-discharge
  (:use #:cl)
  (:export
   #:constant-inlet-discharge #:inlet-flow-rate
   #:fluctuating-inlet-discharge #:fluctuation-frequency #:fluctuation-delay
   #:noisy-inlet-discharge #:inlet-discharge-noise
   #:intermittent-inlet-discharge #:intermittent-periods-flow-rates))

(in-package inlet-discharge)

;; * Inlet (specific) discharge
;; ** Rational
;; To make it easier to input and to inspect inlet discharges, they are
;; implemented as classes-functions (with metaclass
;; =CLOSER-MOP:FUNCALLABLE-STANDARD-CLASS=).

;; ** Constant inlet discharge
;; Simple constant function represented as a class.
(defclass constant-inlet-discharge ()
  ((inlet-flow-rate
    :initarg :inlet-flow-rate
    :initform (/ 10d0 1000d0 3600d0)
    :accessor inlet-flow-rate
    :documentation "Flow rate in m/s"))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "Constant inlet discharge"))

(defmethod initialize-instance :after ((obj constant-inlet-discharge) &key)
  (closer-mop:set-funcallable-instance-function
   obj
   #'(lambda (x)
       (declare (ignore x)
                (optimize (speed 3)))
       (inlet-flow-rate obj))))

(defmethod print-object ((obj constant-inlet-discharge) out)
  (with-slots (inlet-flow-rate) obj
    (print-unreadable-object (obj out :type t)
      (format out "~@<~:_rate = ~A~:>" inlet-flow-rate))))

;; ** Fluctuating (regular) inlet discharge
;; Represents three-parameter regular fluctuating (oscillating) inlet
;; discharge defined by
;; \begin{equation}
;; q_{\text{inlet}} = \frac{1}{2}M(\sin{(2\pi F[t-D])} + 1),
;; \end{equation}
;; with $M$ being =INLET-FLOW-RATE=, $F$ being =FLUCTUATION-FREQUENCY=
;; and $D$ =FLUCTUATION-DELAY=.
(defclass fluctuating-inlet-discharge (constant-inlet-discharge)
  ((fluctuation-frequency
    :initarg :fluctuation-frequency
    :accessor fluctuation-frequency
    :documentation "Frequency in 1/s")
   (fluctuation-delay
    :initarg :fluctuation-delay
    :accessor fluctuation-delay
    :documentation "Delay in s"))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Fluctuating inlet discharge is calculated as
    m * sin(f * (t - d))
where
  m is the magnitude (INLET-FLOW-RATE)
  f is FLUCTUATION-FREQUENCY
  d is FLUCTUATION-DELAY"))

(defmethod initialize-instance :after ((obj fluctuating-inlet-discharge) &key)
  (closer-mop:set-funcallable-instance-function
   obj
   #'(lambda (time)
       (declare (optimize (speed 3))
                (type double-float time))
       (with-slots ((m inlet-flow-rate)
                    (f fluctuation-frequency)
                    (d fluctuation-delay)) obj
         (declare (type double-float m f d))
         (* 0.5d0 m (1+ (sin (* 2d0 pi f (- time d)))))))))

(defmethod print-object ((obj fluctuating-inlet-discharge) out)
  (with-slots ((m inlet-flow-rate)
               (f fluctuation-frequency)
               (d fluctuation-delay)) obj
    (print-unreadable-object (obj out :type t)
      (format out "~A * sin(2pi * ~A * (t - ~A))" m f d))))

;; ** Noisy inlet discharge
;; This inlet discharge is a composite of a base inlet discharge with the
;; noise: noise is applied as a percentage of the effective value of the
;; base inlet discharge.
(defclass noisy-inlet-discharge ()
  ((inlet-discharge-noise
    :initarg :inlet-discharge-noise
    :accessor inlet-discharge-noise
    :type (double-float 0d0 1d0)
    :documentation "White noise for the discharge")
   (base-inlet-discharge
    :initarg :base-inlet-discharge
    :accessor base-inlet-discharge
    :documentation "Base inlet discharge for which the noise will be added")
   (noise-time-interval
    :initform 300d0
    :documentation "Time interval over which the noise won't change")
   (noise-save :initform (make-hash-table)))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Noisy inlet discharge takes a BASE-INLET-DISCHARGE and adds a noise
to its value as per
  m +/- noise * 100%
Will only have an effect if m > 0 and noise < 1"))

(defmethod initialize-instance :after ((obj noisy-inlet-discharge) &key)
  (closer-mop:set-funcallable-instance-function
   obj
   #'(lambda (time)
       (with-slots ((noise inlet-discharge-noise)
                    (base base-inlet-discharge)
                    noise-time-interval
                    noise-save) obj
         (let* ((interval-number (ceiling time noise-time-interval))
                (saved-noise (gethash interval-number noise-save)))
           (unless saved-noise
             (setf saved-noise (random (* 2d0 noise)))
             (setf (gethash interval-number noise-save) saved-noise))
           (* (funcall base time) (+ 1d0 saved-noise (- noise))))))))

(defmethod print-object ((obj noisy-inlet-discharge) out)
  (with-slots ((noise inlet-discharge-noise)
               (inlet base-inlet-discharge)) obj
    (print-unreadable-object (obj out :type t)
      (format out "~@<~:_Base = ~A ~:_Noise = ~A~:>" inlet noise))))

;; ** Intermittent inlet discharge
(defclass intermittent-inlet-discharge ()
  ((intermittent-periods-flow-rates
    :initarg :intermittent-periods-flow-rates
    :reader intermittent-periods-flow-rates
    :documentation "List of pairs (PERIOD INLET-DISCHARGE)")
   (cycle-time
    :documentation "Overall time of the cycle"))
  (:documentation
   "Representation for intermittent irrigation. Each intermittent cycle is
represented as the collection of flow rates (INERMITTENT-PERIODS-FLOW-RATES)")
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((obj intermittent-inlet-discharge) &key)
  (with-slots (intermittent-periods-flow-rates cycle-time) obj
    (setf cycle-time
          (loop for (time) in intermittent-periods-flow-rates
             sum time))
    (closer-mop:set-funcallable-instance-function
     obj
     #'(lambda (time)
         (let* ((full-periods (floor time cycle-time))
                (reduced-time (- time (* full-periods cycle-time))))
           (loop for (period-time period-inlet) in intermittent-periods-flow-rates
              if (minusp (- reduced-time period-time))
              return (funcall period-inlet time)
              else
              do (decf reduced-time period-time)
              end))))))

(defmethod print-object ((obj intermittent-inlet-discharge) out)
  (print-unreadable-object (obj out :type t)
    (with-slots (intermittent-periods-flow-rates cycle-time) obj
      (format out "~{~A~^ ~} (~A)"
              intermittent-periods-flow-rates
              cycle-time))))
