(in-package darcy-utils)

;; * General utility functions
;; ** =FILL-ARRAY=
;; Creates new array with all the elements filled by =VALUE=
(defun fill-array (value dimensions &optional (type t))
  "Creates a new array with DIMENSIONS and all items filled by VALUE.
Optional type can be provided (defaults to T)"
  (make-array dimensions :element-type type :initial-element value))

;; ** =INIT-ARRAY=
;; Creates new array initialized by function applied to indices.
(defun init-array (function dimensions &optional (type t))
  "Creates a new array (for now: vectors only) with all items
  initialize by FUNCTION"
  (let ((array (make-array dimensions :element-type type)))
    (loop for i from 0 below dimensions
       do (setf (aref array i)
                (funcall function i)))
    array))

;; * Generic instantiation

;; ** Default: argument is its own instance
(defgeneric instantiate (argument)
  (:documentation
   "Instantiate argument provided")
  (:method ((argument t))
    argument))

;; ** For a vector: instantiate each item
(defmethod instantiate ((args vector))
  (make-array (length args)
              :initial-contents
              (loop for item across args
                 collect (instantiate item))))

;; ** List: instantiate each item
(defmethod instantiate ((args list))
  (loop for item in args
     collect (instantiate item)))

;; * Configuration

;; ** Definition
;; (defclass configuration ()
;;   ((configuration-class-name
;;     :initarg :class-name
;;     :reader configuration-class-name
;;     :documentation "Class name used for instantiation")
;;    (configuration-list
;;     :initarg :list
;;     :accessor configuration-list
;;     :documentation
;;     "Plist of parameters to be passed to MAKE-INSTANCE"))
;;   (:documentation
;;    "Provides the way to collect the arguments to instantiate 
;; a complex object"))

;; ;; ** Printing
;; (defmethod print-object ((obj configuration) out)
;;   (print-unreadable-object (obj out :type t)
;;     (with-slots (configuration-class-name configuration-list) obj
;;       (format out "~A ~{~A~^ ~}"
;;               configuration-class-name
;;               configuration-list))))

;; ;; ** Main functionality: =INSTANTIATE=
;; (defmethod instantiate ((args configuration))
;;   (with-slots (configuration-class-name configuration-list) args
;;     (apply #'make-instance configuration-class-name
;;            (loop for (key value) on configuration-list by #'cddr
;;               append (list key (instantiate value))))))

;; ;; ** Function to manipulate the configuration
;; (defun update-configuration (configuration &rest keyword-updates)
;;   "Creates new configuration based on current where the parameter values
;; provided by KEYWORD-UPDATES take precedence.
;; NOTE: if current configuration does not have a particular keyword from
;; KEYWORD-UPDATES, this keyword is ignored"
;;   (with-slots (configuration-class-name configuration-list) configuration
;;     (let ((new-list
;;            (loop for (parameter value) on configuration-list by #'cddr
;;               append (list parameter (getf keyword-updates parameter value)))))
;;       (make-instance 'configuration
;;         :class-name configuration-class-name
;;         :list new-list))))

;; ;; ** Reader-macros: =#@= and =#&=
;; (defun enable-configuration-syntax ()
;;   "Enables reader-macros #@ and #& to fill in configurations.
;;   #@ has a form: #@(CLASS-NAME :KEY1 V1 :KEY2 V2 ...)
;;   where
;;     CLASS-NAME is the name of the class (unquoted symbol)
;;     :KEY1, :KEY2, ... are the keys used for configuration
;;     V1, V2, ... are corresponding values (evaluated!)
;;   #& has a form #@(CLASS-NAME LIST-OF-PARAMETERS)
;;   where
;;     CLASS-NAME is the name of the class (unquoted symbol)
;;     LIST-OF-PARAMETERS is the list or  name of the variable
;;        containing the list of configuration parameters (evaluated)"
;;   (set-dispatch-macro-character
;;    #\# #\@
;;    #'(lambda (stream char1 char2)
;;        (declare (ignore char1 char2))
;;        (let* ((parameters (read stream t nil t))) 
;;          `(make-instance 'configuration
;;             :class-name ',(first parameters)
;;             :list (list ,@(rest parameters))))))
;;   (set-dispatch-macro-character
;;    #\# #\&
;;    #'(lambda (stream char1 char2)
;;        (declare (ignore char1 char2))
;;        (let* ((parameters (read stream t nil t)))
;;          `(make-instance 'configuration
;;             :class-name ',(first parameters)
;;             :list ,(second parameters))))))

