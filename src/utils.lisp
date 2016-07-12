(in-package darcy-utils)

(defun fill-array (value dimensions &optional (type t))
  (make-array dimensions :element-type type :initial-element value))

(defun init-array (function dimensions &optional (type t))
  "For now only works for vectors"
  (let ((array (make-array dimensions :element-type type)))
    (loop for i from 0 below dimensions
       do (setf (aref array i)
                (funcall function i)))
    array))
