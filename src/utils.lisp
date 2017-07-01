(in-package cl-user)

(defpackage darcy-utils
  (:use #:cl)
  (:export #:fill-array #:init-array))


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

