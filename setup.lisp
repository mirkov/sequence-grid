(in-package :sequence-grid)

(defparameter *array-type*
  #+clisp 'array
  #-clisp 'grid:foreign-array
  "Default grid array type

Can be one of 'grid:foreign-array or 'array")

(defparameter *float-type*
  #+clisp 'float
  #-clisp 'double-float
  "Default float type

Calls to the function are coerced to *float-type*

Valid values are 'single-float or 'double-float")

(defparameter *integer-type* '(unsigned-byte 32)
  "Default integer byte length")



(defconstant *v0*
  (make-grid `((,*array-type* 4) ,*float-type*)
	     :initial-contents '(0d0 1d0 2d0 3d0)))

(defconstant *v1*
  (make-grid `((,*array-type* 4) ,*float-type*)
	     :initial-contents '(10d0 11d0 12d0 13d0)))

(defconstant *v2*
  (make-grid `((,*array-type* 3) ,*float-type*)
	     :initial-contents '(0d0 1d0 2d0)))

(defparameter *array-3-4-double-float*
  (grid::test-grid-double-float *array-type* '(3 4)))

(defparameter *vector-4-double-float*
  (grid::test-grid-double-float *array-type* '(4)))

(defparameter *0-1-2* (grid::make-grid `((,*array-type*) ,*integer-type*)
			     :initial-contents '(0 1 2)))
(defparameter *0-2-4* (grid::make-grid `((,*array-type*) ,*integer-type*)
				      :initial-contents '(0 2 4)))
