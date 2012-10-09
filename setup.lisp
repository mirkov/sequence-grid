(in-package :sequence-grid)


(defparameter *default-integer-type* '(unsigned-byte 32)
  "Default integer byte length")


(defconstant *v0*
  (make-grid `((,*default-grid-type* 4) ,*default-element-type*)
	     :initial-contents '(0d0 1d0 2d0 3d0)))

(defconstant *v1*
  (make-grid `((,*default-grid-type* 4) ,*default-element-type*)
	     :initial-contents '(10d0 11d0 12d0 13d0)))

(defconstant *v2*
  (make-grid `((,*default-grid-type* 3) ,*default-element-type*)
	     :initial-contents '(0d0 1d0 2d0)))

(defparameter *array-3-4-double-float*
  (grid::test-grid-double-float *default-grid-type* '(3 4)))

(defparameter *vector-4-double-float*
  (grid::test-grid-double-float *default-grid-type* '(4)))

(defparameter *0-1-2*
  (grid::make-grid `((,*default-grid-type*)
		     ,*default-integer-type*)
		   :initial-contents '(0 1 2)))
(defparameter *0-2-4*
  (grid::make-grid `((,*default-grid-type*) ,*default-integer-type*)
		   :initial-contents '(0 2 4)))
