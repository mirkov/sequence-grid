

(in-package :sequence-grid)

(define-test progression
  (assert-numerical-equal
   #(1d0 2d0 3d0)
   (progression :linear 1 3 :count 3))
  (assert-numerical-equal
   #(1d0 10d0 100d0)
   (progression :geometric 1 100 :count 3)))
  

(defgeneric progression (type begin end &key count &allow-other-keys)
  (:documentation
   "Return a 1D grid containg a progression of numbers between BEGIN and
END.  BEGIN need not be less then END.

COUNT determines the grid size.  The default is 21

TYPE determines the progression type:
- :LINEAR
- :GEOMETRIC
- :POWER (not implemented)
- :LOGARITHMIC (not implemented)
")
  (:method ((type (eql :linear)) (begin number) (end number)
	    &key (count 21))
    "linear sequence between numbers `begin' and `end'

 `begin' can be less than `end'"
    (assert (> count 0))
    (let ((scale (/ (- end begin)
		    (1- count))))
      (let ((grid (map-grid :source #'(lambda (i)
					(+ (*  i scale)
					   begin))
			    :destination-specification
			    `((,*default-grid-type* ,count) ,*default-element-type*))))
	(setf (gref grid (- count 1)) end)
	grid)))
  (:method ((type (eql :geometric)) (begin number) (end number)
	    &key (count 21))
    "Geometric sequence between two positive numbers `begin' and `end'
`begin' can be less than `end'"
    (assert (> count 0))
    (let ((rat (expt (/ end begin) (/ 1. (1- count))))
	  (value begin))
      (let ((grid (map-grid :source #'(lambda (i)
					(declare (ignore i))
					(prog1 value
					  (setf value (* value rat))))
			    :destination-specification
			    `((,*default-grid-type* ,count) ,*default-element-type*))))
	(setf (gref grid (- count 1)) end)
	grid))))