;;;; sequences.lisp

(in-package #:sequences)

;;; "sequences" goes here. Hacks and glory await!


(define-test indgen
  (assert-grid-equal (grid::make-grid `((,*array-type*) ,*integer-type*)
				      :initial-contents '(0 1 2))
		     (integer-seq 3)))

(defgeneric ci-seq (kind cound &key)
  (:documentation
   "Return a 1D grid of length COUNT that stores consequtive integers

KIND determins the integer representation (float, integer, byte, ...

The primary effect of KIND is to determine the type that will be used
to store the values: signed/unsigned bytes or integers, and their
length.
")
  (:method ((kind (eql :integer)) (count integer) &key (len 32))
    "Return vector of length `count', type signed-byte of `len' bytes
where the value of each element is its index.

Allowed values of `len' are 8, 16, 32, 64"
    (assert (member len '(8 16 32 64)))
    (assert (> count 0))
    (map-grid :source #'identity
	      :source-dims `(,count)
	      :destination-specification `((,*array-type* ,count)
					   (signed-byte ,len))))
  (:method ((kind (eql :count)) (count integer)  &key (len 32))
    "Return vector of length `count', type signed-byte of `len' bytes
where the value of each element is its index.

Allowed values of `len' are 8, 16, 32, 64"
    (assert (member len '(8 16 32 64)))
    (assert (> count 0))
    (map-grid :source #'identity
	      :source-dims `(,count)
	      :destination-specification
	      `((,*array-type* ,count) (unsigned-byte ,len))))
  (:method ((kind (eql :natural)) (count integer) &key (len 16))
    "Return vector of length `count' of natural numbers, starting at 1.

Allowed values of `len' are 8, 16, 32, 64"
    (assert (member len '(8 16 32 64)))
    (assert (> count 0))
    (map-grid :source #'(lambda (arg)
			  (1+ arg))
	      :source-dims `(,count)
	      :destination-specification `((,*array-type* ,count)
					   (unsigned-byte ,len))))
  (:method ((kind (eql :float)) (count integer) &key (type 'double-float))
    "Return floating vector of length `count', where the value of each
element is its index.

The floating type is either `single' or `double' (default), determined
by `type'"
    (map-grid :source #'(lambda (i)
			  (coerce i 'float))
	      :source-dims `(,count)
	      :destination-specification `((,*array-type* ,count) ,type)))
  (:method ((kind (eql :complex)) (count integer) &key (type :double-float))
    "Return complex vector of length `count', where the real part of  each
element is its index.

The floating type is either `single' or `double' (default), determined by `type'"
    (map-grid :source #'(lambda (i)
			  (coerce i 'float))
	      :source-dims `(,count)
	      :destination-specification `((,*array-type* ,count) (complex ,type)))))



(define-test index-seq-float
  (assert-grid-equal
   (grid::make-grid `((,*array-type*) ,*float-type*)
		    :initial-contents '(0d0 1d0 2d0))
		     (index-seq-float 3)))



