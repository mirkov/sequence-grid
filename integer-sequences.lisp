;;;; sequences.lisp

(in-package #:sequence-grid)

;;; "sequences" goes here. Hacks and glory await!

(define-test constant
  (assert-numerical-equal (grid::make-grid `((,*default-grid-type*) ,*default-integer-type*)
				      :initial-contents '(1 1 1))
		     (ci-seq :constant 3 :value 1 :bits 1)))

(define-test integer
  (assert-numerical-equal (grid::make-grid `((,*default-grid-type*) ,*default-integer-type*)
				      :initial-contents '(0 1 2))
		     (ci-seq :integer 3))
  (assert-numerical-equal (grid::make-grid `((,*default-grid-type*) ,*default-integer-type*)
				      :initial-contents '(0 2 4))
		     (ci-seq :integer 3 :step 2)))

(define-test count
  (assert-numerical-equal (grid::make-grid `((,*default-grid-type*) ,*default-integer-type*)
				      :initial-contents '(0 1 2))
		     (ci-seq :count 3))
  (assert-numerical-equal (grid::make-grid `((,*default-grid-type*) ,*default-integer-type*)
				      :initial-contents '(0 2 4))
		     (ci-seq :count 3 :step 2)))

(define-test natural
  (assert-numerical-equal (grid::make-grid `((,*default-grid-type*) ,*default-integer-type*)
				      :initial-contents '(1 2 3))
		     (ci-seq :natural 3))
  (assert-numerical-equal (grid::make-grid `((,*default-grid-type*) ,*default-integer-type*)
				      :initial-contents '(1 3 5))
		     (ci-seq :natural 3 :step 2)))

(define-test float
  (assert-numerical-equal (grid::make-grid `((,*default-grid-type*) ,*default-integer-type*)
				      :initial-contents '(0 1 2))
		     (ci-seq :float 3))
  (assert-numerical-equal (grid::make-grid `((,*default-grid-type*) ,*default-integer-type*)
				      :initial-contents '(0 2 4))
		     (ci-seq :float 3 :step 2)))

(define-test complex
  (assert-numerical-equal (grid::make-grid `((,*default-grid-type*) ,*default-integer-type*)
				      :initial-contents
				      (list #C(0 0) #C(1 0) #C(2 0)))
		     (ci-seq :complex 3))
  (assert-numerical-equal (grid::make-grid `((,*default-grid-type*) ,*default-integer-type*)
				      :initial-contents 
				      (list #C(0 0) #C(2 0) #C(4 0)))
		     (ci-seq :complex 3 :step 2)))



(defgeneric ci-seq (kind count &key)
  (:documentation
   "Return a 1D grid of length COUNT that stores consequtive integers

KIND determins the integer representation (float, integer, byte, ...

The primary effect of KIND is to determine the type that will be used
to store the values: signed/unsigned bytes or integers, and their
length.

Accepted keys are
- VALUE (for KIND = :CONSTANT)
- STEP (for all other KIND's)
- BITS (specifying the integer representation length)
")
  (:method ((kind (eql :constant)) (count integer) &key value (bits 1))
    "Return vector of length `count', type signed-byte of BITS bits
where the value is VALUE

Allowed values of BITS are 8, 16, 32, 64"
    (assert (member bits '(1 2 4 8 16 32 64)))
    (assert (> count 0))
    (make-grid `((,*default-grid-type* ,count) (integer ,bits)) :initial-element value))
  (:method ((kind (eql :integer)) (count integer) &key (step 1) (bits 32))
    "Return vector of length `count', type signed-byte of BITS bits
where the value of each element is its index.

Allowed values of BITS are 8, 16, 32, 64"
    (assert (member bits '(8 16 32 64)))
    (assert (> count 0))
    (map-grid :source (lambda (arg)
			(* step arg))
	      :source-dims `(,count)
	      :destination-specification `((,*default-grid-type* ,count)
					   (signed-byte ,bits))))
  (:method ((kind (eql :count)) (count integer)  &key (step 1) (bits 32))
    "Return vector of length `count', type signed-byte of BITS bits
where the value of each element is its index.

Allowed values of BITS are 8, 16, 32, 64"
    (assert (member bits '(8 16 32 64)))
    (assert (> count 0))
    (map-grid :source (lambda (arg)
			(* step arg))
	      :source-dims `(,count)
	      :destination-specification
	      `((,*default-grid-type* ,count) (unsigned-byte ,bits))))
  (:method ((kind (eql :natural)) (count integer)
	    &key (step 1) (bits 16))
    "Return vector of length `count' of natural numbers, starting at 1.

Allowed values of BITS are 8, 16, 32, 64"
    (assert (member bits '(8 16 32 64)))
    (assert (> count 0))
    (map-grid :source #'(lambda (arg)
			  (1+ (* step arg)))
	      :source-dims `(,count)
	      :destination-specification `((,*default-grid-type* ,count)
					   (unsigned-byte ,bits))))
  (:method ((kind (eql :float)) (count integer)
	    &key (step 1))
    "Return floating vector of length `count', where the value of each
element is its index.

The floating type is either `single' or `double' (default), determined
by `type'"
    (map-grid :source #'(lambda (i)
			  (coerce (* step i) 'float))
	      :source-dims `(,count)
	      :destination-specification
	      `((,*default-grid-type* ,count) ,*default-element-type*)))
  (:method ((kind (eql :complex)) (count integer)
	    &key (step 1))
    "Return complex vector of length `count', where the real part of  each
element is its index.

The floating type is either `single' or `double' (default), determined by `type'"
    (map-grid :source #'(lambda (i)
			  (coerce (* step i) 'float))
	      :source-dims `(,count)
	      :destination-specification 
	      `((,*default-grid-type* ,count)
		(complex ,*default-element-type*)))))





