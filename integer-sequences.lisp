;;;; sequences.lisp

(in-package #:sequence-grid)

;;; "sequences" goes here. Hacks and glory await!

(define-test constant
  (assert-numerical-equal (grid::make-grid `((,*array-type*) ,*integer-type*)
				      :initial-contents '(1 1 1))
		     (ci-seq :constant 3 :value 1 :len 1)))

(define-test integer
  (assert-numerical-equal (grid::make-grid `((,*array-type*) ,*integer-type*)
				      :initial-contents '(0 1 2))
		     (ci-seq :integer 3))
  (assert-numerical-equal (grid::make-grid `((,*array-type*) ,*integer-type*)
				      :initial-contents '(0 2 4))
		     (ci-seq :integer 3 :step 2)))

(define-test count
  (assert-numerical-equal (grid::make-grid `((,*array-type*) ,*integer-type*)
				      :initial-contents '(0 1 2))
		     (ci-seq :count 3))
  (assert-numerical-equal (grid::make-grid `((,*array-type*) ,*integer-type*)
				      :initial-contents '(0 2 4))
		     (ci-seq :count 3 :step 2)))

(define-test natural
  (assert-numerical-equal (grid::make-grid `((,*array-type*) ,*integer-type*)
				      :initial-contents '(1 2 3))
		     (ci-seq :natural 3))
  (assert-numerical-equal (grid::make-grid `((,*array-type*) ,*integer-type*)
				      :initial-contents '(1 3 5))
		     (ci-seq :natural 3 :step 2)))

(define-test float
  (assert-numerical-equal (grid::make-grid `((,*array-type*) ,*integer-type*)
				      :initial-contents '(0 1 2))
		     (ci-seq :float 3))
  (assert-numerical-equal (grid::make-grid `((,*array-type*) ,*integer-type*)
				      :initial-contents '(0 2 4))
		     (ci-seq :float 3 :step 2)))

(define-test complex
  (assert-numerical-equal (grid::make-grid `((,*array-type*) ,*integer-type*)
				      :initial-contents
				      (list #C(0 0) #C(1 0) #C(2 0)))
		     (ci-seq :complex 3))
  (assert-numerical-equal (grid::make-grid `((,*array-type*) ,*integer-type*)
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
")
  (:method ((kind (eql :constant)) (count integer) &key value (len 1))
    "Return vector of length `count', type signed-byte of `len' bytes
where the value is VALUE

Allowed values of `len' are 8, 16, 32, 64"
    (assert (member len '(1 2 4 8 16 32 64)))
    (assert (> count 0))
    (make-grid `((,*array-type* ,count) (integer ,len)) :initial-element value))
  (:method ((kind (eql :integer)) (count integer) &key (step 1) (len 32))
    "Return vector of length `count', type signed-byte of `len' bytes
where the value of each element is its index.

Allowed values of `len' are 8, 16, 32, 64"
    (assert (member len '(8 16 32 64)))
    (assert (> count 0))
    (map-grid :source (lambda (arg)
			(* step arg))
	      :source-dims `(,count)
	      :destination-specification `((,*array-type* ,count)
					   (signed-byte ,len))))
  (:method ((kind (eql :count)) (count integer)  &key (step 1) (len 32))
    "Return vector of length `count', type signed-byte of `len' bytes
where the value of each element is its index.

Allowed values of `len' are 8, 16, 32, 64"
    (assert (member len '(8 16 32 64)))
    (assert (> count 0))
    (map-grid :source (lambda (arg)
			(* step arg))
	      :source-dims `(,count)
	      :destination-specification
	      `((,*array-type* ,count) (unsigned-byte ,len))))
  (:method ((kind (eql :natural)) (count integer)
	    &key (step 1) (len 16))
    "Return vector of length `count' of natural numbers, starting at 1.

Allowed values of `len' are 8, 16, 32, 64"
    (assert (member len '(8 16 32 64)))
    (assert (> count 0))
    (map-grid :source #'(lambda (arg)
			  (1+ (* step arg)))
	      :source-dims `(,count)
	      :destination-specification `((,*array-type* ,count)
					   (unsigned-byte ,len))))
  (:method ((kind (eql :float)) (count integer)
	    &key (step 1) (type 'double-float))
    "Return floating vector of length `count', where the value of each
element is its index.

The floating type is either `single' or `double' (default), determined
by `type'"
    (map-grid :source #'(lambda (i)
			  (coerce (* step i) 'float))
	      :source-dims `(,count)
	      :destination-specification `((,*array-type* ,count) ,type)))
  (:method ((kind (eql :complex)) (count integer)
	    &key (step 1) (type 'double-float))
    "Return complex vector of length `count', where the real part of  each
element is its index.

The floating type is either `single' or `double' (default), determined by `type'"
    (map-grid :source #'(lambda (i)
			  (coerce (* step i) 'float))
	      :source-dims `(,count)
	      :destination-specification `((,*array-type* ,count) (complex ,type)))))





