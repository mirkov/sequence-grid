;;;; package.lisp

(defpackage #:sequence-grid
  (:nicknames #:seq-grid)
  (:use #:cl #:grid #:map-grid-utils #:lisp-unit #:alexandria)
  (:shadow #:lisp-unit
	   :norm
	   :set-equal))

