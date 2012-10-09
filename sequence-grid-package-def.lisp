;;;; package.lisp

(defpackage #:sequence-grid
  (:nicknames #:seq-grid)
  (:use #:cl #:map-grid-utils #:lisp-unit #:alexandria)
  (:shadow #:lisp-unit
	   :norm
	   :set-equal)
  (:shadow :iterate :rotate))

(antik:make-user-package :sequence-grid)

