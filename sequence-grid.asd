;;;; sequence-grid.asd

(asdf:defsystem #:sequence-grid
  :serial t
  :description "Describe sequences here"
  :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
  :license "Not for redistribution"
  :depends-on (#:alexandria
               #:map-grid-utils
               #:lisp-unit)
  :components ((:file "sequence-grid-package-def")
               (:file "setup")
	       (:file "integer-sequences")
	       (:file "float-sequences")))

