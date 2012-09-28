;;;; sequences.asd

(asdf:defsystem #:sequences
  :serial t
  :description "Describe sequences here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:map-grid-utils
               #:lisp-unit)
  :components ((:file "sequences-package-def")
               (:file "setup")
	       (:file "integer-sequences")
	       (:file "float-sequences")))

