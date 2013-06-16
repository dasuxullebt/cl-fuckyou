;;; -*- lisp -*-

;;; Copyright 2013 Christoph-Simon Senjak

(defsystem "cl-fuckyou"
  :description "Fastcgi Usable through Common lisp Kits Yachting through the Oceans of Unportability"
;  :version ""
  :author "(let ((n \"Christoph-Simon Senjak\")) (format nil \"~A ~
  <~C~C~C~C~A>\" n (elt n 0) (elt n 10) (elt n 16) #\\@ \"uxul.de\"))"
  :license "GNU AGPL, Version 3, see agpl.txt"
  :depends-on ( #:f-underscore
		#:flexi-streams
		#:chanl
		#:hunchentoot
		#:bordeaux-threads
		#:usocket
		#:flexi-streams
		#:cl-containers
		#:trivial-garbage)
  :components ((:file "fuckyou")
	       (:file "several")
	       (:file "format")
	       (:file "request-ids")
	       (:file "request")
	       (:file "connection"))
  :serial t)