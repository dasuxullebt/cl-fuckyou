;;; -*- lisp -*-

;;; Copyright 2013 Christoph-Simon Senjak

;; Defines a structure that can be used by the fastcgi-connection to
;; manage its requests

(in-package :fuckyou)

(defclass fcgi-request ()
  ((id :type (integer 1 65535) :accessor id :initarg :id)
   (connection :accessor connection
	       :initarg :connection)
   (script-params :accessor script-params
		  :initform (make-instance 'chanl:bounded-channel :size 1))
   (script-stdout :accessor script-stdout
		  :initform (make-instance 'chanl:bounded-channel :size 1))
   (script-stderr :accessor script-stderr
		  :initform (make-instance 'chanl:bounded-channel :size 1))
   (script-stdin :accessor script-stdin
		 :initform (make-instance 'chanl:bounded-channel :size 1))
   (script-data-in :accessor script-data-in
		   :initform (make-instance 'chanl:bounded-channel :size 1))
   (request-end-out :accessor request-end-out
		    :initform (make-instance 'chanl:bounded-channel :size 1))))

(defmethod shared-initialize :before ((i fcgi-request) s &rest initargs &key &allow-other-keys)
  (if (not (getf initargs :id)) (error "fcgi-requests MUST be initialized with an id")))