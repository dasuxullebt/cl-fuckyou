;;; -*- lisp -*-

;;; Copyright 2013 Christoph-Simon Senjak

;; Defines a structure that can be used by the fastcgi-connection to
;; manage its request-ids.

(in-package :fuckyou)

(defstruct request-id-heap
  (next-free-num 1 :type (integer 1 65536))
  (queue-of-used (cl-containers:make-container
		  'cl-containers:priority-queue-on-container
		  :key #'identity
		  :test #'-
		  :container-type 'cl-containers:binary-search-tree)))

(defun acquire-request-id (rid)
  "Return the next free request id, or nil if no such exists (which is
unlikely since there are 65535 of them)."
  (declare (type request-id-heap rid))
  (cond
    ((cl-containers:empty-p (slot-value rid 'queue-of-used))
     (and
      (<= (slot-value rid 'next-free-num) 65535)
      (prog1 (slot-value rid 'next-free-num)
	(incf (slot-value rid 'next-free-num)))))
    (t (cl-containers:dequeue (slot-value rid 'queue-of-used)))))

(defun release-request-id (id rid)
  (declare (type request-id-heap rid)
	   (type (integer 1 65535) id))
  (cl-containers:enqueue (slot-value rid 'queue-of-used) id))