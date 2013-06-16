;;; -*- lisp -*-

;;; Copyright 2013 Christoph-Simon Senjak

(in-package :fuckyou)

(defmacro faden (&body body)
  (let ((myvarsym (gensym)))
    `(bordeaux-threads:make-thread
      (let ((,myvarsym *standard-output*))
	(lambda ()
	  (let ((*standard-output* ,myvarsym))
	    ,@body))))))

(defparameter *debug-level* 99)

(defmacro db (level &body body)
  `(when (<= ,level *debug-level*)
     ,@body))

