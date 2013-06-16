;;; -*- lisp -*-

;;; Copyright 2013 Christoph-Simon Senjak

;; See http://www.fastcgi.com/drupal/node/6?q=node/22#S3

(in-package :fuckyou)
(defconstant +fcgi-listensock-fileno+ 0)
(defconstant +fcgi-version-1+ 1)

(defconstant +fcgi-begin-request+ 1)
(defconstant +fcgi-abort-request+ 2)
(defconstant +fcgi-end-request+ 3)
(defconstant +fcgi-params+ 4)
(defconstant +fcgi-stdin+ 5)
(defconstant +fcgi-stdout+ 6)
(defconstant +fcgi-stderr+ 7)
(defconstant +fcgi-data+ 8)
(defconstant +fcgi-get-values+ 9)
(defconstant +fcgi-get-values-result+ 10)
(defconstant +fcgi-unknown-type+ 11)
(defconstant +fcgi-maxtype+ +fcgi-unknown-type+)

(defconstant +fcgi-null-request-id+ 0)
(defconstant +fcgi-keep-conn+ 1)
(defconstant +fcgi-responder+ 1)
(defconstant +fcgi-authorizer+ 2)
(defconstant +fcgi-filter+ 3)
(defconstant +fcgi-request-complete+ 0)
(defconstant +fcgi-cant-mpx-conn+ 1)
(defconstant +fcgi-overloaded+ 2)
(defconstant +fcgi-unknown-role+ 3)
(defconstant +fcgi-max-conns+ "FCGI_MAX_CONNS")
(defconstant +fcgi-max-reqs+ "FCGI_MAX_REQS")
(defconstant +fcgi-mpxs-conns+ "FCGI_MPXS_CONNS")

;; encoding bullshit

(defparameter *external-format* :utf-8)

;; protocol basics

(defun make-record (version type id data)
  (declare (type (unsigned-byte 8) version type)
	   (type (unsigned-byte 16) id)
	   (type (vector (unsigned-byte 8) *) data))
  (let ((len (coerce (length data) '(unsigned-byte 16))))
    (concatenate '(vector (unsigned-byte 8))
		 (vector version type
			 (ash id -8) (mod id 256)
			 (ash len -8) (mod len 256)
			 0 0)
		 data)))

(defun make-name-val (name val)
  (let ((name (cond ((typep name 'string)
		     (flexi-streams:string-to-octets name
				       :external-format *external-format*))))
	(val (cond ((typep val 'string)
		    (flexi-streams:string-to-octets val
					:external-format *external-format*)))))
    (flet ((enc-len (len)
	     (cond ((<= len 127)
		    (list len))
		   (t
		    (map 'list #'(lambda (x) (mod x 255))
			 (list (ash len -24)
			       (ash len -16)
			       (ash len -8)
			       len))))))
      (let ((name-len (enc-len (length name)))
	    (val-len (enc-len (length val))))
	(concatenate '(vector (unsigned-byte 8))
		     name-len val-len name val)))))
(defun parse-record (data &key (length (length data)))
      (labels
	  ((g-elt (data index)
	     (cond ((and (array-in-bounds-p data index) (< index length))
		    (elt data index))
		   (t (return-from parse-record nil))))
	   (g-subseq (data start end)
	     (cond ((and (array-in-bounds-p data (1- end)) (< (1- end) length))
		    (subseq data start end))
		   (t (return-from parse-record nil)))))
      (let* ((version (g-elt data 0))
	     (type (g-elt data 1))
	     (id (+ (* 256 (g-elt data 2)) (g-elt data 3)))
	     (length (+ (* 256 (g-elt data 4)) (g-elt data 5)))
	     (padding-length (g-elt data 6))
	     ;; elt data 7 == reserved
	     (content (g-subseq data 8 (+ 8 length)))
	     (next-free-index (+ 8 length padding-length)))
	(values
	 T version type id length content next-free-index))))

(defun parse-name-val (data)
      (labels
	  ((g-elt (data index)
	     (cond ((array-in-bounds-p data index)
		    (elt data index))
		   (t (return-from parse-name-val nil))))
	   (g-subseq (data start end)
	     (cond ((array-in-bounds-p data (1- end))
		    (subseq data start end))
		   (t (return-from parse-name-val nil))))
	   (parse-len (data index)
	       (cond ((<= (g-elt data index) 127)
		      (values (g-elt data index) (1+ index)))
		     (t
		      (values
		       (+ (* #x1000000 (- (g-elt data index) #b10000000))
			  (* #x10000 (g-elt data (1+ index)))
			  (* #x100 (g-elt data (+ 2 index)))
			  (g-elt data (+ 3 index))) (+ index 4))))))
	(multiple-value-bind
	      (name-length val-l-index) (parse-len data 0)
	  (multiple-value-bind
		(value-length name-begin) (parse-len data val-l-index)
	    (values T
		    (g-subseq data name-begin (+ name-begin name-length))
		    (g-subseq data (+ name-begin name-length)
			      (+ name-begin name-length value-length))
		    (+ name-begin name-length value-length))))))

(defun get-values-request ()
  (make-record +fcgi-version-1+
	       +fcgi-get-values+
	       0
	       (concatenate '(vector (unsigned-byte 8))
			    (make-name-val +fcgi-max-conns+ "")
			    (make-name-val +fcgi-max-reqs+ "")
			    (make-name-val +fcgi-mpxs-conns+ ""))))

(defun begin-request (id &key (role +fcgi-responder+) (flags +fcgi-keep-conn+))
  (make-record +fcgi-version-1+
	       +fcgi-begin-request+
	       id
	       (coerce (vector (ash role -8)
			       (mod role 256)
			       flags
			       0 0 0 0 0) '(vector (unsigned-byte 8)))))


;; Note: Deprecated
(defun make-cgi-init (php-file query-string)
  (make-record +fcgi-version-1+
	       +fcgi-params+
	       77
  (concatenate '(vector (unsigned-byte 8))
	       (make-name-val "PHP_SELF" "TODO ***************")
	       (make-name-val "GATEWAY_INTERFACE" "CGI/1.1")
	       (make-name-val "SERVER_ADDR" "127.0.0.1")
	       (make-name-val "SERVER_NAME" "localhost")
	       (make-name-val "SERVER_PROTOCOL" "HTTP/1.1")
	       (make-name-val "REQUEST_METHOD" "GET")
	       (make-name-val "REQUEST_TIME" (format nil "~s" (get-universal-time)))
	       (make-name-val "QUERY_STRING" query-string)
	       (make-name-val "DOCUMENT_ROOT" "/")
	       ;; missing: HTTP_ACCEPT
	       (make-name-val "HTTP_ACCEPT_CHARSET" "utf-8")
	       ;; missing: HTTP_ACCEPT_ENCODING
	       (make-name-val "HTTP_ACCEPT_LANGUAGE" "en")
	       (make-name-val "HTTP_CONNECTION" "Close")
	       (make-name-val "HTTP_HOST" "localhost")
	       ;; missing: referer
	       ;; missing: user-agent
	       ;; missing: https
	       (make-name-val "REMOTE_ADDR" "127.0.0.1")
	       (make-name-val "REMOTE_HOST" "localhost")
	       (make-name-val "REMOTE_PORT" "19253")
	       ;; missing: remote-user
	       ;; missing: redirect-remote-user
	       (make-name-val "SCRIPT_FILENAME" php-file)
	       ;; missing: server-admin
	       ;; missing: server-port
	       ;; missing: server-signature
	       (make-name-val "REQUEST_URI" query-string)
	       ;; missing: php-auth-digest
	       ;; missing: php-auth-user
	       ;; missing: php-auth-pw
	       ;; missing: auth-type
	       (make-name-val "PATH_INFO" "/")
	       ;; missing (?): orig-path-info
	       )
  ))