;;; -*- lisp -*-

;;; Copyright 2013 Christoph-Simon Senjak

;; Defines a structure and handlers for managing a fastcgi-connection

(in-package :fuckyou)

(defclass fastcgi-connection ()
  ((handler-thread :accessor handler-thread)
   (handler-rpc :accessor handler-rpc :initform (make-instance 'chanl:bounded-channel :size 1))
   (max-reqs :initform 1 :accessor max-reqs)
   (max-conns :initform 1 :accessor max-conns)
   (mpxs-conns :initform 0 :accessor mpxs-conns) ; no multiplexing yet
   (socket :accessor socket)
   (host :initform "localhost" :initarg :host :accessor host)
   (port :initform 6379 :initarg :port :accessor port)
   (sockets :accessor sockets :initform NIL) ; not used yet
   (requests :accessor requests
	     :initform (trivial-garbage:make-weak-hash-table :weakness :value))
   (request-id-heap :accessor request-id-heap
		    :initform (make-request-id-heap)))
  (:documentation "

handler-thread: the thread that runs run-handler

handler-rpc: a chanl-stream to which the other functions pass lists of
the form (command chanl-stream &rest args), where the passed
chanl-stream is the stream to which the result is passed.

max-reqs, max-conns, mpxs-conns: the corresponding fastcgi-properties,
which are negotiated by run-handler.

sockets: as soon as connection multiplexing will be implemented, this
will probably become a structure containing all the sockets to the
fastcgi-application.

requests: a weak hash table containing the fcgi-requests.

request-id-heap: a heap for managing the free request-ids, and keeping
them small. see request-ids.lisp.

"))

(defun run-handler (connection)
  (declare (type fastcgi-connection connection))

  (db 99 (format t "Starting handler...~%"))

  (let* ((received-data (make-array 65536 :initial-element 0
				    :element-type '(unsigned-byte 8)
				    :adjustable nil))
	 (received-length 0)
	 (socket (setf (socket connection)
		       (usocket:socket-connect (host connection) (port connection)
					       :element-type '(unsigned-byte 8))))
	 (sockstr (usocket:socket-stream socket)))

    ;; negotiate parameters
    
    (db 99 (format t "Negotiating...~%"))

    ;; TODO: make this work when the result does not come in one read
    ;; (currently too lazy, as it appears to work that way)
    (write-sequence (get-values-request) sockstr)
    (force-output sockstr)
    (incf received-length (read-sequence received-data sockstr
					 :start received-length))
    (multiple-value-bind
	  (success version type id length content next-free-index)
	(parse-record (subseq received-data 0))
      (declare (ignore length next-free-index))
      (assert success)
      (assert (= version +fcgi-version-1+))
      (assert (= type +fcgi-get-values-result+))
      (assert (= id 0))
      (let ((cur 0))
	(dotimes (fuck 3)
	  (multiple-value-bind
		(success name value next) (parse-name-val (subseq content cur))
	    (when (not success) (error "Malformed fcgi-get-values-result received."))
	    (labels ((sto (i)
		       (flexi-streams:string-to-octets i :external-format *external-format*))
		     (ots (i)
		       (flexi-streams:octets-to-string i :external-format *external-format*))
		     (oti (i)
		       (parse-integer (ots i))))
	      (cond
		((equalp (sto +fcgi-max-conns+) name) (setf (max-conns connection) (oti value)))
		((equalp (sto +fcgi-max-reqs+) name) (setf (max-reqs connection) (oti value)))
		((equalp (sto +fcgi-mpxs-conns+) name) (setf (mpxs-conns connection) (oti value)))
		(t (error (format nil "Got a name-value-pair I did not request:~%Name: ~S ~A~%Value: ~S ~A~%"
				  name (ots name) value (ots value))))))
	    (incf cur next)))))

    (usocket:socket-close socket)
    (setf socket (setf (socket connection)
		       (usocket:socket-connect (host connection) (port connection)
					       :element-type '(unsigned-byte 8))))
    (setf sockstr (usocket:socket-stream socket))

    (db 99 (format t "Negotiation finished.~%"))

    ;; negotiation finished. now let's wait for requests and instructions.
    
    (labels
	((new-request ()
	   (db 99 (format t "Creating new request.~%"))
	   (let ((id (acquire-request-id (request-id-heap connection))))
	     (when (not id) (error "No request IDs left"))
	     (when (> id (max-reqs connection))
	       (release-request-id id (request-id-heap connection))
	       (error "Too many requests"))
	     (let ((request (make-instance 'fcgi-request
					   :id id
					   :connection connection)))
	       (setf (gethash id (requests connection)) request)
	       ;; (trivial-garbage:finalize
	       ;;  request
	       ;;  (let ((id id)
	       ;; 	    (connection connection))
	       ;; 	(lambda ()
	       ;; 	  (hunchentoot:log-message*
	       ;; 	   :warning "Finallizer called for request with ID ~d on host ~A port ~A~%"
	       ;; 	   id (host connection) (host connection))
	       ;; 	  (release-request-id id (request-id-heap connection)))))
	       (db 99 (format t "Request created. Returning.~%"))
	       request)))
	 (old-request (id)
	   "Release a request that has been finished"
	   (db 99 (format t "Releasing request.~%"))
	   (when (not (remhash id (requests connection)))
	     (error (format nil "Probably a BUG: Nonexistent request-id released: ~d" id)))
	   (release-request-id id (request-id-heap connection)))
	 (wait-for-socket-input ()
	   "This function is for my convenience, it runs in a separate
thread and tells me when new input is available, and does a few other
convenience-things."
	   (db 99 (format t "wait-for-socket-input started.~%"))
	   (let* ((buffer (make-array '(65536) :element-type '(unsigned-byte 8)
				      :initial-element 0 :adjustable nil))
		  (read-length 0)
		  (currently-read 0)
		  (poll-attempts 0)
		  (return-stream (make-instance 'chanl:bounded-channel :size 1)))
	     (loop
		;; TODO: find something that does not poll here
		(assert (open-stream-p sockstr))
		(setf currently-read (read-sequence buffer sockstr :start read-length))
		(cond
		  ((zerop currently-read)
		   (when (> poll-attempts 100)
		     (let ((slp (* .001 (expt 2 (- poll-attempts 100)))))
		       (db 99 (format t "wait-for-socket-input: sleeping ~d seconds till next poll~%" slp))
		       (sleep slp)))
		   (setf poll-attempts (min (1+ poll-attempts) 112)))
		  (t
		   (setf poll-attempts 0)
		   (db 99 (format t "wait-for-socket-input: read-length = ~d bytes.~%" read-length))
		   (chanl:send (handler-rpc connection) (list :received return-stream buffer read-length))
		   (db 99 (format t "wait-for-socket-input: sent.~%"))
		   (multiple-value-bind (ret str) (chanl:recv return-stream :blockp t)
		     (assert (eql str return-stream))
		     (when (not (zerop ret))
		       (setf read-length 0)
		       (do ((i ret (1+ i))) ((= i 65536))
			 (setf (elt buffer read-length) (elt buffer i))
			 (incf read-length)))))))))
	 (wait-for-input ()
	   (let ((inp-list nil))
	     (maphash (lambda (k v)
			(declare (ignore k))
			(push (script-stdin v) inp-list)
			(push (script-data-in v) inp-list)
			(push (script-params v) inp-list)) (requests connection))
	     (push (handler-rpc connection) inp-list)
	     (multiple-value-bind (ret str) (chanl:recv inp-list :blockp t)
	       (cond ((eql str (handler-rpc connection))
		      (return-from wait-for-input (values ret :handler-rpc -1)))
		     (t (maphash (lambda (k v)
				   (declare (ignore k))
				   (cond ((eql (script-stdin v) str)
					  (return-from wait-for-input (values ret :script-stdin v)))
					 ((eql (script-data-in v) str)
					  (return-from wait-for-input (values ret :script-data-in v)))
					 ((eql (script-params v) str)
					  (return-from wait-for-input (values ret :script-params v)))))
				 (requests connection))))
	       (error "Probably a BUG: received data from nonexistent chanl-stream")))))
      (db 99 (format t "Starting reader thread and handling stuff...~%"))
      (let ((reader-thread (faden (wait-for-socket-input))))
	(declare (ignore reader-thread))
	(loop
	   (multiple-value-bind (data source exact-source) (wait-for-input)
	     (db 99 (format t "Got some input!~%"))
	     (case source
	       (:handler-rpc
		;; RPC-Calls
		(db 99 (format t "Got an rpc-call!~%"))
		(case (car data)
		  (:received
		   ;; data from socket in wait-for-socket-input was received
		   (db 99 (format t "Received stuff from socket!~%"))
		   (destructuring-bind (return-stream buffer read-length) (cdr data)
		     (multiple-value-bind (success version type id length content next-free-index)
			 (parse-record buffer :length read-length)
		       (declare (ignore length))
		       (cond
			 (success
			  (assert (= version +fcgi-version-1+))
			  (case type
			    (#.+fcgi-unknown-type+
			     (error (format nil "Script says it does not understand record type ~A" content)))
			    (#.+fcgi-stdout+
			     (chanl:send (script-stdout (gethash id (requests connection))) content :blockp t))
			    (#.+fcgi-stderr+
			     (chanl:send (script-stderr (gethash id (requests connection))) content :blockp t))
			    (#.+fcgi-end-request+
			     ;; TODOOOOOOOOOOO!!!
			     (db 99 (format t "Request ~d ended!~%" id))
			     (chanl:send (request-end-out (gethash id (requests connection))) content :blockp t)
			     (old-request id))
			    (t (error (format nil "unknown request type:~d" type))))
			  (chanl:send return-stream next-free-index :blockp t))
			 (t (chanl:send return-stream 0 :blockp
					t))))))
		  (:new-request
		   ;; new request was requested
		   ;; TODO: add restarts for when requests cannot be allocated
		   (db 99 (format t "Will create new request!~%"))
		   (destructuring-bind (ret role flags) (cdr data)
		     (let ((req (new-request)))
		       (write-sequence (begin-request (id req) :role role :flags flags) sockstr)
		       (force-output sockstr)
		       (chanl:send ret req :blockp t))))))
	       (:script-stdin
		(write-sequence (make-record +fcgi-version-1+ +fcgi-stdin+ (id exact-source) data) sockstr)
		(force-output sockstr))
	       (:script-data-in
		(write-sequence (make-record +fcgi-version-1+ +fcgi-data+ (id exact-source) data) sockstr)
		(force-output sockstr))
	       (:script-params
		(cond (data
		       (destructuring-bind (k . v) data
			 (write-sequence (make-record +fcgi-version-1+ +fcgi-params+ (id exact-source)
						      (make-name-val k v)) sockstr)
			 (force-output sockstr)
			 (db 99 (format t "Wrote ~S=~S~%" k v))))
		      (t (write-sequence (make-record +fcgi-version-1+ +fcgi-params+ (id exact-source)
						      (make-array '(0) :element-type '(unsigned-byte 8))) sockstr)))))))))))

(defun new-connection (host port)
  (let* ((conn (make-instance 'fastcgi-connection :host host :port port))
	 (thr (faden (run-handler conn))))
    (setf (handler-thread conn) thr)
    conn))

(defun new-request (connection params-hash-table
		    &key
		    (role +fcgi-responder+)
		    (flags +fcgi-keep-conn+))
  (let ((str (handler-rpc connection))
	(ret (make-instance 'chanl:bounded-channel :size 1)))
    (chanl:send str (list :new-request ret role flags) :blockp t)
    (multiple-value-bind (ret str) (chanl:recv ret :blockp t)
      (declare (ignore str))
      ;; ret now contains the request object.
      (maphash (lambda (k v)
		 (chanl:send (script-params ret) (cons k v) :blockp t))
	       params-hash-table)
      (chanl:send (script-params ret) nil)
      ret)))

(defun simple-request (connection params-hash-table input-stdin)
  (let ((req (new-request connection params-hash-table))
	(stdout-seq #())
	(stderr-seq #()))
    (format t "Request-ID: ~d~%" (id req))
    (when input-stdin (chanl:send (script-stdin req) input-stdin :blockp t))
    (chanl:send (script-stdin req) (make-array '(0) :element-type '(unsigned-byte 8)))
    (format t "Data sent. Entering receive loop.~%")
    (loop
       (multiple-value-bind
	     (data str)
	   (chanl:recv (list (script-stdout req) (script-stderr req) (request-end-out req)))
	 (format t "Received something.~%")
	 (cond
	   ((eql str (script-stdout req))
	    (format t "Received stdout.~%")
	    (when (zerop (length data))
	      (db 99 (format t "stdout ended. returning."))
	      (return-from simple-request (list stdout-seq stderr-seq)))
	    (setf stdout-seq (concatenate 'vector
					  stdout-seq data)))
	   ((eql str (script-stderr req))
	    (format t "Received stderr.~%")
	    (setf stderr-seq (concatenate 'vector
					  stderr-seq data)))
	   ((eql str (request-end-out req))
	    (format t "Received end-request.~%")
	    (return-from simple-request (list stdout-seq stderr-seq)))
	   (t (error "maeh!")))))))

(defun simple-php-request (connection script-file query-string)
  (let ((ph (make-hash-table)))
    ;; missing: PHP_SELF
    (dolist (c `(("GATEWAY_INTERFACE" . "CGI/1.1")
		 ("SERVER_ADDR" . "127.0.0.1")
		 ("SERVER_NAME" . "localhost")
		 ("SERVER_PROTOCOL" . "HTTP/1.1")
		 ("REQUEST_METHOD" . "GET")
		 ;; missing: REQUEST_TIME
		 ("QUERY_STRING" . ,query-string)
		 ("DOCUMENT_ROOT" . "/")
		 ;; missing: HTTP_ACCEPT
		 ("HTTP_ACCEPT_CHARSET" . "utf-8")
		 ("HTTP_ACCEPT_LANGUAGE" . "en")
		 ("HTTP_CONNECTION" . "Close")
		 ("HTTP_HOST" . "localhost")
		 ("REMOTE_ADDR" . "127.0.0.1")
		 ("REMOTE_HOST" . "localhost")
		 ("REMOTE_PORT" . "19253")
		 ;; missing: REMOTE_USER, REDIRECT_REMOTE_USER
		 ("SCRIPT_FILENAME" . ,script-file)
		 ;; missing: SERVER_ADMIN, SERVER_PORT, SERVER_SIGNATURE, REQUEST_URI
		 ;; missing: php-auth-digest
		 ;; missing: php-auth-user
		 ;; missing: php-auth-pw
		 ;; missing: auth-type
		 ;; missing: PATH_INFO, ORIG_PATH_INFO
		 ))
      (setf (gethash (car c) ph) (cdr c)))
    (simple-request connection ph nil)))

(defun direct-simple-php-request (host port document-root script-file query-string)
  (usocket:with-client-socket (sock stream host port :element-type '(unsigned-byte 8))
    (let* ((received-data (make-array 65536 :initial-element 0
				    :element-type '(unsigned-byte 8)
				    :adjustable nil))
	   (received-length 0)
	   (stdout-seq #())
	   (stderr-seq #()))
      (write-sequence (begin-request 1 :flags 0) stream)
      (write-sequence
       (make-record +fcgi-version-1+ +fcgi-params+ 1
		    (apply #'concatenate '(vector (unsigned-byte 8))
			   (map 'list (lambda (x)
					(make-name-val (car x) (cdr x)))
				`(("GATEWAY_INTERFACE" . "CGI/1.1")
				  ("SERVER_ADDR" . "127.0.0.1")
				  ("SERVER_NAME" . "localhost")
				  ("SERVER_PROTOCOL" . "HTTP/1.1")
				  ("SERVER_SOFTWARE" . "CL_FUCKYOU")
				  ("REQUEST_METHOD" . "GET")
				  ;; missing: REQUEST_TIME
				  ("QUERY_STRING" . ,query-string)
				  ("DOCUMENT_ROOT" . ,document-root)
				  ;; missing: HTTP_ACCEPT
				  ("HTTP_ACCEPT_CHARSET" . "utf-8")
				  ("HTTP_ACCEPT_LANGUAGE" . "en")
				  ("HTTP_CONNECTION" . "Close")
				  ("HTTP_HOST" . "localhost")
				  ("REMOTE_ADDR" . "127.0.0.1")
				  ("REMOTE_HOST" . "localhost")
				  ("REMOTE_PORT" . "19253")
				  ;; missing: REMOTE_USER, REDIRECT_REMOTE_USER
				  ("SCRIPT_FILENAME" . ,(concatenate 'string 
								     document-root script-file))
				  ("SCRIPT_NAME" . ,(concatenate 'string 
								     document-root script-file))
				  ("REQUEST_URI" . ,(concatenate 'string script-file "?" query-string))
				  ;; missing: SERVER_ADMIN, SERVER_PORT, SERVER_SIGNATURE
				  ;; missing: php-auth-digest
				  ;; missing: php-auth-user
				  ;; missing: php-auth-pw
				  ;; missing: auth-type
				  ;; missing: PATH_INFO, ORIG_PATH_INFO
				  )))) stream)
      (write-sequence (make-record +fcgi-version-1+ +fcgi-stdin+ 1
				   (make-array '(0) :element-type '(unsigned-byte 8))) stream)
      (force-output stream)
      (loop
	 (cond
	   ((listen stream)
	    (format t "Eingabedaten vorhanden!~%")
	    (setf received-length
		  (read-sequence received-data stream :start received-length))
	    (format t "Empfangene laenge: ~d~%" received-length)
	    (multiple-value-bind
		  (success version type id length content next-free-index)
		(parse-record received-data :length received-length)
	      ;(declare (ignore length))
	      (cond
		(success
		 (format t "Juhu! Richtig geparst! Typ: ~d, Laenge: ~d~%" type length)
		 (assert (= version +fcgi-version-1+))
		 (assert (= id 1))
		 (case type
		   (#.+fcgi-stderr+
		    (db 99 (format t "Received stderr data~%"))
		    (setf stderr-seq (concatenate '(vector (unsigned-byte 8)) stderr-seq content)))
		   (#.+fcgi-stdout+
		    (db 99 (format t "Received stdout data~%"))
		    (setf stdout-seq (concatenate '(vector (unsigned-byte 8)) stdout-seq content)))
		   (#.+fcgi-end-request+
		    (db 99 (format t "Received end!~%"))
		    (return-from direct-simple-php-request
		      (list stdout-seq stderr-seq content))))
		 (setf (subseq received-data 0) (subseq received-data next-free-index))
		 (decf received-length next-free-index))
		(t
		 (error received-data)))))
	   (t (let ((rd (read-byte stream nil nil)))
		(cond (rd
		       (format t "Stream endet noch nicht!")
		       (setf (elt received-data received-length) rd)
		       (incf received-length))
		      (t
		       (format t "Stream endet hier!")
		       (return-from direct-simple-php-request
			 (list stdout-seq stderr-seq nil)))))
	      (sleep .2)))))))