(in-package #:sc)

(defvar *internal-server* nil)

(defclass internal-server (rt-server)
  ((sc-world :initform nil :accessor sc-world)
   (sc-thread :initform nil :accessor sc-thread)
   (sc-reply-thread :initform nil :accessor sc-reply-thread)
   (reply-handle-table :initform (make-hash-table :test #'equal) :reader reply-handle-table)
   (shm-interface :initform nil :accessor shm-interface)))

(defmethod is-local-p ((server internal-server))
  t)

(defun start-reply-handle-thread ()
  (bt:make-thread
   (lambda ()
     (let ((size (cffi:foreign-symbol-pointer "size"))
	   (buffer (cffi:foreign-symbol-pointer "buf"))
	   (mutex (cffi:foreign-symbol-pointer "mutex"))
	   (read-condition-var (cffi:foreign-symbol-pointer "read_condition_var"))
	   (write-condition-var (cffi:foreign-symbol-pointer "write_condition_var")))
       (cffi:foreign-funcall "communicate_init")
       (cffi:foreign-funcall "pthread_mutex_lock" :pointer mutex)
       (unwind-protect 
	    (loop
	      (cffi:foreign-funcall "pthread_cond_wait" :pointer read-condition-var
							:pointer mutex)
	      (let* ((size (cffi:mem-ref size :int))
		     (msg (make-array size :element-type '(unsigned-byte 8))))
		(when (/= size 0)
		  (cffi:with-pointer-to-vector-data (msg-ptr msg)
		    (memcpy msg-ptr (cffi:mem-ref buffer :pointer) size))
		  (let* ((message (osc:decode-bundle msg))
			 (handler (gethash (car message) (reply-handle-table *internal-server*))))
		    (if handler (handler-case (apply handler (cdr message))
				  (error (c) (format t "~a --error in reply thread~%" c)))
			(format t "not found handle for: ~a~%" message)))
		  (cffi:foreign-funcall "pthread_cond_signal" :pointer write-condition-var))))
	 (cffi:foreign-funcall "communicate_dealloc"))))
   :name "screply thread"))

(defmethod initialize-instance :before ((self internal-server) &key)
  (when *internal-server*
    (error "internal server must only one instance."))
  (setf *internal-server* self))

(defmethod install-reply-responder ((rt-server internal-server) cmd-name handler)
  (setf (gethash cmd-name (reply-handle-table rt-server)) handler))

(defmethod uninstall-reply-responder ((rt-server internal-server) cmd-name)
  (setf (gethash cmd-name (reply-handle-table rt-server)) nil))

(defmethod server-boot ((rt-server internal-server))
  (call-next-method)
  #+ccl
  (setup-shm-interface rt-server (cffi:foreign-funcall "getpid" :int)))

(defmethod bootup-server-process ((rt-server internal-server))
  (setf (sc-thread rt-server)
	(bt:make-thread (lambda ()
			  #+ccl
			  (ccl:setenv "SC_SYNTHDEF_PATH" (su:full-pathname *sc-synthdefs-path*))
			  #+sbcl
			  (sb-posix:setenv "SC_SYNTHDEF_PATH" (su:full-pathname *sc-synthdefs-path*) 1)
			  (setf (sc-reply-thread rt-server) (start-reply-handle-thread))
			  (set-print-func (cffi:foreign-symbol-pointer "sbcl_printf"))
			  (with-server-options (options (server-options rt-server))
			    (let ((world (make-world options)))
			      (setf (sc-world rt-server) world)))
			  (world-wait-for-quit (sc-world rt-server)))
			:name "scsynth thread"))
  (thread-wait (lambda () (sc-world rt-server))))

(defmethod cleanup-server ((rt-server internal-server))
  (bt:join-thread (sc-thread rt-server))
  (bt:destroy-thread (sc-reply-thread rt-server))
  (bt:join-thread (sc-reply-thread rt-server))
  (setf (sc-thread rt-server) nil
	(sc-reply-thread rt-server) nil
	(shm-interface rt-server) nil
	(sc-world rt-server) nil))

(defmethod send-message ((server internal-server) &rest msg)
  (let* ((encode-msg (apply #'osc:encode-message msg)))
    (cffi:with-pointer-to-vector-data (msg encode-msg)
      (world-send-packet (sc-world server) (length encode-msg) msg (cffi:foreign-symbol-pointer "sbcl_reply_func")))))

(defmethod send-bundle ((server internal-server) time lists-of-messages)
  (let* ((encode-msg (osc:encode-bundle lists-of-messages time)))
    (cffi:with-pointer-to-vector-data (msg encode-msg)
      (world-send-packet (sc-world server) (length encode-msg) msg (cffi:foreign-symbol-pointer "sbcl_reply_func")))))

(defmethod %buffer-data ((rt-server internal-server) buffer)
  (let* ((chanls (chanls buffer))
	 (frames (frames buffer))
	 (array (make-array (* chanls frames) :element-type 'single-float)))
    (cffi:with-pointer-to-vector-data (array-ptr array)
      (cffi:with-foreign-objects ((change-p :char)
				  (sndbuf '(:struct snd-buf)))
	(memset sndbuf 0 (cffi:foreign-type-size '(:struct snd-buf)))
	(cffi:with-foreign-slots ((samples data) sndbuf (:struct snd-buf))
	  (setf samples (* chanls frames)
		data array-ptr))
	(world-copy-sndbuf (sc-world rt-server) (bufnum buffer) sndbuf 0 change-p)))
    array))

(defun make-internal-server (name server-options)
  (make-instance 'internal-server
		 :server-options server-options
		 :name name))

(export '(make-internal-server) :sc)
