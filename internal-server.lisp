(in-package #:sc)

(defvar *internal-server* nil)

(defclass internal-server (rt-server)
  ((sc-world :initform nil :accessor sc-world)
   (sc-reply-thread :initform nil :accessor sc-reply-thread)
   (reply-handle-table :initform (make-hash-table :test #'equal) :reader reply-handle-table)))

(defmethod is-local-p ((server internal-server))
  t)

(defun start-reply-handle-thread ()
  (bt:make-thread
   (lambda ()
     (let ((fd (cffi:foreign-symbol-pointer "sc_fd"))
	   (msg (make-array 1024 :element-type '(unsigned-byte 8)))
	   (size (make-array 1 :element-type '(unsigned-byte 32))))
       (cffi:foreign-funcall "communicate_init")
       (setf fd (cffi:mem-ref fd :int))
       (assert (/= -1 fd) nil "invalid fd...in screply_thread")
       (cffi:with-pointer-to-vector-data (msg-ptr msg)
	 (cffi:with-pointer-to-vector-data (size-ptr size)
	   (unwind-protect 
		(loop
		  (let* ((result (cffi:foreign-funcall "read" :int fd :pointer size-ptr :unsigned-long 4
							      :unsigned-long)))
		    (when (/= result -1)
		      (assert (= result 4) nil "no invalid size_read..in screply_thread")
		      (let* ((message-size (aref size 0)))
			(assert (> 1024 message-size) nil
				"too long response message size: ~d!" message-size)
			(let ((result (cffi:foreign-funcall "read" :int fd :pointer msg-ptr :unsigned-long message-size
								   :unsigned-long)))
			  (assert (= result message-size) nil "no invalid message read!..in screply_thread"))
			(let* ((message (osc:decode-bundle msg))
			       (handler (gethash (car message) (reply-handle-table *internal-server*))))
			  (if handler (handler-case (apply handler (cdr message))
					(error (c) (format t "~a --error in reply thread~%" c)))
			    (format t "not found handle for: ~a~%" message)))))))
	     (cffi:foreign-funcall "communicate_dealloc"))))))
   :name "screply thread"))

(defmethod initialize-instance :before ((self internal-server) &key)
  (when *internal-server*
    (error "internal server must only one instance."))
  (setf *internal-server* self))

(defmethod install-reply-responder ((rt-server internal-server) cmd-name handler)
  (setf (gethash cmd-name (reply-handle-table rt-server)) handler))

(defmethod uninstall-reply-responder ((rt-server internal-server) cmd-name)
  (setf (gethash cmd-name (reply-handle-table rt-server)) nil))


(defmethod bootup-server-process ((rt-server internal-server))
  (setf (sc-thread rt-server)
	(bt:make-thread (lambda ()
			  #+ccl
			  (ccl:setenv "SC_SYNTHDEF_PATH" (full-pathname *sc-synthdefs-path*))
			  #+sbcl
			  (sb-posix:setenv "SC_SYNTHDEF_PATH" (full-pathname *sc-synthdefs-path*) 1)
			  #+ecl
			  (ext:setenv "SC_SYNTHDEF_PATH" (full-pathname *sc-synthdefs-path*))
			  (setf (sc-reply-thread rt-server) (start-reply-handle-thread))
			  (set-print-func (cffi:foreign-symbol-pointer "sbcl_printf"))
			  (with-server-options (options (server-options rt-server))
			    (let ((world (make-world options)))
			      (setf (sc-world rt-server) world)
			      (world-wait-for-quit (sc-world rt-server) t))))
			:name "scsynth thread"))
  (thread-wait (lambda () (sc-world rt-server))))

(defmethod cleanup-server ((rt-server internal-server))
  (bt:join-thread (sc-thread rt-server))
  (bt:destroy-thread (sc-reply-thread rt-server))
  (bt:join-thread (sc-reply-thread rt-server))
  (setf (sc-thread rt-server) nil
	(sc-reply-thread rt-server) nil
	(sc-world rt-server) nil))

(defmethod send-message ((server internal-server) &rest msg)
  (let* ((encode-msg (apply #'osc:encode-message msg)))
    (cffi:with-pointer-to-vector-data (msg encode-msg)
      (world-send-packet (sc-world server) (length encode-msg) msg (cffi:foreign-symbol-pointer "sbcl_reply_func")))))

(defmethod send-bundle ((server internal-server) time lists-of-messages)
  (let* ((encode-msg (osc:encode-bundle lists-of-messages time)))
    (cffi:with-pointer-to-vector-data (msg encode-msg)
      (world-send-packet (sc-world server) (length encode-msg) msg (cffi:foreign-symbol-pointer "sbcl_reply_func")))))


(defun control-get-sync (index)
  (cffi:with-foreign-slots ((control-bus) (sc-world *s*) (:struct world))
    (cffi:mem-aref control-bus :float index)))

(defun control-set-sync (index value)
  (cffi:with-foreign-slots ((control-bus) (sc-world *s*) (:struct world))
    (setf (cffi:mem-aref control-bus :float index) (coerce value 'single-float))))

(defun buffer-data (buffer)
  (let* ((chanls (chanls buffer))
	 (frames (frames buffer))
	 (array (make-array (* chanls frames) :element-type 'single-float)))
    (cffi:with-pointer-to-vector-data (array-ptr array)
      (cffi:with-foreign-slots ((snd-bufs) (sc-world *s*) (:struct world))
	(let* ((data (getf (cffi:mem-aref snd-bufs '(:struct snd-buf) (bufnum buffer)) 'data)))
	  (memcpy array-ptr data (* chanls frames (cffi:foreign-type-size :float))))))
    array))

(defun make-internal-server (name &key (server-options (make-server-options)))
  (make-instance 'internal-server
		 :server-options server-options
		 :name name))

(export '(make-internal-server control-get-sync control-set-sync buffer-data) :sc)
