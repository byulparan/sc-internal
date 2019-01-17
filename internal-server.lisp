(in-package #:sc)

(defvar *internal-server* nil)

(defclass internal-server (rt-server)
  ((sc-world :initform nil :accessor sc-world)
   (sc-buffer :initform nil :accessor sc-buffer)
   (sc-reply-thread :initform nil :accessor sc-reply-thread)
   (reply-handle-table :initform (make-hash-table :test #'equal) :reader reply-handle-table)))

(defmethod is-local-p ((server internal-server))
  t)

(defmacro with-sbcl-lock (&body body)
  `(unwind-protect (progn
		     (cffi:foreign-funcall "sbcl_lock")
		     ,@body)
     (cffi:foreign-funcall "sbcl_unlock")))

(defun start-reply-handle-thread ()
  (cffi:foreign-funcall "communicate_init" :pointer (static-vectors:static-vector-pointer (sc-buffer *s*)))
  (bt:make-thread
   (lambda ()
     (let ((size-ptr (cffi:foreign-symbol-pointer "sbcl_message_size")))
       (with-sbcl-lock
	 (loop
	   (cffi:foreign-funcall "sbcl_wait_signal")
	   (unless (sc-thread *s*) (return))
	   (let ((size (cffi:mem-ref size-ptr :int)))
	     (unless (zerop size)
	       (let* ((message (sc-osc::decode-bundle (sc-buffer *s*)))
		      (handler (gethash (car message) (reply-handle-table *internal-server*))))
		 (if handler (handler-case (apply handler (cdr message))
			       (error (c) (format t "~a --error in reply thread~%" c)))
		   (format t "not found handle for: ~a~%" message))
		 (setf (cffi:mem-ref size-ptr :int) 0)
		 (cffi:foreign-funcall "sbcl_send_signal"))))))))
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
  (setf (sc-buffer rt-server) (static-vectors:make-static-vector 2048 :initial-element 0))
  (setf (sc-thread rt-server)
	(bt:make-thread (lambda ()
			  #+ccl
			  (let* ((path (full-pathname *sc-synthdefs-path*)))
			    (when (and path (probe-file path))
			      (ccl:setenv "SC_SYNTHDEF_PATH" path)))
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
  (setf (sc-thread rt-server) nil)
  (cffi:foreign-funcall "sbcl_quit_signal")
  (bt:join-thread (sc-reply-thread rt-server))
  (setf (sc-reply-thread rt-server) nil)
  (setf (sc-world rt-server) nil))

(defmethod send-message ((server internal-server) &rest msg)
  (let* ((encode-msg (apply #'sc-osc::encode-message msg)))
    (cffi:with-pointer-to-vector-data (msg encode-msg)
      (world-send-packet (sc-world server) (length encode-msg) msg (cffi:foreign-symbol-pointer "sbcl_reply_func")))))

(defmethod send-bundle ((server internal-server) time lists-of-messages)
  (let* ((encode-msg (sc-osc::encode-bundle lists-of-messages time)))
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
