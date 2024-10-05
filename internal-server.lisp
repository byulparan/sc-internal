(in-package #:sc)

(defvar *internal-server* nil)

(defclass internal-server (rt-server)
  ((sc-world :initform nil :accessor sc-world)
   (sc-buffer :initform nil :accessor sc-buffer)
   (sc-reply-thread :initform nil :accessor sc-reply-thread)
   (reply-handle-table :initform (make-hash-table :test #'equal) :reader reply-handle-table)))

(defmethod is-local-p ((server internal-server))
  t)

(cffi:defcallback sc-reply-lisp-callback :void nil
  (let* ((messages (cdr (sc-osc::decode-bundle (sc-buffer *internal-server*)))))
    (loop for message in messages
	  for handler = (gethash (car message) (reply-handle-table *internal-server*))
	  do (if handler (handler-case (apply handler (cdr message))
			   (error (c) (format t "~a --error in reply thread~%" c)))
	       (format t "not found handle for ~a~%" message)))))

(defun start-reply-handle-thread ()
  (bt:make-thread
   (lambda ()
     (setf *random-state* (make-random-state t))
     (cffi:foreign-funcall "sc_lisp_reply_thread" :pointer (static-vectors:static-vector-pointer (sc-buffer *internal-server*))
						  :pointer (cffi:callback sc-reply-lisp-callback)))
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
  (call-in-main-thread
   (lambda ()
     #+ccl (let* ((path (and *sc-synthdefs-path* (full-pathname *sc-synthdefs-path*))))
	     (when (and path (probe-file path))
	       (ccl:setenv "SC_SYNTHDEF_PATH" path)))
     #+sbcl (sb-posix:setenv "SC_SYNTHDEF_PATH" (full-pathname *sc-synthdefs-path*) 1)
     #+ecl (ext:setenv "SC_SYNTHDEF_PATH" (full-pathname *sc-synthdefs-path*))
     #+lispworks (hcl:setenv "SC_SYNTHDEF_PATH" (full-pathname *sc-synthdefs-path*))
     (setf (sc-reply-thread rt-server) (start-reply-handle-thread))
     (set-print-func (cffi:foreign-symbol-pointer "sc_lisp_printf"))
     (with-server-options (options (server-options rt-server))
       (let ((world (make-world options)))
	 (setf (sc-thread rt-server)
	   (trivial-main-thread:main-thread))
	 (setf (sc-world rt-server) world))))))

(defmethod cleanup-server ((rt-server internal-server))
  (call-in-main-thread
   (lambda () (world-wait-for-quit (sc-world rt-server) t)))
  (setf (sc-thread rt-server) nil)
  (cffi:foreign-funcall "sc_lisp_reply_quit")
  (bt:join-thread (sc-reply-thread rt-server))
  (setf (sc-reply-thread rt-server) nil)
  (setf (sc-world rt-server) nil))

(defmethod send-message ((server internal-server) &rest msg)
  (let* ((encode-msg (#-lispworks progn
		      #+lispworks sys:in-static-area
		      (apply #'sc-osc::encode-message msg))))
    (cffi:with-pointer-to-vector-data (msg encode-msg)
      (world-send-packet (sc-world server) (length encode-msg) msg (cffi:foreign-symbol-pointer "sc_lisp_reply_func")))))

(defmethod send-bundle ((server internal-server) time lists-of-messages)
  (let* ((encode-msg (#-lispworks progn
		      #+lispworks sys:in-static-area
		      (sc-osc::encode-bundle lists-of-messages (+ time (latency server))))))
    (cffi:with-pointer-to-vector-data (msg encode-msg)
      (world-send-packet (sc-world server) (length encode-msg) msg (cffi:foreign-symbol-pointer "sc_lisp_reply_func")))))


(defun control-get-sync (index)
  (cffi:with-foreign-slots ((control-bus) (sc-world *s*) (:struct world))
    (cffi:mem-aref control-bus :float (floor (floatfy index)))))

(defun control-set-sync (index value)
  (cffi:with-foreign-slots ((control-bus) (sc-world *s*) (:struct world))
    (setf (cffi:mem-aref control-bus :float (floor (floatfy index))) (coerce value 'single-float))))

(defun buffer-data (buffer)
  (let* ((chanls (chanls buffer))
	 (frames (frames buffer))
	 (array (#-lispworks progn
		 #+lispworks sys:in-static-area
		 (make-array (* chanls frames) :element-type 'single-float))))
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
