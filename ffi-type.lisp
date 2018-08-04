(in-package #:sc)

#+ccl
(progn
  (cffi:load-foreign-library (cat (namestring (asdf/system:system-source-directory :sc-internal))
				  "libscsynth.1.0.0.dylib"))
  (cffi:load-foreign-library (cat (namestring (asdf/system:system-source-directory :sc-internal))
				  "libscsynth_add.dylib")))

#+Sbcl ;; should be load libscsynth on main-thread
(let* ((sem (sb-thread:make-semaphore)))
  (sb-thread:interrupt-thread
   (sb-thread:main-thread)
   (lambda ()
     (sb-int:with-float-traps-masked (:invalid :divide-by-zero)
       (cffi:load-foreign-library (cat (namestring (asdf/system:system-source-directory :sc-internal))
				       "libscsynth.1.0.0.dylib"))
       (cffi:load-foreign-library (cat (namestring (asdf/system:system-source-directory :sc-internal))
				       "libscsynth_add.dylib"))
       (sb-thread:signal-semaphore sem))))
  (sb-thread:wait-on-semaphore sem))

#+ecl
(handler-case 
    (cffi:load-foreign-library (cat (namestring (asdf/system:system-source-directory :sc-internal))
				       "libscsynth_add.dylib"))
  (error ()))

(cffi:defcfun memset :pointer
  (s :pointer)
  (c :int)
  (n :unsigned-long))

(cffi:defcfun memcpy :pointer
  (dst :pointer)
  (src :pointer)
  (len :unsigned-long))

(cffi:defcstruct snd-buf
  (sample-rate :double)
  (sample-dur :double)
  (data :pointer)
  (channels :int)
  (samples :int)
  (frames :int)
  (mask :int)
  (mask1 :int)
  (coord :int)
  (sndfile :pointer))

(cffi:defcstruct rate
  (sample-rate :double)
  (sample-dur :double)
  (buf-duration :double)
  (buf-rate :double)
  (slope-factor :double)
  (radians-per-sample :double)
  (buf-length :int)
  (filter-loops :int)
  (filter-remain :int)
  (filter-slope :double))


(cffi:defcstruct world
  (hw :pointer)
  (ft :pointer)
  (sample-rate :double)
  (buf-length :int)
  (buf-counter :int)
  (num-audio-bus-channels :unsigned-int)
  (num-control-bus-channels :unsigned-int)
  (num-inputs :unsigned-int)
  (num-outputs :unsigned-int)
  (audio-bus :pointer)
  (control-bus :pointer)
  (audio-bus-touched :pointer)
  (control-bus-touched :pointer)
  (num-snd-bufs :unsigned-int)
  (snd-bufs :pointer)
  (snd-bufs-non-real-time-mirror :pointer)
  (snd-buf-updates :pointer)
  (group :pointer)
  (full-rate (:struct rate))
  (buf-rate (:struct rate))
  (num-r-gens :unsigned-int)
  (r-gen :pointer)
  (num-units :unsigned-int)
  (num-graphs :unsigned-int)
  (num-groups :unsigned-int)
  (sample-offset :int)
  (nrt-lock :pointer)
  (num-shared-controls :unsigned-int)
  (shared-controls :pointer)
  (real-time :char)
  (running :char)
  (dump-osc :int)
  (driver-lock :pointer)
  (subsample-offset :float)
  (verbosity :int)
  (error-notification :int)
  (local-error-notification :int)
  (rendezvous :char)
  (restricted-path :pointer))

(cffi:defcstruct world-options
  (password :string)
  (num-buffers :unsigned-int)
  (max-logins :unsigned-int)
  (max-nodes :unsigned-int)
  (max-graph-defs :unsigned-int)
  (max-wire-bufs :unsigned-int)
  (num-audio-bus-channels :unsigned-int)
  (num-input-bus-channels :unsigned-int)
  (num-output-bus-channels :unsigned-int)
  (num-control-bus-channels :unsigned-int)
  (buf-length :unsigned-int)
  (realtime-memory-size :unsigned-int)
  (num-shared-controls :int)
  (shared-controls :pointer)
  (real-time :char)
  (memory-locking :char)
  (non-realtime-cmd-filename :string)
  (non-realtime-input-filename :string)
  (non-realtime-output-filename :string)
  (non-realtime-output-header-format :string)
  (non-realtime-output-sample-format :string)
  (preferred-sample-rate :unsigned-int)
  (num-r-gens :unsigned-int)
  (preferred-hardware-buffer-frame-size :unsigned-int)
  (load-graph-defs :unsigned-int)
  (input-streams-enabled :string)
  (output-streams-enabled :string)
  (in-device-name :pointer)
  (verbosity :int)
  (rendezvous :char)
  (ugens-plugin-path :string)
  (out-device-name :string)
  (restricted-path :string)
  (shared-memory-id :int))

;;;
(defmacro set-slots ((options) &body name-and-values)
  (cons 'setf
	(loop for (name value) on name-and-values by #'cddr
	      append
	      `((cffi:foreign-slot-value ,options '(:struct world-options) ',name) ,value))))

(defmacro with-server-options ((options server-options) &body body)
  `(cffi:with-foreign-objects ((,options '(:struct world-options)))
     (set-slots (,options)
       password (cffi-sys:null-pointer)
       num-buffers (server-options-num-sample-buffers ,server-options)
       max-logins (server-options-max-logins ,server-options)
       max-nodes (server-options-max-num-nodes ,server-options)
       max-graph-defs (server-options-max-num-synthdefs ,server-options)
       max-wire-bufs (server-options-num-wire-buffers ,server-options)
       num-audio-bus-channels (server-options-num-audio-bus ,server-options)
       num-input-bus-channels (server-options-num-input-bus ,server-options)
       num-output-bus-channels (server-options-num-output-bus ,server-options)
       num-control-bus-channels (server-options-num-control-bus ,server-options)
       buf-length (server-options-block-size ,server-options)
       realtime-memory-size (server-options-realtime-mem-size ,server-options)
       num-shared-controls 0
       shared-controls (cffi:null-pointer)
       real-time 1
       memory-locking 0
       non-realtime-cmd-filename (cffi-sys:null-pointer)
       non-realtime-input-filename (cffi-sys:null-pointer)
       non-realtime-output-filename (cffi-sys:null-pointer)
       non-realtime-output-header-format (cffi-sys:null-pointer)
       non-realtime-output-sample-format (cffi-sys:null-pointer)
       preferred-sample-rate (server-options-hardware-samplerate ,server-options)
       num-r-gens (server-options-num-random-seeds ,server-options)
       preferred-hardware-buffer-frame-size (server-options-hardware-buffer-size ,server-options)
       load-graph-defs (server-options-load-synthdefs-p ,server-options)
       input-streams-enabled (cffi-sys:null-pointer)
       output-streams-enabled (cffi-sys:null-pointer)
       in-device-name (cffi-sys:null-pointer)
       verbosity (server-options-verbosity ,server-options)
       rendezvous (server-options-publish-to-rendezvous-p ,server-options)
       ugens-plugin-path (alexandria:if-let ((plugin-path (format nil "~{~a~^:~}" (server-options-ugen-plugins-path ,server-options))))
			   plugin-path (cffi-sys:null-pointer))
       out-device-name (cffi-sys:null-pointer)
       restricted-path (cffi-sys:null-pointer)
       shared-memory-id 0)
     ,@body))


;;;
(cffi:defcfun ("SetPrintFunc" set-print-func) :int
  (func :pointer))

(cffi:defcfun "World_New" :pointer
  (options (:pointer (:struct world-options))))

(cffi:defcfun ("World_WaitForQuit" world-wait-for-quit) :void
  (world :pointer)
  (unload-plugins :bool))

(cffi:defcfun ("World_SendPacket" world-send-packet) :void
  (world :pointer)
  (size :int)
  (data :pointer)
  (reply-func :pointer))

(cffi:defcfun ("World_CopySndBuf" world-copy-sndbuf) :int
  (world :pointer)
  (index :unsigned-int)
  (outbuf :pointer)
  (only-if-changed :char)
  (out-did-chnge :pointer))

(cffi:defcfun "make_world" :pointer
  (options (:pointer (:struct world-options))))
