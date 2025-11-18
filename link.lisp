(in-package #:sc)

(cffi:defcfun (link-enable "LinkEnable") :void
  (bpm :float))
(cffi:defcfun (link-disable "LinkDisable") :void)
(cffi:defcfun (link-get-tempo "LinkGetTempo") :float)
(cffi:defcfun (%link-set-tempo "LinkSetTempo") :void
  (bpm :float))
(cffi:defcfun (link-get-time "LinkGetTime") :double)
(cffi:defcfun (link-get-beat "LinkGetBeat") :double)
(cffi:defcfun (%link-get-beat-to-time "LinkGetBeatToTime") :double
  (beat :double))
(cffi:defcfun (%link-get-time-to-beat "LinkGetTimeToBeat") :double
  (time :double))


(defun link-set-tempo (bpm &key relaunch lag)
  (declare (ignore relaunch lag))
  (%link-set-tempo (float bpm 1.0)))

(defun link-get-beat-to-time (beat)
  (%link-get-beat-to-time (float beat 1.0d0)))

(defun link-get-time-to-beat (time)
  (%link-get-time-to-beat (float time 1.0d0)))


(defclass link-clock (tempo-clock)
  ())

(defmethod beats-to-secs ((tempo-clock link-clock) beats)
  (link-get-beat-to-time beats))

(defmethod secs-to-beats ((tempo-clock link-clock) secs)
  (link-get-time-to-beat secs))

(defmethod tempo-clock-beats ((tempo-clock link-clock))
  (link-get-beat))

(defmethod tempo-clock-set-bpm ((tempo-clock link-clock) new-bpm)
  (link-set-tempo new-bpm)
  (call-next-method))


(defun sync-timeoffset ()
  (let* ((diff .0)
	 (min-diff 10000000)
	 (offset 0.0))
    (dotimes (i 8)
      (let* ((before-time (core-audio-time))
	     (link-time (link-get-time))
	     (after-time (core-audio-time)))
	(setf diff (- after-time before-time))
	(when (< diff min-diff)
	  (setf min-diff diff)
	  (setf offset (- link-time (+ before-time (* diff .5)))))))
    offset))

(defun link-clock (flag)
  (stop)
  (if flag (progn
	     (link-enable (sc-extensions:bpm))
	     (setf (link-offset *s*) (sync-timeoffset))
	     (pushnew #'link-set-tempo sc-extensions::*bpm-functions*)
	     (pushnew #'link-disable  *server-quit-hooks*)
	     (set-clock (make-instance 'sc::link-clock
			  :name "Link"
			  :server *s*
			  :timestamp #'link-get-time
			  :bpm (link-get-tempo)
			  :time-sync-p nil))
	     (sc-extensions:bpm (link-get-tempo)))
    (progn
      (setf (link-offset *s*) 0)
      (set-clock (make-instance 'sc::tempo-clock
		   :name (sc::name *s*)
		   :server *s*
		   :timestamp #'core-audio-time
		   :bpm (sc-extensions:bpm)
		   :time-sync-p nil))
      (alexandria::removef sc-extensions::*bpm-functions* #'link-set-tempo)
      (alexandria:removef *server-quit-hooks* #'link-disable)
      (link-disable))))

(export 'link-clock)



