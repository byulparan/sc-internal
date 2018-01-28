
(asdf/defsystem:defsystem #:sc-internal
  :depends-on (#:sc
	       #+sbcl #:trivial-main-thread)
  :components ((:file "ffi-type")
	       (:file "internal-server")))

