(asdf/defsystem:defsystem #:sc-internal
  :depends-on (#:cffi-shared-libs
	       #:cl-collider
	       #:static-vectors
	       #:trivial-main-thread)
  :components ((:file "ffi-type")
	       (:file "internal-server")))

