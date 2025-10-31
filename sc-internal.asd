(asdf/defsystem:defsystem #:sc-internal
  :depends-on (#:cl-collider
	       #:sc-extensions
	       #:static-vectors
	       #:trivial-main-thread)
  :serial t
  :components ((:file "ffi-type")
	       (:file "internal-server")
	       (:file "link")))

