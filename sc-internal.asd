(asdf/defsystem:defsystem #:sc-internal
  :depends-on (#:cl-collider #:static-vectors)
  :components ((:file "ffi-type")
	       (:file "internal-server")))

