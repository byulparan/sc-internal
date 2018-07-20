
(asdf/defsystem:defsystem #:sc-internal
  :depends-on (#:cl-collider)
  :components ((:file "ffi-type")
	       (:file "internal-server")))

