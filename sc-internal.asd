
(asdf/defsystem:defsystem #:sc-internal
  :depends-on (#:sc)
  :components ((:file "ffi-type")
	       (:file "internal-server")))

