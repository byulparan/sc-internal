
(asdf/defsystem:defsystem #:sc-internal
  :depends-on (#:sc #:sc-shm-interface)
  :components ((:file "ffi-type")
	       (:file "internal-server")))

