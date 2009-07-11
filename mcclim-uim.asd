;;;; -*- Mode: LISP; -*-
(asdf:defsystem :mcclim-uim
  :version "0.0.0"
  :serial t
  :components ((:file "package")
               (:file "uim-ffi")
               (:file "uim"))
  :depends-on (cffi mcclim))

