(in-package :cl-user)
(defpackage cl-fiber-prop-asd
  (:use :cl :asdf))
(in-package :cl-fiber-prop-asd)

(defsystem cl-fiber-prop
;  :version "0.1-SNAPSHOT"
  :author "Martin Kielhorn"
  :license "GPL"
  :depends-on (:cffi :gsll)
  :serial t
  :components ((:file "package")
	       (:file "cubic-interpolation")
	       (:file "zbrent")
	       (:file "image")
	       (:file "hollow-waveguide")
	       (:file "fft-ffi")
	       (:file "step-waveguide"))
  :description "light propagation through a multimode fiber")
