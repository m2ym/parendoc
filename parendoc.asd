(in-package :cl-user)

(defpackage :parendoc-asd
  (:use :cl :asdf))
(in-package :parendoc-asd)

(defsystem :parendoc
  :version "0.1"
  :author "Tomohiro Matsuyama"
  :license "LLGPL"
  :depends-on (:alexandria
               :anaphora
               :trivial-types
               :swank)
  :components ((:module "src"
                :serial t
                :components ((:file "util")
                             (:file "model")
                             (:file "doc")
                             (:file "generate")
                             (:file "render")
                             (:file "markdown")
                             (:file "parendoc")))))
