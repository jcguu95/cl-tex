(defsystem :cl-tex
  :name "cl-tex"
  :description "cl-schedule is a program that helps you write TeX
  in common lisp."
  :author ("Jin-Cheng Guu <jcguu95@gmail.com>")
  :license "MIT"
  :version "0.0.1"
  :components ((:module "src"
                :components ((:file "package")
                             (:file "cl-tex")
                             (:file "cl-tex-user"))))
  :serial t
  :depends-on (:cl-ppcre))

(defsystem :cl-tex-test
  :depends-on (#:cl-tex)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "cl-tex-test")
                             (:file "cl-tex-user-test"))))
  :serial t)

(defsystem :cl-tex-example
  :depends-on (#:cl-tex)
  :components ((:module "example"
                :components ((:file "package")
                             (:file "example"))))
  :serial t)
