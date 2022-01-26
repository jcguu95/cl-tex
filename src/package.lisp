(defpackage :cl-tex
  (:use #:common-lisp)
  (:export :tex!))

(defpackage :cl-tex-user
  (:use #:common-lisp #:cl-tex)
  (:export

   :\{
   :\,
   :env
   :indent

   :documentclass
   :meta
   :usepackage
   :make-title
   :toc
   :print-compile-time

   ))
