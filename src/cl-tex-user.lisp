(in-package :cl-tex-user)

(defmacro \{ (&rest xs)
  (if (= (length xs) 1)
      `(format nil "\{~a\}" (tex! ,(car xs)))
      `(format nil "~{\{~a\}~}" (list ,@(loop for x in xs
                                              collect `(tex! ,x))))))

(defmacro \, (&rest xs)
  `(format nil "~{~a~^, ~}"
           (list ,@(loop for x in xs
                         collect `(tex! ,x)))))

(defun indent (text &optional (step 2))
  (require 'cl-ppcre)
  (labels ((make-space (step)
             (assert (integerp step))
             (if (= step 0)
                 ""
                 (format nil " ~a" (make-space (1- step))))))
    (format nil "~{~a~^~%~}"
            (mapcar (lambda (x)
                      (format nil "~a~a" (make-space step) x))
                    (cl-ppcre:split #\Newline text)))))

(defmacro env (name options &rest body)
  (declare (ignore options))         ; TODO Implement this later.
  `(format nil "\\begin\{~a\}~%~a~%\\end\{~a\}"
           ,name (indent (tex! ,@body)) ,name))

(defun documentclass (x)
  (if (atom x)
      (format nil "\\documentclass\{~a\}" x)
      (format nil "\\documentclass[~a]\{~a\}" (cadr x) (car x))))

(defun meta (&rest body)
  (tex!
   (:date (\{ (getf body :date)))
   (:author (\{ (format nil "~{~a~^, ~}"
                        (getf body :author))))
   (:title (\{ (getf body :title)))))

(defun usepackage (&rest xs)
  (let ((l (length xs)))
    (case l
      (0 "")
      (1 (let ((x0 (car xs)))
           (if (atom x0)
               (format nil "\\usepackage\{~a\}" x0)
               (format nil "\\usepackage[~a]\{~a\}" (cadr x0) (car x0)))))
      (t (format nil "~a~%~a~&"
                 (usepackage (car xs))
                 (apply #'usepackage (cdr xs)))))))

(defun make-title ()
  (tex! (:maketitle)))

(defun toc ()
  (tex! (:tableofcontents)))

(defun print-compile-time ()
  (tex!
   (env "flushright" () "Compiled Time: [\\today\\,\\DTMcurrenttime] \\qquad")))
