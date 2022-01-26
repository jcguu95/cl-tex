(in-package :cl-tex)

(defmacro tex! (&rest body)
  "The core compiler."
  (let ((l (length body)))
    (case l
      (0 "")
      (1 (let ((term (car body)))
           (cond ((atom term)
                  `(format nil "~a" ,term))
                 ((keywordp (car term))
                  `(keyword-dispatch ,@term))
                 ((symbolp (car term))
                  `,term)
                 (t
                  `(format nil "~{~a~}"
                           (list ,@(loop for x in term
                                         collect `(tex! ,x))))))))
      (t `(concatenate 'string
                       (tex! ,(car body))
                       (format nil "~%")
                       (tex! ,@(cdr body)))))))

(defun keyword-dispatch- (&rest body)
  (let* ((keyword (car body))
         (name (string-downcase (symbol-name keyword))))
    (assert (keywordp keyword))
    ;; TODO Implement upcase/downcase trasformer.
    (format nil "\\~a~{~a~}" name (cdr body))))

(defmacro keyword-dispatch (&rest body)
  `(keyword-dispatch-
    ,(car body)
    ,@(loop for term in (cdr body)
            collect `(tex! ,term))))
