(in-package :cl-tex-example)

(defun run! ()
  (with-open-file (out "/tmp/out.tex"
                       :direction :output
                       :if-exists :supersede)
    (format
     out "~a"
     (tex! (header)
           (meta :date ""
                 :author '("Jin-Cheng Guu" "Y. H. Tham")
                 :title "Explicit Factorization of a Categorical Center")
           (env "document" ()
                (make-title)
                (print-compile-time)
                ;; (abstract)
                ;; (toc)
                ;; (section/intro)
                ;; (section/acknowledgement)
                ;; (bibliography)
                )))))

(defun header ()
  (tex!
   (documentclass '("extarticle" "12pt"))
   (usepackage '("geometry" "margin=0.3in, top=1in, bottom=1in")
               '("inputenc" "utf8")

               "amsmath"
               "amssymb"
               "amsthm"
               "mathtools"

               "thmtools"
               "thm-restate"

               "changepage"
               '("fontenc" "T1")

               "eulervm" "eucal" "eufrak" "concrete" ; the math fonts used in concrete mathematics
               "bbold"

               "lipsum" "hyperref" "datetime2"
               '("xcolor" "dvipsnames"))
   (:hypersetup (\{ (\, "colorlinks=true" "filecolor=red"
                        "linkcolor=blue" "urlcolor=blue")))
   (usepackage "graphicx")
   (:graphicpath (\{ "./graphics/"))
   (usepackage '("biblatex" "backend=biber, style=alphabetic"))
   (:bibliography (\{ "reference.bib")) ; pdflatex name.text -> biber name -> pdflatex name.text
   (:setcounter (\{ "section" "-1"))
   (:newtheoremstyle (\{ "mystyle"
                         "" "" "" ""
                         (:bfseries)
                         "" " "
                         ((:thmname "#1")
                          (:thmnumber " #2")
                          (:thmnote " (#3)"))))
   (:theoremstyle (\{ "mystyle"))
   (:newtheorem (\{ "theorem" ("equation" :[]) "Theorem"))
   (:|AtEndEnvironment| (\{ "theorem" "\\null\\hfill$\\diamond$"))
   ;; ..
   (:definecolor (\{ "my-dark-gray" "gray" "0.13"))
   (:newcommand (\{ "\\p" (\{ (:textup (\{ (:texttt "+")))))) ; small unary plus
   (:newcommand (\{ "\\m" (:mbox (\{ "-"))))             ; small unary minus
   (:renewcommand (\{ (:proof) (\{ (:color "{my-dark-gray}") (:oldproof))))
   (:renewcommand (\{ (:qedsymbol) ("$" (:backsquare) "$")))
   (:newenvironment "{Proof}"
                    "[1]"
                    "[Proof]"
                    ((:proof "[#1]")
                     "\\leftskip=0.5cm"
                     "\\rightskip=0cm")
                    (\{ (:endproof)))
   (:newcommand (\{ "\\Rep" (:operatorname "Rep")))

   ;; ..

   (usepackage "tikz"))
  ;; tikz setup..
  ;; tikz setup..
  )

(defun abstract ()
  '(:abstract
    ;;  TODO $ macro should translate b*, ^, and to into
    ;;  strings based on user-defined rules. It should also
    ;;  forbid any $ in the resulting string. mlet stands for
    ;;  math-let, should also be implemented.
    (mlet ((cat "C")
           (bop "bop")
           ;; TODO local math macros..
           (center "???macro")
           (b* "???macro")
           (-> "???macro"))

     "Given a braided fusion category" ($ cat)
     ", it is well known that the natural map "
     ($ (-> (b* cat (^ cat bop))
         (center cat)))
     "from the square of" ($ cat) "to the (Drinfeld)
            categorical center"
     ($ (center cat))
     "is an equivalence if and only if" ($ cat)
     "is modular. However,it is not clear how to construct
      the inverse and the natural isomorphisms. In this work, we
      provide an explicit construction using insights from a
      specific quantum field theory, and explore how the
      equivalence fails for the nonmodular cases.")))

(defun section/intro ()
  '(section
    ("Introduction")

    "~\\newline"
    (format nil "~%")
    "\\noindent" (:textbf "Ubiquity of Symmetries")
    (format nil "~%~%")
    "Symmetry has always been the central topic of pure mathematics.
The reason of its ubiquity is straightforward: Any symmetrical
object can be made much simpler by dividing out the extra
information, and any object with its original symmetry divided
out can be made more intuitive by recovering the extra
information. It is therefore useful, practically and
conceptually, to pass between both pictures."

    ;; TODO A pattern has emerged.
    ;; How to define a lisp macro for this?
    "~\\newline"
    (format nil "~%")
    "\\noindent" (:textbf "Ubiquity of Symmetries")
    (format nil "~%~%")
    "During the mid-" ($ 19)
    "th century, the use of symmetries entered and
fundamentally changed theoretical physics. In particular, the
advent of Maxwell's equations and their further reduction by"
    ($ (unitary-group 1)) "-symmetry provided profound insights
leading us to the birth of Einstein's relativity and quantum
mechanics. Later in the " ($ 20) "th century, the hidden
symmetries of our universe were further exploited by gauge
theorists, who provided the celebrated Standard Model."

    ;; TODO A pattern has emerged.
    ;; How to define a lisp macro for this?
    "~\\newline"
    (format nil "~%")
    "\\noindent" (:textbf "Groups as Classical Symmetries")
    (format nil "~%~%")

    "Mathematically, the backbone of symmetries in gauge theory
was provided by " (:textit "groups") " (collections of
symmetries). By understanding possible behaviors of groups, we
can predict the phenomena in an object (e.g. the space-time, a
statistical model, a molecule" (:cite "serre/finite-group-rep")
    ".. etc) whose symmetries are described by the group."

    ;; ..

    ;; \begin{center}
    ;; \includegraphics[height=4cm]{ochiai-unknots}
    ;; \end{center}
    (env :center ()
     (:includegraphics ("height=4cm" :[])
      "ochiai-unknots"))

    ;; ..

    ;; $$X \boxtimes Y \mapsto (X \otimes Y, c_{-,X} \otimes (c^{\text{-}1})_{Y,-})$$
    ($$ (mapsto
         (b* "X" "Y")
         (paren #((ot X Y)
                  (ot
                   (_ "c" "-,X")
                   (_ (^ "c" "\\text{-}1")
                    "Y,-"))))))

    ;; ..

    (env :enumerate ()

     ("Besides the natural monoidal structure,
there is another hidden tensor product for"
      ($ (center cat) ",")
      "namely the reduced tensor product"
      ($ (:overline (:otimes)))
      (:cite "reduced--tham")
      ".")

     ("It admits natural generalizations to all (oriented
        punctured) surfaces, namely the categorical center of
        higher genera"
      (:cite "guu/higher-genera-center")
      ".")
     ;; ..
     )))

(defun section/acknowledgement ()
  (section* ("Acknowledgement")
   "It is a great honor of the authors to contribute to such a
beautiful theory. The authors would like to express deep
gratitude to Alexander Kirillov for his guidance. The authors are
also benefited by fruitful discussions with Thibault Décoppet and
Victor Ostrik."))
