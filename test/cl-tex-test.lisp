(in-package :cl-tex-test)

;; If the first term is a keyword, translate it into the
;; expressions as follows. If the keyword has all-capital letters
;; in the name, downcase it; if it has all-downcase letters,
;; upcase it. If the first term expands to a string, treat the
;; following as annotations.
(tex! (:abc   "{1}" "[2]" "{3}"))       ; => \abc{1}[2]{3}
(tex! (:|TeX| "{1}" "[2]" "{3}"))       ; => \TeX{1}[2]{3}
(tex! (:|tex| "{1}" "[2]" "{3}"))       ; => \TEX{1}[2]{3}
;;
(tex! (:hypersetup "{a=1, b=2}"
                   "{3}" "[4]" "(5)"))  ; => \hypersetup{a=1, b=2}{3}[4](5)

(tex! (:hypersetup "{a=1, b=2}" "{3}" "[4]" "(5)")
      (make-title)
      (make-title))

(tex! (:newtheoremstyle "{mystyle}"
                        "{}" "{}" "{}" "{}"
                        (\{ (:bfseries))
                        "{}" "{ }"
                        ((:thmname "#1")
                         (:thmnumber " #2")
                         (:thmnote " (#3)"))))

(tex! (:date (:hi)))                    ; => \\date\\hi
(tex! 1)                                ; => "1"
(tex! 1 2)                              ; => "1\n2"
(tex! (1 2))                            ; => "12"
(tex! (1 2 (:hyper)))                   ; => "12\\hyper"

;; Test
(keyword-dispatch :day "{Monday}" (\{ (:day))) ; => \\day{Monday}{\\day}
