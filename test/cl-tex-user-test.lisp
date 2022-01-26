(in-package :cl-tex-user-test)

(\{ "a")                                ; => "{a}"
(\{ "a" "b" (:good))                    ; => "{a}{b}{\\good}"

(\, (:date) "a"
    (:day (\{ (:mon "{c}"))))           ; => "\\date, a, \\day{\\mon{c}}"
(\, (:date) (:date))                    ; => "\\date, \\date"

(tex!
 (env "document" ()
      (:hi "{1}")
      (env "center" ()
           (:hi "[2]")
           (make-title)))
 (env "document" ()
      (:hi "{1}")
      (env "center" ()
           (:hi "[2]")
           (make-title))))
