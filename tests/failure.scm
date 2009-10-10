#!r6rs

(import (rnrs) (xunit))

(add-failure "this is an issue")
(assert-= 1 (+ 1 1))
(assert-boolean=? #t (not (not #f)))
(assert-char-ci=? #\Z (integer->char (+ 25 (char->integer #\Ａ))))
(assert-char=? #\Z (integer->char (+ 25 (char->integer #\a))))
(assert-string=? "R6RS" (string-append "R" (number->string 5) "RS"))
(assert-zero? (+ 1 1))
(skip-unless #t
  (assert-boolean=? #t #f)
  (assert-= 0 1))

(report)
