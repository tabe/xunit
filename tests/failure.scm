#!r6rs

(import (rnrs) (xunit))

(assert-= 1 (+ 1 1))
(assert-boolean=? #t (not (not #f)))
(assert-char=? #\Z (integer->char (+ 25 (char->integer #\a))))
(assert-string=? "R6RS" (string-append "R" (number->string 5) "RS"))

(report)
