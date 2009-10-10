#!r6rs

(import (rnrs) (xunit))

(add-message "this is a message")
(assert-= 2 (+ 1 1))
(assert-boolean=? #t (not (not (not #f))))
(assert-char-ci=? #\z (integer->char (+ 25 (char->integer #\A))))
(assert-char=? #\Z (integer->char (+ 25 (char->integer #\A))))
(assert-string=? "R6RS" (string-append "R" (number->string 6) "RS"))
(assert-zero? (- 1 1))
(skip-unless #f
  (assert-boolean=? #t #f)
  (assert-= 0 1))

(report)
