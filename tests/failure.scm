#!r6rs

(import (rnrs) (xunit))

(define-syntax should-be-false
  (syntax-rules ()
    ((_ expr)
     (assert (not expr)))))

(should-be-false (fail! "this is an issue"))
(should-be-false (assert-raise error? (raise (make-message-condition "OK"))))
(should-be-false (assert-raise integer? (+ 1 1)))
(should-be-false (assert-= 1 (+ 1 1)))
(should-be-false (assert-boolean=? #t (not (not #f))))
(should-be-false (assert-char-ci=? #\Z (integer->char (+ 25 (char->integer #\Ａ)))))
(should-be-false (assert-char=? #\Z (integer->char (+ 25 (char->integer #\a)))))
(should-be-false (assert-string=? "R6RS" (string-append "R" (number->string 5) "RS")))
(should-be-false (assert-boolean? 't))
(should-be-false (assert-zero? (+ 1 1)))
(skip-unless #t
  (should-be-false (assert-boolean=? #t #f))
  (should-be-false (assert-= 0 1)))

(report)
