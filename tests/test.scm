#!r6rs

(import (rnrs) (xunit))

(define-syntax should-be-false
  (syntax-rules ()
    ((_ expr)
     (assert (not expr)))))

(add-message! "this is a message")

(should-be-false (fail! "this is a to-be-reset failure"))
(reset!)

(assert (assert-raise message-condition? (raise (make-message-condition "OK"))))
(assert (assert-= 2 (+ 1 1)))
(assert (assert-boolean=? #t (not (not (not #f)))))
(assert (assert-char-ci=? #\z (integer->char (+ 25 (char->integer #\A)))))
(assert (assert-char=? #\Z (integer->char (+ 25 (char->integer #\A)))))
(assert (assert-string=? "R6RS" (string-append "R" (number->string 6) "RS")))
(assert (assert-boolean? #t))
(assert (assert-char? #\a))
(assert (assert-complex? +i))
(assert (assert-even? 2))
(assert (assert-exact? 1))
(assert (assert-finite? 0))
(assert (assert-inexact? #i3))
(assert (assert-infinite? +inf.0))
(assert (assert-integer-valued? 3+0i))
(assert (assert-integer? 3))
(assert (assert-list? '(l i s t)))
(assert (assert-nan? +nan.0))
(assert (assert-negative? -1))
(assert (assert-null? '()))
(assert (assert-number? 7))
(assert (assert-odd? 1))
(assert (assert-pair? '(car . cdr)))
(assert (assert-positive? 1))
(assert (assert-procedure? call/cc))
(assert (assert-rational-valued? 6/10+0.0i))
(assert (assert-rational? 6/10))
(assert (assert-real-valued? +nan.0+0i))
(assert (assert-real? -2.5+0i))
(assert (assert-string? "string"))
(assert (assert-symbol? 'symbol))
(assert (assert-vector? '#(v e c t o r)))
(assert (assert-zero? (- 1 1)))
(assert
 (skip-unless #f
   (assert (assert-boolean=? #t #f))
   (assert (assert-= 0 1))))

(report)
