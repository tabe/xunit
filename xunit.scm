;;
;;   Copyright (c) 2009 Takeshi Abe. All rights reserved.
;;
;;   Redistribution and use in source and binary forms, with or without
;;   modification, are permitted provided that the following conditions
;;   are met:
;;
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;
;;    3. Neither the name of the authors nor the names of its contributors
;;       may be used to endorse or promote products derived from this
;;       software without specific prior written permission.
;;
;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(library (xunit)
  (export define-assert-equivalence
          assert-=
          assert-boolean=?
          assert-bytevector=?
          assert-char=?
          assert-eq?
          assert-equal?
          assert-eqv?
          assert-fl=?
          assert-fx=?
          assert-string-ci=?
          assert-string=?
          assert-symbol=?
          report)
  (import (rnrs))

  (define *failure* '())

  (define (add-failure expected expr actual)
    (let ((message (call-with-string-output-port
                    (lambda (port)
                      (put-datum port expected)
                      (put-string port " expected, but ")
                      (put-datum port expr)
                      (put-string port " => ")
                      (put-datum port actual)
                      (newline port)))))
      (set! *failure* (cons message *failure*))))

  (define-syntax assert-equivalence
    (syntax-rules ()
      ((_ equiv expected expr)
       (let ((actual expr))
         (guard (con
                 ((assertion-violation? con)
                  (add-failure expected 'expr actual)))
           (assert (equiv expected actual)))))))

  (define-syntax define-assert-equivalence
    (lambda (x)
      (syntax-case x ()
        ((k equiv)
         (let ((n (string->symbol (string-append "assert-" (symbol->string (syntax->datum #'equiv))))))
           (with-syntax ((name (datum->syntax #'k n)))
             #'(define-syntax name
                 (syntax-rules ()
                   ((_ expected expr)
                    (assert-equivalence equiv expected expr))))))))))

  (define-assert-equivalence =)
  (define-assert-equivalence boolean=?)
  (define-assert-equivalence bytevector=?)
  (define-assert-equivalence char=?)
  (define-assert-equivalence eq?)
  (define-assert-equivalence equal?)
  (define-assert-equivalence eqv?)
  (define-assert-equivalence fl=?)
  (define-assert-equivalence fx=?)
  (define-assert-equivalence string-ci=?)
  (define-assert-equivalence string=?)
  (define-assert-equivalence symbol=?)

  (define (report)
    (cond ((null? *failure*)
           (display "passed.\n")
           (exit))
          (else
           (for-each
            (lambda (e) (display e (current-error-port)))
            (reverse *failure*))
           (display "failed.\n")
           (exit 1))))

)
