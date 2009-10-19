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
  (export add-message
          add-failure
          define-assert-equivalence
          define-assert-predicate
          assert-raise
          ;;
          assert-=
          assert-boolean=?
          assert-char=?
          assert-eq?
          assert-equal?
          assert-eqv?
          assert-string=?
          assert-symbol=?
          ;;
          assert-bound-identifier=?
          assert-bytevector=?
          assert-char-ci=?
          assert-enum-set=?
          assert-fl=?
          assert-free-identifier=?
          assert-fx=?
          assert-string-ci=?
          ;;
          assert-boolean?
          assert-char?
          assert-complex?
          assert-even?
          assert-exact?
          assert-finite?
          assert-inexact?
          assert-infinite?
          assert-integer-valued?
          assert-integer?
          assert-list?
          assert-nan?
          assert-negative?
          assert-null?
          assert-number?
          assert-odd?
          assert-pair?
          assert-positive?
          assert-procedure?
          assert-rational-valued?
          assert-rational?
          assert-real-valued?
          assert-real?
          assert-string?
          assert-symbol?
          assert-vector?
          assert-zero?
          ;;
          assert-assertion-violation?
          assert-binary-port?  
          assert-bitwise-bit-set?
          assert-buffer-mode?
          assert-bytevector?
          assert-char-alphabetic?
          assert-char-ci<=?
          assert-char-ci<?
          assert-char-ci>=?
          assert-char-ci>?
          assert-char-lower-case?
          assert-char-numeric?
          assert-char-title-case?
          assert-char-upper-case?
          assert-char-whitespace?
          assert-condition?
          assert-enum-set-member?
          assert-enum-set-subset?
          assert-eof-object?
          assert-error?
          assert-file-exists?
          assert-fixnum?
          assert-fl<=?
          assert-fl<?
          assert-fl>=?
          assert-fl>?
          assert-fleven?
          assert-flfinite?
          assert-flinfinite?
          assert-flinteger?
          assert-flnan?
          assert-flnegative?
          assert-flodd?
          assert-flonum?
          assert-flpositive?
          assert-flzero?
          assert-fx<=?
          assert-fx<?
          assert-fx>=?
          assert-fx>?
          assert-fxbit-set?
          assert-fxeven?
          assert-fxnegative?
          assert-fxodd?
          assert-fxpositive?
          assert-fxzero?
          assert-hashtable-contains?
          assert-hashtable-mutable?
          assert-hashtable?
          assert-i/o-decoding-error?
          assert-i/o-encoding-error?
          assert-i/o-error?
          assert-i/o-file-already-exists-error?
          assert-i/o-file-does-not-exist-error?
          assert-i/o-filename-error?
          assert-i/o-invalid-position-error?
          assert-i/o-port-error?
          assert-i/o-read-error?
          assert-i/o-write-error?
          assert-identifier?
          assert-implementation-restriction-violation?
          assert-input-port?
          assert-irritants-condition?
          assert-lexical-violation?
          assert-message-condition?
          assert-no-infinities-violation?
          assert-no-nans-violation?
          assert-non-continuable-violation?
          assert-output-port?
          assert-port-eof?
          assert-port-has-port-position?
          assert-port-has-set-port-position!?
          assert-port?
          assert-record-field-mutable?
          assert-record-type-descriptor?
          assert-record-type-generative?
          assert-record-type-opaque?
          assert-record-type-sealed?
          assert-record?
          assert-serious-condition?
          assert-string-ci<=?
          assert-string-ci<?
          assert-string-ci>=?
          assert-string-ci>?
          assert-syntax-violation?
          assert-textual-port?
          assert-undefined-violation?
          assert-violation?
          assert-warning?
          assert-who-condition?
          ;;
          skip-unless
          report)
  (import (rnrs))

  (define *result* #t)

  (define *messages* '())

  (define-syntax add-message
    (syntax-rules ()
      ((_ message)
       (set! *messages* (cons (string-append message "\n") *messages*)))))

  (define-syntax add-failure
    (syntax-rules ()
      ((_ message)
       (begin
         (set! *result* #f)
         (add-message message)))
      ((_ expected expr actual)
       (add-failure
        (call-with-string-output-port
         (lambda (port)
           (put-datum port expected)
           (put-string port " expected, but ")
           (put-datum port 'expr)
           (put-string port " => ")
           (put-datum port actual)))))))

  (define-syntax skip-unless
    (syntax-rules ()
      ((_ test assertion ...)
       (if test
           (begin assertion ...)
           (add-message
            (string-append "skipped:"
                           (call-with-string-output-port
                            (lambda (port)
                              (begin
                                (put-string port "\n  ")
                                (put-datum port 'assertion))
                              ...))))))))

  (define-syntax assert-equivalence
    (syntax-rules ()
      ((_ equiv expected expr)
       (let ((actual expr))
         (guard (con
                 ((assertion-violation? con)
                  (add-failure expected expr actual)))
           (assert (equiv expected actual)))))))

  (define-syntax assert-predicate
    (lambda (x)
      (syntax-case x ()
        ((_ pred expr0 expr1 ...)
         (with-syntax (((t1 ...) (generate-temporaries #'(expr1 ...))))
           #'(let ((t0 expr0)
                   (t1 expr1)
                   ...)
               (guard (con
                       ((assertion-violation? con)
                        (add-failure
                         (call-with-string-output-port
                          (lambda (port)
                            (put-datum port '(pred expr0 expr1 ...))
                            (put-string port " expected to be true, but #f"))))))
                 (assert (pred t0 t1 ...)))))))))

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

  (define-syntax define-assert-predicate
    (lambda (x)
      (syntax-case x ()
        ((k pred)
         (let ((n (string->symbol (string-append "assert-" (symbol->string (syntax->datum #'pred))))))
           (with-syntax ((name (datum->syntax #'k n)))
             #'(define-syntax name
                 (syntax-rules ()
                   ((_ expr0 expr1 (... ...))
                    (assert-predicate pred expr0 expr1 (... ...)))))))))))

  (define-syntax assert-raise
    (syntax-rules ()
      ((_ pred expr ...)
       (guard (obj
               ((pred obj))
               (else (add-failure
                      (call-with-string-output-port
                       (lambda (port)
                         (put-string port "an object satisfying ")
                         (put-datum port 'pred)
                         (put-string port " expected to be raised, but actually ")
                         (put-datum port obj))))))
         expr ...
         (add-failure
          (call-with-string-output-port
           (lambda (port)
             (put-string port "an object satisfying ")
             (put-datum port 'pred)
             (put-string port " expected to be raised, but never"))))))))

  ;; for Base
  (define-assert-equivalence =)
  (define-assert-equivalence boolean=?)
  (define-assert-equivalence char=?)
  (define-assert-equivalence eq?)
  (define-assert-equivalence equal?)
  (define-assert-equivalence eqv?)
  (define-assert-equivalence string=?)
  (define-assert-equivalence symbol=?)
  ;; for Standard Libraries
  (define-assert-equivalence bound-identifier=?)
  (define-assert-equivalence bytevector=?)
  (define-assert-equivalence char-ci=?)
  (define-assert-equivalence enum-set=?)
  (define-assert-equivalence fl=?)
  (define-assert-equivalence free-identifier=?)
  (define-assert-equivalence fx=?)
  (define-assert-equivalence string-ci=?)

  ;; for Base
  (define-assert-predicate boolean?)
  (define-assert-predicate char?)
  (define-assert-predicate complex?)
  (define-assert-predicate even?)
  (define-assert-predicate exact?)
  (define-assert-predicate finite?)
  (define-assert-predicate inexact?)
  (define-assert-predicate infinite?)
  (define-assert-predicate integer-valued?)
  (define-assert-predicate integer?)
  (define-assert-predicate list?)
  (define-assert-predicate nan?)
  (define-assert-predicate negative?)
  (define-assert-predicate null?)
  (define-assert-predicate number?)
  (define-assert-predicate odd?)
  (define-assert-predicate pair?)
  (define-assert-predicate positive?)
  (define-assert-predicate procedure?)
  (define-assert-predicate rational-valued?)
  (define-assert-predicate rational?)
  (define-assert-predicate real-valued?)
  (define-assert-predicate real?)
  (define-assert-predicate string?)
  (define-assert-predicate symbol?)
  (define-assert-predicate vector?)
  (define-assert-predicate zero?)
  ;; for Standard Libraries
  (define-assert-predicate assertion-violation?)
  (define-assert-predicate binary-port?)  
  (define-assert-predicate bitwise-bit-set?)
  (define-assert-predicate buffer-mode?)
  (define-assert-predicate bytevector?)
  (define-assert-predicate char-alphabetic?)
  (define-assert-predicate char-ci<=?)
  (define-assert-predicate char-ci<?)
  (define-assert-predicate char-ci>=?)
  (define-assert-predicate char-ci>?)
  (define-assert-predicate char-lower-case?)
  (define-assert-predicate char-numeric?)
  (define-assert-predicate char-title-case?)
  (define-assert-predicate char-upper-case?)
  (define-assert-predicate char-whitespace?)
  (define-assert-predicate condition?)
  (define-assert-predicate enum-set-member?)
  (define-assert-predicate enum-set-subset?)
  (define-assert-predicate eof-object?)
  (define-assert-predicate error?)
  (define-assert-predicate file-exists?)
  (define-assert-predicate fixnum?)
  (define-assert-predicate fl<=?)
  (define-assert-predicate fl<?)
  (define-assert-predicate fl>=?)
  (define-assert-predicate fl>?)
  (define-assert-predicate fleven?)
  (define-assert-predicate flfinite?)
  (define-assert-predicate flinfinite?)
  (define-assert-predicate flinteger?)
  (define-assert-predicate flnan?)
  (define-assert-predicate flnegative?)
  (define-assert-predicate flodd?)
  (define-assert-predicate flonum?)
  (define-assert-predicate flpositive?)
  (define-assert-predicate flzero?)
  (define-assert-predicate fx<=?)
  (define-assert-predicate fx<?)
  (define-assert-predicate fx>=?)
  (define-assert-predicate fx>?)
  (define-assert-predicate fxbit-set?)
  (define-assert-predicate fxeven?)
  (define-assert-predicate fxnegative?)
  (define-assert-predicate fxodd?)
  (define-assert-predicate fxpositive?)
  (define-assert-predicate fxzero?)
  (define-assert-predicate hashtable-contains?)
  (define-assert-predicate hashtable-mutable?)
  (define-assert-predicate hashtable?)
  (define-assert-predicate i/o-decoding-error?)
  (define-assert-predicate i/o-encoding-error?)
  (define-assert-predicate i/o-error?)
  (define-assert-predicate i/o-file-already-exists-error?)
  (define-assert-predicate i/o-file-does-not-exist-error?)
  (define-assert-predicate i/o-filename-error?)
  (define-assert-predicate i/o-invalid-position-error?)
  (define-assert-predicate i/o-port-error?)
  (define-assert-predicate i/o-read-error?)
  (define-assert-predicate i/o-write-error?)
  (define-assert-predicate identifier?)
  (define-assert-predicate implementation-restriction-violation?)
  (define-assert-predicate input-port?)
  (define-assert-predicate irritants-condition?)
  (define-assert-predicate lexical-violation?)
  (define-assert-predicate message-condition?)
  (define-assert-predicate no-infinities-violation?)
  (define-assert-predicate no-nans-violation?)
  (define-assert-predicate non-continuable-violation?)
  (define-assert-predicate output-port?)
  (define-assert-predicate port-eof?)
  (define-assert-predicate port-has-port-position?)
  (define-assert-predicate port-has-set-port-position!?)
  (define-assert-predicate port?)
  (define-assert-predicate record-field-mutable?)
  (define-assert-predicate record-type-descriptor?)
  (define-assert-predicate record-type-generative?)
  (define-assert-predicate record-type-opaque?)
  (define-assert-predicate record-type-sealed?)
  (define-assert-predicate record?)
  (define-assert-predicate serious-condition?)
  (define-assert-predicate string-ci<=?)
  (define-assert-predicate string-ci<?)
  (define-assert-predicate string-ci>=?)
  (define-assert-predicate string-ci>?)
  (define-assert-predicate syntax-violation?)
  (define-assert-predicate textual-port?)
  (define-assert-predicate undefined-violation?)
  (define-assert-predicate violation?)
  (define-assert-predicate warning?)
  (define-assert-predicate who-condition?)

  (define (report)
    (for-each
     (lambda (e) (display e (current-error-port)))
     (reverse *messages*))
    (flush-output-port (current-error-port))
    (cond (*result*
           (display "passed.\n")
           (exit))
          (else
           (display "failed.\n")
           (exit #f))))

)
