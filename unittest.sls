;;; A minimalistic unit testing framework for Chez Scheme.
;;;
;;; Its unique feature is the ability to select and execute tests by a set of
;;; tags. It works like this: each test is defined with a set of tags which
;;; during the execution are compared with a set of active tags defined by
;;; `active-tags` parameter If these sets have common tags then the test
;;; is executed, othervise it is ignored.
;;;
;;; Author:  Oleksandr Gituliar <oleksandr@gituliar.net>
;;; Version: 17.01.12

(library (unittest)
  (export assert-eq? assert-eqv? assert-equal? assert-true? define-test let-test
          print-test-report active-tags)
  (import (chezscheme))

(define active-tags (make-parameter #f))
(define failure-count (make-parameter 0))
(define success-count (make-parameter 0))


(define (true? x)
  (eq? x #t))

(define (make-assert check)
  (lambda (arg . args)
    (if (not (apply check arg args))
        (raise-continuable 'error))))

(define assert-eq?
  (make-assert eq?))

(define assert-eqv?
  (make-assert eqv?))

(define assert-equal?
  (make-assert equal?))

(define assert-true?
  (make-assert true?))


(define (member?  obj ls)
    (and (member obj ls) #t))

(define (active-test? test-tags)
  (cond ((eq? (active-tags) #f)
         #f)
        ((or (eq? (active-tags) #t)
              (null? test-tags))
         #t)
        (else
         (let active-test-rec?
              ([tags test-tags])
              (if (null? tags)
                  #f
                  (or (member? (car tags) (active-tags))
                      (active-test-rec? (cdr tags))))))))

(define-syntax define-test
  (syntax-rules ()
    ((_ label tags body ...)
     (if (active-test? (quote tags))
         (call/cc
           (lambda (k)
             (with-exception-handler
               (lambda (x)
                 (failure-count (+ (failure-count) 1))
                 (printf "FAIL\n")
                 (k #f))
               (lambda ()
                 (printf "Run ~s\n" label)
                 body ...
                 (success-count (+ (success-count) 1))
                 (printf "")))))))))


(define (print-test-report)
  (let ((test-count (+ (success-count) (failure-count))))
   (begin
     (printf "===============================================================================\n")
     (printf "Run ~s tests (PASS: ~s, FAIL: ~s)\n"
             test-count
             (success-count)
             (failure-count)))))

(define-syntax let-test
  (syntax-rules ()
    ((_ ([v1 e1] ...) body ...)
     (let ([v1 (delay e1)] ...)
       (let-syntax
         ((v1 (identifier-syntax (force v1))) ...)
         body ...)))))
)
