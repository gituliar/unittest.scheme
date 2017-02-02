;;; A minimalistic unit testing framework for Chez Scheme.
;;;
;;; Its unique feature is the ability to select and execute tests by a set of
;;; tags. It works like this: each test is defined with a set of tags which
;;; during the execution are compared with a set of active tags defined by
;;; `active-tags` parameter If these sets have common tags then the test
;;; is executed, othervise it is ignored.
;;;
;;; Author:  Oleksandr Gituliar <oleksandr@gituliar.net>
;;; Version: 17.02.02

(library (unittest)
  (export active-tags assert-equal? assert-false? assert-true? define-test
          display-test-report display-costs let-test)
  (import (chezscheme))

(define active-tags (make-parameter #f))
(define assert-count (make-parameter 0))
(define failure-count (make-parameter 0))
(define success-count (make-parameter 0))

(define (assert-equal? actual expected)
  (assert-count (+ (assert-count) 1))
  (if (not (equal? actual expected))
      (raise-continuable (list 'unittest-error actual expected))))

(define (assert-true? actual)
  (assert-equal? actual #t))

(define (assert-false? actual)
  (assert-equal? actual #f))

(define (active-test? test-tags)
  (define (member?  obj ls)
    (and (member obj ls) #t))
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
             (assert-count 0)
             (with-exception-handler
               (lambda (x)
                 (if (or (not (pair? x))
                         (not (eq? (car x) 'unittest-error)))
                     (raise x))
                 (failure-count (+ (failure-count) 1))
                 (printf "~a[31mFAIL #~s~a[0m\n" #\esc (assert-count) #\esc)
                 (printf "  Expected: ~s\n" (caddr x))
                 (printf "  Got:      ~s\n" (cadr x))
                 (k #f))
               (lambda ()
                 (printf "Check ~a[35m~a~a[0m\n" #\esc label #\esc)
                 body ...
                 (success-count (+ (success-count) 1))
                 (printf "")))))))))

(define (display-cost-center cs)
  (printf
     "      CPU time:     ~,3f sec\n      Instructions: ~:D\n      Memory alloc: ~:D bytes\n"
     (let ([time (cost-center-time cs)])
       (+ (time-second time) (/ (time-nanosecond time) 10e8)))
     (cost-center-instruction-count cs)
     (cost-center-allocation-count cs)))

(define-syntax display-costs
  (syntax-rules ()
    ((_ body ...)
     (let* ([cs (make-cost-center)]
            [result (with-cost-center cpu-time cs (lambda () body ...))])
       (display-cost-center cs)
       result))))

(define (display-test-report)
  (let ((test-count (+ (success-count) (failure-count))))
    (printf "===============================================================================\n")
    (printf "Run ~s tests (PASS: ~s, FAIL: ~s)\n"
            test-count
            (success-count)
            (failure-count))))

(define-syntax let-test
  (syntax-rules ()
    ((_ ([v1 e1] ...) body ...)
     (letrec ([v1 (delay e1)] ...)
       (let-syntax
         ((v1 (identifier-syntax (force v1))) ...)
         body ...)))))
)
