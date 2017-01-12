;;; A minimalistic unit testing framework for Chez Scheme.
;;;
;;; Its unique feature is the ability to select and execute tests by a set
;;; tags. It works like this: each test is defined with a set of tags which
;;; during the execution are compared with a set of active tags defined by
;;; `run-tests` function. If these sets have common tags then the test is
;;; executed, othervise it is ignored.
;;;
;;; Author:  Oleksandr Gituliar <oleksandr@gituliar.net>
;;; Version: 17.01.12

(library (unittest)
  (export assert-true? define-test let-test print-test-report run-tests)
  (import (chezscheme))

(define *active-tags* #f)
(define *tests-fail* 0)
(define *tests-pass* 0)

(define *depth* 0)


(define (assert-true? ex)
  (eq? ex #t))


(define (member?  obj ls)
    (and (member obj ls) #t))

(define (active-test? test-tags)
  (if (or (eq? *active-tags* #t)
          (null? test-tags))
      #t
      (let active-test-rec?
           ([tags test-tags])
           (if (null? tags)
               #f
               (or (member? (car tags) *active-tags*)
                   (active-test-rec? (cdr tags)))))))

(define-syntax define-test
  (syntax-rules ()
    ((_ label tags body ...)
     (if (active-test? tags)
         (run-test label tags (lambda () body ...))))))

(define (print-test-report)
  (if (> *depth* 1)
      (set! *depth* (- *depth* 1))
      (let ((tests-total (+ *tests-pass* *tests-fail*)))
       (begin
         (printf "Run ~s tests (PASS: ~s, FAIL: ~s)\n"
                 tests-total
                 *tests-pass*
                 *tests-fail*)))))

(define-syntax let-test
  (syntax-rules ()
    ((_ ([v1 e1] ...) body ...)
     (let ([v1 (delay (begin (printf "Eval ~s\n" 'v1) e1))] ...)
       (let-syntax
         ((v1 (identifier-syntax (force v1))) ...)
         body ...)))))

(define (run-test label tags test)
  (begin
    (printf "Run ~s\n" label)
    (test)
    (set! *tests-pass* (+ *tests-pass* 1))))

(define-syntax run-tests
  (syntax-rules ()
    ((_ active-tags tests ...)
     (begin
       (run-tests* active-tags)
       tests ...
       (print-test-report)))))

(define (run-tests* tags)
  (begin
    (set! *depth* (+ *depth* 1))
    (if (eq? *active-tags* #f)
        (set! *active-tags* tags))))

)
