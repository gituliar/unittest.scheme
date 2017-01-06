;;; This file contains implementation of the unit testing with tests.  For more
;;; details see 'unittest_test.ss' file.
;;;
;;; Author: Oleksandr Gituliar <oleksandr@gituliar.net>
;;;   Date: 2017-01-02

(library (unittest)
  (export define-test assert-eq? assert-true? run-tests)
  (import (chezscheme))

(define *tests* '())
(define *tests-failed* 0)
(define *tests-passed* 0)


(define (member?  obj ls)
    (and (member obj ls) #t))

(define for-each-apply
  (lambda (f ls)
    (for-each (lambda (x) (apply f x)) ls)))

(define (assert-true? expr)
  (eq? expr #t))

(define assert-eq?
  (lambda (a b)
    (eq? a b)))

(define define-test
  (lambda (label tags test)
    (set! *tests* (cons (list label tags test) *tests*))))

(define (select-tests-by-tags tests tags)
  ;; This routine selects a list of tests, out of a given list `tests',
  ;; whose tags are members of a given list `tags'.
  '())

(define run-tests
  (lambda (run-tags)
    (printf "Total tests: ~s\n" (length *tests*))
    (printf "Selected tags: ~s\n" run-tags)
    (letrec ([tests (reverse *tests*)]
             [accepted? (lambda (tags)
                          (if (null? run-tags)
                              #t
                              (if (null? tags)
                                  #f
                                  (or (member? (car tags) run-tags)
                                      (accepted? (cdr tags))))))])
      (printf "*tests*: ~s\n" tests)
      (for-each-apply
        (lambda (label tags tf)
          (if (accepted? tags)
              (printf "Running ~s\n" label)))
        tests))))

)
