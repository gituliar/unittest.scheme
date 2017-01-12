#!/usr/bin/env scheme-program
;;;
;;; UNIT TESTING WITH TAGS
;;; 
;;; Author: Oleksandr Gituliar <oleksandr@gituliar.net>
;;;   Date: 2017-01-02
;;; 
;;; In this short essay I would like to put your attention to yet another
;;; approach for grouping unit tests and its implementation in Chez Scheme.
;;; Briefly, this approach consists in that every test case is labeled with one
;;; or several tags.  Later, test cases are grouped and executed depending on
;;; whether they were labeled with a particular tag or not. To make this
;;; description more clear, let me give you an example of such tests that we
;;; wrote for the project Fuchsia, where this idea becomes very handy.
;;; 
;;; NOTE: I made a few attempts to find anything at least similar to this idea
;;; on the Internet. Unfortunately, all of them had failed. In spite of this, I
;;; still believe that I am not the only person who had such an idea and I
;;; would appreciate if you inform me of something similar.
;;; 
;;; Fuchsia is a tool for transforming analytical differential equations. Let
;;; me avoid all the technical details and just concentrate on that it heavily
;;; depends on several algorithms for symbolic computation, like factorization
;;; of polynomials or solving linear systems of equations. You will find
;;; implementation of these algorithms in many computer algebra systems (CAS),
;;; like Maple, Mathematica, or Maxima. The problem is that almost every CAS is
;;; propriety and usually people have access only to one of them. Hence, in
;;; order to maximize a number of people who could use Fuchsia we decided to
;;; use Maxima as our default CAS, which is free and open-source.
;;; 
;;; Let us come back to our tests. At the beginning we started with two groups
;;; of tests: tagged #fast and #slow. Fast tests took a few seconds to
;;; complete. I run them after minor code changes, probably dozens times per
;;; day, mostly to ensure that nothing is broken. On the other hand, slow tests
;;; took hours to complete and were build on top of real problems which we
;;; wanted to be 100% sure that Fuchsia can cope with.
;;; 
;;; Unfortunately, Maxima did not manage to successfully deal with some more
;;; complicated examples. That fact forced us to switch between Maple and
;;; Maxima at some places in our code on request. This allows to speed up
;;; Fuchsia on machines with Maple with no loss of functionality when it is not
;;; available.  At this point, for some #slow tests which took hours to
;;; complete with Maxima we get an amazing speed up with Maple. We tagged such
;;; tests with another tag #fast-maple.
;;; 
;;; Taking that into account, for the moment we end up with four situations
;;; when we want to run our tests summarized in the following table with
;;; corresponding combinations of tags:
;;; 
;;;            Maxima             Maple
;;; 
;;;     fast   #fast      #fast or #fast-maple
;;; 
;;;     slow   #slow    #slow and not #fast-maple
;;; 
;;; In summary, it feels natural to group tests using various combinations of
;;; tags and selection rules. In some situations this could be a more elegant
;;; and handy solution comparing to explicitly defined suite cases.

(import (rnrs)
        (unittest))

(run-tests '(fast-maple)

  (define-test "fast" '(fast)
    (lambda (t)
      (assert-true? #t)))
  
  (define-test "slow" '(slow)
    (lambda (t)
      (assert-true? #t)))
  
  (let-test
    ([t #t]
     [f #f])
  
    (define-test "t #1" '()
      (assert-true? t))
  
    (define-test "f #1" '()
      (assert-true? f)))
  
  (define-test "slow fast-maple" '(slow fast-maple)
    (lambda (t)
      (assert-true? #t))))
