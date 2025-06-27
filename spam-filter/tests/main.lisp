(defpackage spam-filter/tests/main
  (:use :cl
        :spam-filter
        :rove))
(in-package :spam-filter/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :spam-filter)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
