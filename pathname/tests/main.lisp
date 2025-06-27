(defpackage pathname/tests/main
  (:use :cl
        :pathname
        :rove))
(in-package :pathname/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :pathname)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
