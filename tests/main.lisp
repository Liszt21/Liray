(defpackage liray/tests
  (:use :cl :fiveam :liray))

(in-package :liray/tests)

(def-suite :liray)

(in-suite :liray)

(test (test-5am :compile-at :run-time)
  (is (equal 1 1))
  (is (not (equal 2 1))))
