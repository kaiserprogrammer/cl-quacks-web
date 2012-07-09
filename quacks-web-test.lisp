(defpackage :quacks-web-test
  (:use :cl :fiveam :quacks-web))
(in-package :quacks-web-test)

(def-suite quacks-web)
(in-suite quacks-web)

(test quacks-web
  (is (equal t t)))

(run!)
