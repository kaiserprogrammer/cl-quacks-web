(defpackage :quacks-web-acceptance
  (:use :cl :fiveam :quacks-web :selenium))
(in-package :quacks-web-acceptance)


(def-suite quacks-web-acceptance)
(in-suite quacks-web-acceptance)

(test quacks-web
  (finishes
    (with-selenium-session (*selenium-session* "*firefox" (puri:parse-uri "http://localhost:8080"))
      (do-open "/")
      (is (do-is-text-present "awesome")))))

(run!)
