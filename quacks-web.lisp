(defpackage :quacks-web
  (:use :cl :quacks :stampede :lisperati :alexandria))
(in-package :quacks-web)

(defvar *server*
  (stampede:create-server "127.0.0.1" 8080
                          (lambda (stream)
                            (handle-request stream))))

(defvar *logger* (open (relative-file "log")
                             :direction :output
                             :if-exists :append
                             :if-does-not-exist :create))

(defvar *db* (make-instance 'memory-db))

(defparameter *index-page* (compile-file-template (relative-file "index.html.lr")))
(defparameter *authors-page* (compile-file-template (relative-file "authors.html.lr")))

(defvar *authors* nil)

(defun handle-request (stream)
  (let* ((req (http-protocol-reader stream))
         (res (list (cons :version (cdr (assoc :version req)))
                    (cons :status 200)
                    (cons "Content-Type" "text/html"))))
    (write-log req)
    (http-protocol-writer
     res
     (switch ((cdr (assoc :url req)) :test 'equal)
       ("/authors" (let ((*authors* nil)) (insert-template *authors-page*)))
       (t (insert-template *index-page*)))
     stream)))

(defun write-log (args)
  (format *logger* "~w~%" args))
