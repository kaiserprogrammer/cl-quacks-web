(defpackage :quacks-web
  (:use :cl :quacks :stampede :lisperati :alexandria)
  (:export
   :present-authors))
(in-package :quacks-web)

(defvar *server*
  (stampede:create-server "127.0.0.1" 8080
                          (lambda (stream)
                            (handle-request stream))
                          :worker-threads 1))

(defvar *logger* (open (relative-file "log")
                             :direction :output
                             :if-exists :append
                             :if-does-not-exist :create))

(defvar *db* (make-instance 'memory-db))

(defparameter *authors-template*
  (compile-file-template (relative-file "authors.html.lr")))
(defparameter *author-template*
  (compile-file-template (relative-file "author.html.lr")))
(defparameter *users-template*
  (compile-file-template (relative-file "users.html.lr")))
(defparameter *user-template*
  (compile-file-template (relative-file "user.html.lr")))

(defvar *authors* nil)
(defvar *author* nil)
(defvar *users* nil)
(defvar *user* nil)
(defvar *user-id* nil)

(defun handle-request (stream)
  (let ((req (http-protocol-reader stream)))
   (handler-case
       (let ((res (list (cons :version (cdr (assoc :version req)))
                        (cons :status 200)
                        (cons "Content-Type" "text/html")))
             (parameters (cdr (assoc :parameters req))))
         (write-log req)
         (http-protocol-writer
          res
          (switch ((cdr (assoc :url req)) :test 'equal)
            ("/authors" (let ((*authors* (get-authors *db*)))
                          (render-template *authors-template*)))
            ("/author/:id" (let ((*author* (get-author (parameter :id parameters) *db*)))
                              (render-template *author-template*)))
            ("/users" (let ((*users* (get-users *db*)))
                        (render-template *users-template*)))
            ("/user/:id" (let ((*user* (get-user (parameter :id parameters) *db*)))
                           (render-template *user-template*)))
            ("/stylesheet/image.css" (progn
                                       (setf (cdr (assoc "Content-Type" res :test 'equal)) "text/css")
                                       (inline-file-template (relative-file "public/stylesheet/image.css"))))
            (t (progn
                 (setf (cdr (assoc :status res)) 404)
                 "404 Not Found")))
          stream))
     (t (e)
       (let* ((res (list (cons :version (cdr (assoc :version req)))
                         (cons :status 200)
                         (cons "Content-Type" "text/plain"))))
         (http-protocol-writer
          res
          (format nil "~w~%~%~a" req e)
          stream))))))

(defun write-log (args)
  (format *logger* "~w~%" args))

(defun parameter (name parameters)
  (cdr (assoc name parameters)))
