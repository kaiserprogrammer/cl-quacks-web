(defpackage :quacks-web
  (:use :cl
        :quacks
        :stampede
        :lisperati
        :alexandria
        :cl-ppcre)
  (:export
   :present-authors))
(in-package :quacks-web)

(defvar *server*
  (stampede:create-http-server 8000 :worker-threads 20))

(defvar *db* (make-instance 'memory-db))

(define-renderer "/home/coder/code/cl-quacks-web/application.html.lr")

(defrenderer-with-page "/home/coder/code/cl-quacks-web/authors" #'render-cl-quacks-web-application *inner-template*)
(defrenderer-with-page (relative-file "users") #'render-cl-quacks-web-application *inner-template*)
(defrenderer-with-page (relative-file "images") #'render-cl-quacks-web-application *inner-template*)

(defroute *server* "GET" "^/authors$" (lambda (req res)
                              (declare (ignorable res req))
                              (let ((*authors* (get-authors *db*))
                                    (*title* "Authors"))
                                (render-authors-index))))

(defroute *server* "GET" "^/authors/:id$" (lambda (req res)
                                  (declare (ignorable res req))
                                  (let* ((id (get-id req))
                                         (*author* (get-author id *db*)))
                                    (render-authors-show))))

(defroute *server* "GET" "^/authors/new" (lambda (req res)
                                  (declare (ignorable req res))
                                  (render-authors-new)))

(defroute *server* "POST" "^/authors" (lambda (req res)
                               (declare (ignorable req res)
                                        (optimize (debug 3)))
                               (add-author (cdr  (assoc "name" (cdr (assoc :params req)) :test #'string=)) *db*)
                               (redirect-to "/authors")))

(defroute *server* "GET" "^/users/:id$" (lambda (req res)
                                (declare (ignorable res req))
                                (let ((*user* (get-user (get-id req) *db*)))
                                  (render-users-show))))

(defroute *server* "GET" "^/public/quacks.css$" (lambda (req res)
                                         (declare (ignorable res req))
                                         (setf (cdr (assoc "Content-Type" res :test #'string=)) "text/css")
                                         (alexandria:read-file-into-string (relative-file "public/quacks.css"))))

(defroute *server* "GET" "^/public/quacks.js$" (lambda (req res)
                                       (declare (ignorable res req))
                                       (setf (cdr (assoc "Content-Type" res :test #'string=)) "text/javascript")
                                       (alexandria:read-file-into-string (relative-file "public/quacks.js"))))

(defroute *server* "GET" "^/images/:id$" (lambda (req res)
                                  (declare (ignorable res req))
                                  (let ((*author-id* (get-id req)))
                                    (render-images-edit))))

(defroute *server* "PUT" "^/images/:id$" (lambda (req res)
                                  (declare (ignorable res req))
                                  (let ((author-id (get-id req))
                                        (url (cdr (assoc "url" (cdr (assoc :params req)) :test #'string=))))
                                    (when (and url (not (emptyp url)))
                                      (add-image author-id url *db*))
                                    (redirect-to (format nil "/authors/~a" author-id)))))

(defroute *server* "GET" "^/users$" (lambda (req res)
                            (declare (ignorable res req))
                            (let ((*users* (get-users *db*)))
                              (render-users-index))))

(defmacro redirect-to (url)
  `(progn (setf (cdr (assoc :status res)) 302)
          (nconc res (list (cons "Location" ,url)))
          ""))

(defvar *authors* nil)
(defvar *author* nil)
(defvar *users* nil)
(defvar *user* nil)
(defvar *user-id* nil)
(defvar *author-id* nil)
(defvar *title* nil)


(defun get-id (req)
  (parse-integer (cdr (assoc :id (cdr (assoc :params req))))))
