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

;; (defvar *logger* (open (relative-file "log")
;;                        :direction :output
;;                        :if-exists :append
;;                        :if-does-not-exist :create))

(defvar *db* (make-instance 'memory-db))

(defvar *inner-template* nil)

(defmacro defrenderer (dir)
  (let ((files (mapcar #'princ-to-string (cl-fad:list-directory (eval dir)))))
    (append (list'progn)
            (loop for file in files
               collect `(define-renderer ,file)))))

(defmacro defrenderer-with-page (dir renderer)
  (let ((files (mapcar #'princ-to-string (cl-fad:list-directory (eval dir)))))
    (append (list'progn)
            (loop for file in files
               collect `(define-renderer-with-page ,file ,renderer)))))

(defmacro define-renderer-with-page (filename renderer)
  (let* ((file (eval filename))
         (dirs (split "/" file))
         (dir (elt dirs (- (length dirs) 2)))
         (action (elt (split "\\." (elt dirs (1- (length dirs)))) 0))
         (fname (intern (string-upcase (concatenate 'string
                                                    "render-"
                                                    dir
                                                    "-"
                                                    action)))))
    `(let ((template (compile-file-template ,file)))
       (defun ,fname
           ()
         (let ((*inner-template* template))
           (funcall ,renderer))))))

(define-renderer "/home/coder/code/cl-quacks-web/application.html.lr")

(defrenderer-with-page (relative-file "authors") #'render-cl-quacks-web-application)
(defrenderer-with-page (relative-file "users") #'render-cl-quacks-web-application)
(defrenderer-with-page (relative-file "images") #'render-cl-quacks-web-application)


(defmacro define-renderer (filename)
  (let* ((file (eval filename))
         (dirs (split "/" file))
         (dir (elt dirs (- (length dirs) 2)))
         (action (elt (split "\\." (elt dirs (1- (length dirs)))) 0))
         (fname (intern (string-upcase (concatenate 'string
                                                    "render-"
                                                    dir
                                                    "-"
                                                    action)))))
    `(let ((template (compile-file-template ,filename)))
       (defun ,fname
           ()
         (render-template template)))))

;; (defrenderer (relative-file "authors"))
;; (defrenderer (relative-file "users"))
;; (defrenderer (relative-file "images"))


(defroute *server* "GET" "^/authors$" (lambda (req res)
                              (declare (ignorable res req))
                              (let ((*authors* (get-authors *db*))
                                    (*title* "Authors"))
                                (render-authors-index))))
(defroute *server* "GET" "^/authors/:id$" (lambda (req res)
                                  (declare (ignorable res req))
                                  (let* ((id (parse-integer (parameter :id (cdr (assoc :params req)))))
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
                                (let ((*user* (get-user (parse-integer (parameter :id (cdr (assoc :params req)))) *db*)))
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
                                  (let ((*author-id* (parameter :id (cdr (assoc :params req)))))
                                    (render-images-edit))))

(defroute *server* "PUT" "^/images/:id$" (lambda (req res)
                                  (declare (ignorable res req))
                                  (let ((author-id (parse-integer (parameter :id (cdr (assoc :params req)))))
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


(defun parameter (name parameters)
  (cdr (assoc name parameters)))
