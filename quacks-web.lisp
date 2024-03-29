(ql:quickload :quacks)
(ql:quickload :stampede)
(ql:quickload :lisperati)
(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)
(ql:quickload :silcro)

(defpackage :quacks-web
  (:use :cl
        :quacks
        :stampede
        :lisperati
        :alexandria
        :cl-ppcre
        :silcro))
(in-package :quacks-web)

(defvar *server*
  (stampede:create-http-server 8000 :worker-threads 20))

(defvar *db* (make-instance 'memory-db))

(defvar *authors* nil)
(defvar *author* nil)
(defvar *users* nil)
(defvar *user* nil)
(defvar *user-id* nil)
(defvar *author-id* nil)
(defvar *title* nil)
(defvar *inner-template* nil)

(defrenderer-with-inner-template #.(relative-file "application.html.lr") #.(relative-file ".") :match "\\.lr$")

(s-get (*server* "/authors")
       (set-session "johnny" "you're awesome")
       (let ((*authors* (get-authors *db*))
             (*title* "Authors"))
         (flush-headers)
         (write-to-client (render-authors-index))
         (response-written)))

(s-get (*server* "/authors/:id")
       (let* ((id (get-id))
              (*author* (get-author id *db*)))
         (render-authors-show)))

(s-get (*server* "/authors/new")
       (let ((cookie (get-session "johnny")))
         (let ((*title* cookie))
           (render-authors-new))))

(s-post (*server* "/authors")
        (add-author (param "name") *db*)
        (redirect-to "/authors"))

(s-get (*server* "/users/:id")
       (let ((*user* (get-user (get-id) *db*)))
         (render-users-show)))
(s-get (*server* "/login")
       (set-cookie "blub" "secrect_session")
       "fine")

(s-dir *server* #.(relative-file "public"))

(s-get (*server* "/images/:id")
       (let ((*author-id* (get-id)))
         (render-images-edit)))

(s-put (*server* "/images/:id")
       (let ((author-id (get-id))
             (url (param "url")))
         (when (and url (not (emptyp url)))
           (add-image author-id url *db*))
         (redirect-to (format nil "/authors/~a" author-id))))

(s-get (*server* "/users")
       (let ((*users* (get-users *db*)))
         (render-users-index)))

;; (s-file *server* "~/broschuere.pdf" "/public/broschuere.pdf")
