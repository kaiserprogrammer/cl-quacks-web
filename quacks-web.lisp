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
  (stampede:create-server 8000
                          (lambda (stream)
                            (handle-request stream))
                          :worker-threads 1))

;; (defvar *logger* (open (relative-file "log")
;;                        :direction :output
;;                        :if-exists :append
;;                        :if-does-not-exist :create))

(defvar *db* (make-instance 'memory-db))

(defmacro defrenderer (dir)
  (let ((files (mapcar #'princ-to-string (cl-fad:list-directory (eval dir)))))
    (append (list'progn)
            (loop for file in files
               collect `(define-renderer ,file)))))

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

(defrenderer (relative-file "authors"))
(defrenderer (relative-file "users"))

(defparameter *routes* (list (list "GET")
                             (list "POST")))

(defun defroute (method reg fun)
  (let ((regex (regex-replace-all ":[^/]+" reg "([^/$]+)"))
        (params (extract-params-from-regex reg)))
    (let ((item (cons regex (cons params fun))))
      (push item (cdr (assoc method *routes* :test #'string=))))))

(defun extract-params-from-regex (reg)
  (let (params)
    (do-register-groups (param) ("/:([^/$]+)" reg)
      (push (make-keyword (string-upcase param)) params))
    (nreverse params)))

(defun call-route (req res)
  (declare (optimize (debug 3)))
  (let* ((url (cdr (assoc :url req)))
         (method (cdr (assoc :method req))))
    (loop for route in (cdr (assoc method *routes* :test #'string=))
       for (match . groups) = (multiple-value-bind (match groups)
                                  (scan-to-strings (car route) url)
                                (cons match groups))
       when match
       return (progn
                (when (not (emptyp groups))
                  (loop for value across (the simple-vector groups)
                     for key in (cadr route)
                     do (push (cons key value) (cdr (assoc :params req)))))
                (funcall (the function (cddr route)) req res)))))


(defroute "GET" "^/authors$" (lambda (req res)
                              (declare (ignorable res req))
                              (let ((*authors* (get-authors *db*)))
                                (render-authors-index))))
(defroute "GET" "^/authors/:id$" (lambda (req res)
                                  (declare (ignorable res req))
                                  (let* ((id (parse-integer (parameter :id (cdr (assoc :params req)))))
                                         (*author* (get-author id *db*)))
                                    (render-authors-show))))
(defroute "GET" "^/authors/new" (lambda (req res)
                                  (declare (ignorable req res))
                                  (render-authors-new)))
(defroute "POST" "^/authors" (lambda (req res)
                               (declare (ignorable req res))
                               (add-author (cdr  (assoc "name" (cdr (assoc :params req)) :test #'string=)) *db*)))

(defroute "GET" "^/users/:id$" (lambda (req res)
                                (declare (ignorable res req))
                                (let ((*user* (get-user (parse-integer (parameter :id (cdr (assoc :params req)))) *db*)))
                                  (render-users-show))))
(defroute "GET" "^/stylesheet/image.css$" (lambda (req res)
                                           (declare (ignorable res req))
                                           (progn
                                             (setf (cdr (assoc "Content-Type" res :test 'equal)) "text/css")
                                             (inline-file-template (relative-file "public/stylesheet/image.css")))))

(defroute "GET" "^/users$" (lambda (req res)
                            (declare (ignorable res req))
                            (let ((*users* (get-users *db*)))
                              (render-users-index))))


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
                         (cons "Content-Type" "text/html"))))
          ;; (write-log req)
          (http-protocol-writer res
                                (call-route req res)
                                stream))
      (t (e)
        (let ((res (list (cons :version (cdr (assoc :version req)))
                         (cons :status 200)
                         (cons "Content-Type" "text/plain"))))
          (http-protocol-writer res
                                (format nil "~w~%~%~a" req e)
                                stream))))))

;; (defun write-log (args)
;;   (format *logger* "~w~%" args))

(defun parameter (name parameters)
  (cdr (assoc name parameters)))
