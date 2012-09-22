(cl:defpackage :quacks-web-system
  (:use :cl :asdf))
(cl:in-package :quacks-web-system)

(defsystem :quacks-web
  :version "0.1"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :description "A Webified version of the quacks system"
  :depends-on (:stampede
               :lisperati
               :quacks
               :silcro
               :alexandria
               :cl-ppcre
               :cl-fad))
