#|
  This file is a part of towngen project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2019-2020
|#
(in-package :cl-user)
(defpackage towngen-asd
  (:use :cl :asdf))
(in-package :towngen-asd)

(defsystem towngen
  :version "0.1"
  :author ""
  :license ""
  :depends-on (alexandria cmu-infix ppath)
  :components ((:file "package")
               (:file "geometry")
               (:file "arc")
               (:file "btree")
               (:file "circular-list")
               (:file "prio-queue")
               (:file "voronoi"))
  :description "")
;;  :in-order-to ((test-op (test-op towngen-test))))
