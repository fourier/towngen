;;; package.lisp --- package definition

;; Copyright (C) 2020 Alexey Veretennikov
;;
;; Author: Alexey Veretennikov <alexey dot veretennikov at gmail dot com>

(in-package :cl-user)

(defpackage town-gen
  (:use :cl :alexandria #+:lispworks :capi))

