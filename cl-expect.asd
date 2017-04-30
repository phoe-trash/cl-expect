;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:cl-expect
  (:use :common-lisp :asdf)
  (:export :s/read :s/write :create-stream))

(in-package :cl-expect)

(defsystem cl-expect
  :name "cl-expect"
  :version "0.0.1"
  :maintainer "Gábor Poczkodi"
  :author "Gábor Poczkodi"
  :licence "MIT"
  :description "Library that allows interfacing with OS programs"
  :long-description "Allows to create, read and write program streams. Requires SBCL."
  :components ((:file "cl-expect")))

