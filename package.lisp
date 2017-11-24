;;;; package.lisp

(defpackage #:cl-expect
  (:use #:cl)

  (:export :create-stream
	   :s/read
	   :s/write
	   :s/execute))

