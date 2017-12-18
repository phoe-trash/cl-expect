;;;; package.lisp

(defpackage #:cl-expect
  (:use #:cl)

  (:export :create-stream
	   :s/read
	   :s/write
	   :s/execute
	   :open-interactive
	   :open-stream
	   :expect
	   :send
	   :director))

