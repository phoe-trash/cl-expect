;;;; cl-expect.asd

(asdf:defsystem #:cl-expect
  :description "Common Lisp library that allows interfacing with OS programs. Requires SBCL and unbuffer."
  :author "Gábor Poczkodi <hajovonta@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "cl-expect")))

