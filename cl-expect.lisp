;;;; cl-expect.lisp

(in-package #:cl-expect)

;;; "cl-expect" goes here. Hacks and glory await!

;; use this to run programs. Returns a stream which can be used to manipulate the program.
(defun program-stream (program &optional args)
  (let ((process (sb-ext:run-program program args
				     :input :stream
				     :output :stream
				     :wait nil
				     :search t)))
    (when process
      (values
       (make-two-way-stream (sb-ext:process-output process)
			    (sb-ext:process-input process))
       process))))

;; collect all output and optionally print it to the screen.
(defun read-all (stream &key format-output)
  (let ((retval nil))
    (loop for line = (read-line stream nil 'eof)
       until (eq line 'eof)
       do
	 (setf retval (append retval (list line)))
	 (when format-output
	   (format t "~d~%" line)
	   (force-output nil)))
    retval))

;; following is from Rainer Joswig: http://stackoverflow.com/questions/18045842/appending-character-to-string-in-common-lisp
(defun make-adjustable-string (s)
  (make-array (length s)
	      :fill-pointer (length s)
	      :adjustable t
	      :initial-contents s
	      :element-type (array-element-type s)))

(defun s/read (stream &key (format-output t) (expect nil) (timeout-in-seconds 120))
  "format-output: t/nil/:quiet
   format-output controls what to display during execution, t: everything, :quiet: nothing, nil: nothing but progress
   expect: nil/list
   if expect is a list, then list elements (strings) are checked against last output so that appropriate action can be taken."
  ;; wait until there is some data to read
  (loop while (not (peek-char nil stream nil 'eof)))
  (let ((retval (make-adjustable-string ""))
	(last-data-time (get-universal-time))
	(notified nil)
	;; (maxlen (if expect
	;; 	    (apply #'max (loop for e in expect collect (length (car e))))
	;; 	    nil))
	(char-count 0))
    (if (and (listp expect)
	     ;; (not (null expect))
	     )
	;;expect is a non-null list
	(progn
	  (loop for c = (read-char-no-hang stream nil 'eof)
	     until (or (eq 'eof c)
		       ;; (null expect)
		       (some #'(lambda (x)
				 (eq t x))
			     (loop for ex in expect
				collect
				  (let ((l-r (length retval))
					(l-e (length ex)))
				    (and
				     (>= l-r l-e)
				     (equal ex (subseq retval (- (length retval)
								 (length ex)))))))))
	     do
	       (if c
		   (progn
		     (incf char-count)
		     (setf last-data-time (get-universal-time))
		     (setf notified nil)
		     (vector-push-extend c retval)
		     
		     ;; this has been temporarily disabled
		     ;; (when (and
		     ;; 	    discard-result
		     ;; 	    maxlen)
		     ;;   (setf retval (last retval maxlen)))
		     (if (and
			  format-output
			  (not (eq :quiet format-output))
			  (not (eq c #\Backspace)))
			 (format t "~a" c)
			 (progn
			   (when (and
				  (= 0 (mod char-count 10000))
				  (not (eq :quiet format-output)))
			     (format t ".")
			     (when (= 0 (mod char-count 1000000))
			       (format t "~%characters streamed so far: ~a~%" (write-to-string char-count)))))))
		   (when (and
			  (not notified)
			  (> (- (get-universal-time)
				last-data-time)
			     timeout-in-seconds))
		     (format t "~%*** Waiting for input for more than ~a seconds, last 1000 characters: ***~%~a~%*** End of data ***~%"
			     timeout-in-seconds
			     (if (> (length retval)
				    1000)
				 (subseq retval (- (length retval)
						   1000))
				 retval))
		     (setf notified t))))))
    (finish-output nil)
    (values
     retval
     ;; (coerce retval 'string)
     (let ((match nil))
       (loop for ex in expect do
	    (when
		(let ((l-r (length retval))
		      (l-e (length ex)))
		  (when (and
			 (>= l-r l-e))
		    (equal ex (subseq retval (- (length retval)
						(length ex))))))
	      (setf match ex)))
       match))))

(defun s/write (stream txt &key (format-output nil))
  (format stream "~a~%" txt)
  (when format-output
    (format t "~a~%" txt)
    (finish-output nil))
  (finish-output stream))

(defun create-stream (program &optional args)
  (program-stream "unbuffer" (append (list "-p" program)
				     args)))

(defmacro with-program-stream (stream-name command args
			       &body body)
  `(let ((,stream-name (create-stream ,command ,args)))
     ,@body))

(defun s/execute (program &optional args)
  (let ((s (create-stream program args)))
    (s/read s :format-output t)))

;; run program and get output
(defun execute (program visible-output-p &optional args)
  (let ((stream (program-stream program args)))
    (finish-output stream)
    (read-all stream :format-output visible-output-p)))

;; s/read and s/write conscise functions
(defun s/read/verbose (stream)
  (s/read stream :format-output t))
(defun s/read/quiet (stream)
  (s/read stream :format-output nil))
(defun s/read/expect (stream expect &optional (verbose nil))
  (s/read stream :format-output verbose :expect expect))
(defun s/write/verbose (stream txt)
  (s/write stream txt :format-output t))
