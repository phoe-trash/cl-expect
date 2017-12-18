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

(defun s/read (stream &key
			(format-output t)
			(expect nil)
			(timeout-in-seconds 120)
			(progress-characters 10000)
			(progress-summary 1000000)
			(waiting-characters 1000)
			(break-characters nil))
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
		       ;;(null c)
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
				  (= 0 (mod char-count progress-characters))
				  (not (eq :quiet format-output)))
			     (format t ".")
			     (when (= 0 (mod char-count progress-summary))
			       (format t "~%characters streamed so far: ~a~%" (write-to-string char-count)))))))
		   (progn
		     (when break-characters
		       (decf break-characters)
		       (when (= 0 break-characters)
			 (format t "~&DEBUG Breaking loop...~%")
			 (return)))
		     (when (and
			    (not notified)
			    (> (- (get-universal-time)
				  last-data-time)
			       timeout-in-seconds))
		       (format t "~%*** Waiting for input for more than ~a seconds, last 1000 characters: ***~%~a~%*** End of data ***~%"
			       timeout-in-seconds
			       (if (> (length retval)
				      waiting-characters)
				   (subseq retval (- (length retval)
						     waiting-characters))
				   retval))
		       (setf notified t)))))))
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

;; (defun |#-reader| (stream sub-char numarg)
;;   (declare (ignore sub-char numarg))
;;   (queue-add (read stream t t t)))

;; (set-dispatch-macro-character #\# #\ #'|#-reader|)

(defun s/read/thread (stream output &key (interactive nil))
  (setf (symbol-value output) (make-adjustable-string ""))
  (loop while (not (peek-char nil stream nil 'eof))) 
  (loop for c = (read-char-no-hang stream nil 'eof)
     until (eq 'eof c)
     do
       (when c
	 (when interactive
	   (format t "~a" c))
	 (vector-push-extend c (symbol-value output)))
       (sb-thread:thread-yield)))

(defun s/write/thread (stream input &key (interactive nil))
  (handler-case
      (progn
	(if interactive
	    (loop
	       while (open-stream-p stream)
	       for w = (read-line)       
	       do
		 (s/write stream w))
	    ;; not interactive
	    (loop
	       while (open-stream-p stream)
	       do
		 (when (> (length (symbol-value input)) 0)
		   (s/write stream (symbol-value input))
		   (setf (symbol-value input) ""))
		 (sb-thread:thread-yield))))
    (error (e) (format t "~a~%" e))))

(defun open-stream (program &optional args)
  (let* ((stream (create-stream program args))
	 (read-var (gensym))
	 (write-var (gensym)))
    (setf (symbol-value read-var) (make-adjustable-string ""))
    (setf (symbol-value write-var) "")
    (let ((read-thread (sb-thread:make-thread
			(lambda (standard-output)
			  (let ((*standard-output* standard-output))
			    (s/read/thread stream read-var :interactive nil)))
			:arguments (list *standard-output*)))
	  (write-thread (sb-thread:make-thread
			 (lambda (standard-output)
			   (let ((*standard-output* standard-output))
			     (s/write/thread stream write-var :interactive nil)))
			 :arguments (list *standard-output*))))
      (values read-var write-var stream read-thread write-thread))))

(defun open-interactive (program &optional args)
  (let* ((stream (create-stream program args))
	 (read-var (gensym))
	 (write-var (gensym))
	 (read-thread (sb-thread:make-thread
		       (lambda (standard-output)
			 (let ((*standard-output* standard-output))
			   (s/read/thread stream read-var :interactive t)))
		       :arguments (list *standard-output*)))
	 (write-thread (sb-thread:make-thread
			(lambda (standard-input)
			  (let ((*standard-input* standard-input))
			    (s/write/thread stream write-var :interactive t)))
			:arguments *standard-input*)))
    (values read-var write-var stream read-thread write-thread)))

(defun reset-output (var)
  (setf (symbol-value var) (make-adjustable-string "")))

(defun expect (var txt)
  (reset-output var)
  (let* ((txt (if (listp txt)
		  txt
		  (list txt)))
	 (match# nil))
    (loop
       for m = (setf match# (loop for s in txt thereis (cl-ppcre:scan s (symbol-value var))))
       until m
       do
	 (sb-thread:thread-yield))
    (values
     (symbol-value var)
     match#)))

(defun send (var txt)
  (setf (symbol-value var) txt))

(defmacro director (program &optional args script)
  `(multiple-value-bind (rv wv stream rt wt)
       (open-stream ,program ,args)
     (declare (ignorable stream))
     (flet ((expect (txt)
	      (expect rv txt))
	    (send (txt)
	      (send wv txt)))
       ,@script)
     (sb-thread:terminate-thread rt)
     (sb-thread:terminate-thread wt)))

