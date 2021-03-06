;;;; trivial-yenc.lisp

(in-package #:trivial-yenc)
;; =============================================================================
;; code vector version of =yend for checking
(defvar =yend (map 'vector #'char-code "=yend"))
;; =ybegin is not needed - we process first line as a string
;; =============================================================================
;;

(defun decode-data (vec out)
  "decode bytes in vec into out stream"
  (let ((esc nil))
    (labels ((decode-byte (c)
	       (if esc
		   (progn
		     (setf esc nil)
		     (write-byte (ldb (byte 8 0) (- c 106)) out)
		     t)
		   (if (eq (char-code #\=) c)
		       (setf esc T)
		       (unless (or (eq c 10 )
				   (eq c 13 ))
			 (write-byte (ldb (byte 8 0) (- c 42)) out))))
	       t)) ;without this, every will stop.
      (every #'decode-byte vec))))


;;------------------------------------------------------------------

(defun read-bytes (in)
  (loop for b = (read-byte in)
     until (eq b 10)
     collect b))


(defun bytes-to-string (line)
  "convert vector of bytes to a string"
  (string-right-trim '(#\SPACE #\TAB #\NEWLINE #\CR)
		     (map 'string #'code-char line)))
(defun bytes-to-vec (line)
    "convert vector of bytes to a string"
    (map 'vector #'identity line))

(defun param-to-pair (param)
  "convert string x=y to (x . y), converting y to a number if possible"
  (let* ((l (split-sequence #\= param))
;	 (sym   (intern (string-upcase (first l))))
	 (ival (parse-integer (second l) :junk-allowed t)))
    (cons (first l)
	  (or ival (second l)))))

;; =============================================================================
(defun decode-line (in out)
  "decode a line of encoded data from in and output to out"
  (let ((vec (bytes-to-vec (read-bytes in) )))
     (if (every #'= =yend vec) 
	nil
	(decode-data vec out)))
)
(defun decode-lines (in out)
  (loop
     while (decode-line in out)))


;; This is rather bogus, and relies on single spaces!  Total crap!  Fail!
(defun header-prim (str start end)
  "parse a simple header consisting of pure name=value pairs"
  (let ((result (split-sequence:split-sequence
		 #\SPACE str :start start :end end )))
    (map 'list #'param-to-pair result)))

(defun header-ybegin- (str)
  "read a parameter line starting with =ybegin; return an alist."
  ;; in real live the name parameter can contain spaces, so parse it separately
  (and (> (length str) 7)
       (string= "=ybegin" str :end2 7)
       (let* ((end (search " name=" str)) ;; a little bogus, isn't it?
	      (result (header-prim str 8 end))) ;8 beign past ybegin...
	 (cons (cons "name" (subseq str (+ 6 end))) ;(length " name="
	       result))))

(defun header-ybegin (in)
  (let ((str (bytes-to-string (read-bytes in))  ))
    (header-ybegin- str)))

(defun header-ypart- (str)
  (and (> (length str) 6)
       (string= "=ypart" str :end2 6)
       (header-prim str 7 (length str))))

(defun header-ypart (in)
  (let ((str (bytes-to-string (read-bytes in))  ))
    (header-ypart- str)))

;; =============================================================================
(defun decode-part (in path)
  "decode stream in and write file in path directory.  Return part number or nil"
  ;;skip message headers
  (let* ((line1 (loop for h = (header-ybegin in)
		   until h
		   finally (return h)))
	 (part (cdr (assoc "part" line1 :test #'string=)))
	 (name (cdr (assoc "name" line1 :test #'string=)))
	 (begin 0)) ;for single-part messages
;    (print line1)
    (when part
     
      (let ((line2 (header-ypart in)))
	(setf begin (1- (cdr (assoc "begin" line2 :test #'string=))))
;	(print line2)
	;(print (assoc "end" line2 :test #'string=))
	))
    (format t "name is [~A]~%"  (merge-pathnames path name ) )
    (with-open-file (out (merge-pathnames path name )
			   :element-type '(unsigned-byte 8)
			   :direction :output
			   :if-does-not-exist :create
			   :if-exists :overwrite)
	
	(file-position out begin)
	(format t "Will write file \"~A\" at location ~A~%" name begin)

	(decode-lines in  out)
	)
    part))

(defun test1 ()
  "decode two-roads.yenc into two-roads.out; load the output file and print to screen."
  (with-open-file (in  (asdf:system-relative-pathname 'trivial-yenc "test/two-roads.ync" ):element-type '(unsigned-byte 8))
    (with-open-file (out (asdf:system-relative-pathname 'trivial-yenc
							"test/two-roads.out" )
			 :element-type '(unsigned-byte 8)
			 :direction :output
			 :if-does-not-exist :create
			 :if-exists :supersede)
      
      (and (prog1 (print (header in "=ybegin")) (terpri))
	   (print (decode-lines in  out)))))
  (with-open-file (in  (asdf:system-relative-pathname 'trivial-yenc "test/two-roads.out" ))
    (loop for line = (read-line in nil)
         while line do (format t "~a~%" line))))


(defun test2 ()
  "decode a multi-part binary from two files."
  (with-open-file (in  (asdf:system-relative-pathname 'trivial-yenc "test/yenc2/00000020.ntx" )
		       :element-type '(unsigned-byte 8))
      (decode-part in (asdf:system-relative-pathname 'trivial-yenc "test/yenc2/")))
  (with-open-file (in  (asdf:system-relative-pathname 'trivial-yenc "test/yenc2/00000021.ntx" )
		       :element-type '(unsigned-byte 8))
    (decode-part in (asdf:system-relative-pathname 'trivial-yenc "test/yenc2/")   
		   )))
