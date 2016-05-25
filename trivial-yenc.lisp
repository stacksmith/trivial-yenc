;;;; trivial-yenc.lisp

(in-package #:trivial-yenc)
;; =============================================================================
;; code vector version of =yend for checking
(defvar =yend (map 'vector #'char-code "=yend"))
;; =ybegin is not needed - we process first line as a string
;; =============================================================================
;;
(defun decode-data (vec out)
  (let ((esc nil))
    (loop for c across vec do
	 (if esc
	     (progn
	       (setf esc nil)
	       (write-byte (ldb (byte 8 0) (- c 64)) out))
	     (if (eq (char-code #\=) c)
		 (setf esc T)
		 (write-byte (ldb (byte 8 0) (- c 42)) out)
		 ))))
  t)


;;------------------------------------------------------------------

(defun read-bytes (in)
  (loop for b = (read-byte in)
     until (eq b 10)
     collect b))


(defun bytes-to-string (line)
  "convert vector of bytes to a string"
  (map 'string #'code-char line))

(defun bytes-to-vec (line)
    "convert vector of bytes to a string"
  (map 'vector #'identity line)
  )
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

(defun header (in magic)
  "read parameter line starting with magic string, return an alist"
  (let ((result (split-sequence:split-sequence
		   #\SPACE (bytes-to-string (read-bytes in)))))
      (and (string= (first result) magic)
	   (map 'list #'param-to-pair (cdr  result)))))
;; =============================================================================
(defun decode-part (in path)
  (let* ((line1 (header in "=ybegin"))
	 (part (cdr (assoc "part" line1 :test #'string=)))
	 (name (cdr (assoc "name" line1 :test #'string=)))
	 )
    (with-open-file (out (merge-pathnames path name )
			 :element-type '(unsigned-byte 8)
			 :direction :output
			 :if-does-not-exist :create
			 :if-exists :overwrite)
      (when part
	(let* ((line2 (header in "=ypart"))
	      (begin (cdr (assoc "")))))
	   )
     )) 
)

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
	   (decode-lines in  out))))
  (with-open-file (in (asdf:system-relative-pathname 'trivial-yenc "test/two-roads.out" ))
    (loop for line = (read-line in nil)
       while line do (format t "~a~%" line))))

(defun test2 ()
  "decode two-roads.yenc into two-roads.out; load the output file and print to screen."
  (with-open-file (in  (asdf:system-relative-pathname 'trivial-yenc "test/yenc2/00000020.ntx" )
		       :element-type '(unsigned-byte 8))
)
  (with-open-file (in (asdf:system-relative-pathname 'trivial-yenc "test/two-roads.out" ))
    (loop for line = (read-line in nil)
         while line do (format t "~a~%" line))))
