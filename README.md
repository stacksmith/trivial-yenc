# trivial-yenc
Common lisp tools for decoding yenc files.  It uses split-sequence.

(ql:quickload :trivial-yenc)

(decode-part instream directory)

instream is a binary input stream (:element-type '(unsigned-byte 8))

directory is a path prefix for the output file

See examples test1 and test2.

test1 opens a yenc file (two roads), decodes and displays the poem from the file.

test2 decodes a multi-part binary, joystick.jpg.

```common-lisp
(defun test2 ()
  "decode multipart messages to create joystick.jpg"
  (with-open-file (in  (asdf:system-relative-pathname 'trivial-yenc "test/yenc2/00000020.ntx" )
		       :element-type '(unsigned-byte 8))
    (decode-part in (asdf:system-relative-pathname 'trivial-yenc "test/yenc2/")))
  (with-open-file (in  (asdf:system-relative-pathname 'trivial-yenc "test/yenc2/00000021.ntx" )
		       :element-type '(unsigned-byte 8))
   (decode-part in (asdf:system-relative-pathname 'trivial-yenc "test/yenc2/")))   )
```

decode-part skips message headers, parses =ybegin line.  If it's a multipart, it parses the next =ypart line.
With enough information, it yydecodes the data that follows and writes it to a file in the directory specified.
(test2) reads yenc files, but in real life you can stream the data right from the newsserver...

See:
* http://www.yenc.org

