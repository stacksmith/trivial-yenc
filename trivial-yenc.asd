;;;; trivial-yenc.asd

(asdf:defsystem #:trivial-yenc
  :description "Decode yenc file to a binary file"
  :author "Stacksmith <stack@apple2.x10.mx>"
  :license "MIT"
  :depends on (#:split-sequence)
  :serial t
  :components ((:file "package")
               (:file "trivial-yenc")))

