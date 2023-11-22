(asdf:defsystem "tiny-renderer"
  :depends-on ("opticl"
               "cl-ppcre"
               "str"
               "parse-number")
  :components ((:file "wavefront-obj")
               (:file "tiny-renderer" :depends-on ("wavefront-obj"))))
