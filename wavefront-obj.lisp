(defpackage wavefront-obj
  (:use :cl)
  (:export
   ;; Wavefront Object Data Structure
   #:obj
   #:obj-vertices
   #:obj-faces
   #:obj-norms
   #:obj-textures
   #:obj-norms
   #:face
   #:face-vertices
   #:vertex
   #:vertex-x
   #:vertex-y
   #:vertex-z
   #:vertex-w

   ;; Wavefront Object Data Manipulate
   #:read-wavefront-obj
   #:read-wavefront-obj-stream
   )
  (:documentation
   "This is a simple lib for reading wavefront object file.
Currently the function is limited and the speed is kinda slow."))

(in-package wavefront-obj)

;;; Wavefront Object Data Structure 
(defstruct obj
  (vertices #() :type array)
  (faces    #() :type array)
  (textures #() :type array)
  (norms    #() :type array))

;; Vertex is a point in 3D space
(defstruct (vertex
            (:constructor mk-vertex (x y z &optional (w 1.0))))
  x y z w)

(defstruct (texture
            (:constructor mk-texture (u &optional (v 0.0) (w 0.0))))
  u v w)

(defstruct (norm
            (:constructor mk-norm (x y z)))
  x y z)

;; Face is consists of 3 (or more) vertices,
;; the vertices are referred by index (starting from 1).
;; Each vertex come with a texture and normal vector.
(defstruct (face
            (:constructor
                mk-face (vertices &optional (texture #()) (normal #()))))
  (vertices #() :type array)
  (texture  #() :type array)
  (normal   #() :type array))

;;; Parser Preproduce
;; Wavefront Object Files use `#' to comment
(defun remove-comments (line)
  "Remove comments from wavefront object files."
  (let ((comment (cl-ppcre:scan "#" line)))
    (if comment (str:substring 0 comment line) line)))

;; Wavefront Object Files use space to separate each token
(defun tokenrize (str)
  "Split input into tokens."
  (cl-ppcre:split "\\s+" (remove-comments str)))

;;; Wavefront Object Parser
(defun ->array (&rest elements)
  "Turn `elements' into array type."
  (make-array (list (length elements)) :initial-contents elements))

(defun parse-vertex (line)
  (apply #'mk-vertex
         (mapcar #'parse-number:parse-number (rest (tokenrize line)))))

(defun parse-texture (line)
  (apply #'mk-texture
         (mapcar #'parse-number:parse-number (rest (tokenrize line)))))

(defun parse-norm (line)
  (apply #'mk-norm
         (mapcar #'parse-number:parse-number (rest (tokenrize line)))))

;;; Parse wavefront object face
(defun parse-face (line)
  (labels ((parse-element-spliter (v)
             (mapcar (lambda (s)
                       (if (zerop (length s)) nil (parse-integer s)))
                     (str:split "/" v)))
           (parse-face-element (elements)
             (apply #'mapcar #'->array
                    (mapcar #'parse-element-spliter elements))))
    (apply #'mk-face (parse-face-element (rest (tokenrize line))))))

;;; User Interface
(defun read-wavefront-obj-stream (stream)
  "Read wavefront object from `stream'."
  (loop for line = (read-line stream nil)
        while line
        when (cl-ppcre:scan "^v " line)
          collect (parse-vertex line) into vertices
        when (cl-ppcre:scan "^f " line)
          collect (parse-face line) into faces
        when (cl-ppcre:scan "^vt " line)
          collect (parse-texture line) into textures
        when (cl-ppcre:scan "^vn " line)
          collect (parse-norm line) into norms
        finally (return (make-obj :vertices (apply #'->array vertices)
                                  :faces    (apply #'->array faces)
                                  :textures (apply #'->array textures)
                                  :norms    (apply #'->array norms)))))

(defun read-wavefront-obj (file)
  "Read wavefront object from `file'."
  (with-open-file (stream file)
    (read-wavefront-obj-stream stream)))
