(defpackage tiny-renderer-test
  (:use :cl :tiny-renderer)
  (:local-nicknames (#:obj #:wavefront-obj))
  (:documentation "This is test examples for TinyRenderer."))

(in-package tiny-renderer-test)

(defparameter output-path "./output/"
  "The output file path of test files.")

(defparameter resource-path "./resource/"
  "The resource file path of test files.")

(time
 (with-image (image 100 100 (str:concat output-path "lesson-0.png"))
   (bound-pixel image (50 50) +red+)))

(time
 (with-image (image 100 100 (str:concat output-path "lesson-0-2.png"))
   (loop for theta from 0 below (* 20 pi) by 0.01 
         for x = (floor (+ 50 (* theta (cos theta))))
         for y = (floor (+ 50 (* theta (sin theta))))
         do (bound-pixel image (x y) +red+))))

(time
 (let ((size 5000)
       (step 10))
   (with-image (image size size (str:concat output-path "lesson-1.png"))
     (loop for i from 0 below size by step do
       (draw-line image 0 0 i size +red+))
     (loop for i from 0 below size by step do
       (draw-line image 0 size size i +green+))
     (loop for i from 0 below size by step do
       (draw-line image size size i 0 +blue+))
     (loop for i from 0 below size by step do
       (draw-line image size 0 0 i +white+)))))

(time
 (defparameter miku
   (obj:read-wavefront-obj (str:concat resource-path "miku.obj"))))

(time
 (defparameter head
   (obj:read-wavefront-obj (str:concat resource-path "african_head.obj"))))

(defun draw-line-vertex-center (image v1 v2 color)
  "Draw line using wavefront object vertex, center at image center."
  (declare (obj:vertex v1 v2))
  (opticl:with-image-bounds (height width) image
    (labels ((->x (v) (ceiling (* width  (- 0.5 (* 0.5 (obj:vertex-x v))))))
             (->y (v) (ceiling (* height (- 0.5 (* 0.5 (obj:vertex-y v)))))))
      (draw-line image (->x v1) (->y v1) (->x v2) (->y v2) color))))

(defun draw-line-vertex-bottom (image v1 v2 color)
  "Draw line using wavefront object vertex, center at image bottom."
  (declare (obj:vertex v1 v2))
  (opticl:with-image-bounds (height width) image
    (labels ((->x (v) (ceiling (* width  (- 0.5 (* 0.5 (obj:vertex-x v))))))
             (->y (v) (ceiling (* height (- 1.0 (* 0.5 (obj:vertex-y v)))))))
      (draw-line image (->x v1) (->y v1) (->x v2) (->y v2) color))))

(defun draw-obj-wireframe
    (image obj color &key (line-fn #'draw-line-vertex-center))
  "Draw `obj' on `image' by wireframe using `color'. "
  (loop for idx below (length (obj:obj-faces obj))
        for face = (obj:face-vertices (elt (obj:obj-faces obj) idx))
        do (loop for i below 3
                 for j = (mod (1+ i) 3)
                 for vi = (elt (obj:obj-vertices obj) (1- (elt face i)))
                 for vj = (elt (obj:obj-vertices obj) (1- (elt face j)))
                 do (funcall line-fn image vi vj color))))

(time
 (with-image (image 1000 1000 (str:concat output-path "lesson-1-head.png"))
   (draw-obj-wireframe image head +white+)))

(time
 (with-image (image 5000 5000 (str:concat output-path "lesson-1-miku.png"))
   (draw-obj-wireframe image miku +white+ :line-fn #'draw-line-vertex-bottom)))
