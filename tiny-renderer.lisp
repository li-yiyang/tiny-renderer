(defpackage tiny-renderer
  (:use :cl)
  (:export
   ;; some common colors 
   #:+red+
   #:+green+
   #:+blue+
   #:+white+

   ;; file I/O
   #:with-image
   
   ;; some lowlevel render functions/marcos
   #:bound-pixel
   #:draw-line
   )
  (:local-nicknames (#:obj #:wavefront-obj)))

(in-package tiny-renderer)

;;; Define some common colors
(defparameter +red+   '(255 0   0))
(defparameter +green+ '(0   255 0))
(defparameter +blue+  '(0   0   255))
(defparameter +white+ '(255 255 255))

;;; Some helper functions
(defmacro let-values* (bindings &body body)
  "Allows you to write something like:

(let-values* (((x y) (values 1 2)
              (z     3)))
  (+ x y z)) ;; => 6"
  (labels ((expand (bindings)
             (if (null bindings)
                 (cons 'progn body)
                 (let ((var (first  (car bindings)))
                       (exp (rest (car bindings))))
                   `(multiple-value-bind ,(if (atom var) (list var) var)
                        ,(cons 'progn exp)
                      ,(expand (cdr bindings)))))))
    (expand bindings)))

;;; Make new images
(defmacro with-image
    ((image width height image-path &key (overwrite t)) &body body)
  "Make a `image' with size `width' x `height', saved to `path'. 
If `overwrite' ignore existing file, otherwise, read into memory."
  (alexandria:with-gensyms (type w h path)
    `(let* ((,w ,width)
            (,h ,height)
            (,path ,image-path)
            (,type
              (alexandria:make-keyword
               (string-upcase (pathname-type ,path))))
            (,image
              ,(if overwrite
                   `(opticl:make-8-bit-rgb-image ,w ,h)
                   `(ecase ,type
                      ((:jpeg :jpg) (opticl:read-jpeg-file ,path))
                      (:png         (opticl:read-png-file  ,path))))))
       (progn ,@body)
       (ecase ,type
         ((:jpeg :jpg) (opticl:write-jpeg-file ,path ,image))
         (:png         (opticl:write-png-file  ,path ,image)))
       ,path)))

;;; Drawing pixels
(defmacro pixel (image (x y) &optional color)
  "Read/Set (if with RGB `color') for pixel at `x', `y' of `image'.

The color could be a list like (R G B), or the color could be a 
list variable name.

Please note that this does not check the boundary condition, 
so drawing with pixel position overflow will break the program. "
  (if color
      `(setf
        (opticl:pixel ,image ,y ,x)
        ,(if (listp color)
             `(values ,@color)
             `(apply #'values ,color)))
      `(opticl:pixel ,image ,y ,x)))

(defmacro bound-pixel (image (x y) &optional color)
  "Read/Set (if with RGB `color') for pixel at `x', `y' of `image'.
Same with `pixel' but came with boundary check."
  (alexandria:with-gensyms (height width)
    `(opticl:with-image-bounds (,height ,width) ,image
       (if (and (> ,x 0) (< ,x ,height) (> ,y 0) (< ,y ,width))
           ,(if color
                `(pixel ,image (,x ,y) ,color)
                `(opticl:pixel ,image ,y ,x))))))

;;; Draw line
(defun line-within-rectangle-p (w h x1 y1 x2 y2)
  "Test if line from (x1 y1) to (x2 y2) is within the rectangle (w h).
Return values are: within-rectangle-p x1 y1 x2 y2."
  ;; Trivial Test: test if the line go cross the rectangle
  (when (not (or (and (< x1 0) (< x2 0)) (and (>= x1 w) (>= x2 w))
                 (and (< y1 0) (< y2 0)) (and (>= y1 h) (>= y2 h))))
    (let ((kx (- x2 x1))
          (ky (- y2 y1)))
      ;; Calculate intersection point if a point is outside of the rectangle
      (let-values*
          (((xa ya)
            (cond ((<  x1 0)
                   (values 0 (+ y1 (floor (* ky (- x1)) kx))))
                  ((>= x1 w)
                   (values (1- w) (+ y1 (floor (* ky (- w 1 x1)) kx))))
                  ((<  y1 0)
                   (values (+ x1 (floor (* kx (- y1)) ky)) 0))
                  ((>= y1 h)
                   (values (+ x1 (floor (* kx (- h 1 y1)) ky)) (1- h)))
                  (t (values x1 y1))))
           ((xb yb)
            (cond ((<  x2 0)
                   (values 0 (+ y2 (floor (* ky (- x2)) kx))))
                  ((>= x2 w)
                   (values (1- w) (+ y2 (floor (* ky (- w 1 x2)) kx))))
                  ((<  y2 0)
                   (values (+ x2 (floor (* kx (- y2)) ky)) 0))
                  ((>= y2 h)
                   (values (+ x2 (floor (* kx (- h 1 y2)) ky)) (1- h)))
                  (t (values x2 y2)))))
        ;; Make sure intersection point is within the rectangle
        (when (and (>= xa 0) (< xa w) (>= ya 0) (< ya h)
                   (>= xb 0) (< xb w) (>= yb 0) (< yb h))
          (values t xa ya xb yb))))))

(defun reorder-line-points (xa ya xb yb)
  "Reorder the line points to make xa < xb.
If the line is too steep, swap x-y coordinates.
Return values are steep xa ya xb yb."
  (let ((steep (< (abs (- xb xa)) (abs (- yb ya)))))
    (if steep
        (if (> ya yb)
            (values steep yb xb ya xa)
            (values steep ya xa yb xb))
        (if (> xa xb)
            (values steep xb yb xa ya)
            (values steep xa ya xb yb)))))

(defun draw--line (image steep a1 b1 a2 b2 color)
  "Draw line on `image' from (x1 y1) (x2 y2) with `color'.
If `steep' reverse x-y coordinates.
NOTE: This method should not be called alone."
  (let* ((dx (- a2 a1)) (dy (- b2 b1))
         (err 0) (derr (* (abs dy) 2))
         (y b1) (delta-y (if (> b2 b1) 1 -1)))
    (loop for x from a1 to a2
          do (incf err derr)
          if steep
            do (pixel image (y x) color)
          else do (pixel image (x y) color)
          if (> err dx)
            do (incf y delta-y)
            and do (decf err (* 2 dx)))))

(defun draw-line (image x1 y1 x2 y2 color)
  "Draw line on `image' from (x1 y1) (x2 y2) with `color'."
  (opticl:with-image-bounds (height width) image
    (let-values* (((in-rectangle-p a1 b1 a2 b2)
                   (line-within-rectangle-p width height x1 y1 x2 y2)))
      (when in-rectangle-p
        (let-values* (((steep xa ya xb yb)
                       (reorder-line-points a1 b1 a2 b2)))
          (draw--line image steep xa ya xb yb color))))))

(defun ->barycentric-coordinate (x y x1 y1 x2 y2 x3 y3)
  "Trun Cartesian coordinate (x y) into Barycentric coordinate."
  ;; See: https://en.wikipedia.org/wiki/Barycentric_coordinate_system#Conversion_between_barycentric_and_Cartesian_coordinates
  (let ((det-T (- (* (- x1 x3) (- y2 y3))
                  (* (- x2 x3) (- y1 y3)))))
    (if (zerop det-T)
        (values -1 0 0)
        (let ((l1 (/ (+ (* (- y2 y3) (- x x3)) (* (- x3 x2) (- y y3))) det-T))
              (l2 (/ (+ (* (- y3 y1) (- x x3)) (* (- x1 x3) (- y y3))) det-T)))
          (values l1 l2 (- 1 l1 l2))))))

(defun point-within-triangle-p (x y x1 y1 x2 y2 x3 y3)
  "Test if point (x y) is within triangle ((x1 y1) (x2 y2) (x3 y3))."
  (let-values* (((l1 l2 l3) (->barycentric-coordinate x y x1 y1 x2 y2 x3 y3)))
    (not (or (< l1 0) (< l2 0) (< l3 0)))))

(defun draw-triangle (image x1 y1 x2 y2 x3 y3 color)
  "Draw triangle ((x1 y1) (x2 y2) (x3 y3)) on `image' with `color'."
  (opticl:with-image-bounds (height width) image
    ;; Make boundary box
    (let-values* (((x-min x-max y-min y-max)
                   (values (min 0 x1 x2 x3) (max (1- width) x1 x2 x3)
                           (min 0 y1 y2 y3) (max (1- height) y1 y2 y3))))
      (loop for x from x-min to x-max do
        (loop for y from y-min to y-max
              if (point-within-triangle-p x y x1 y1 x2 y2 x3 y3)
                do (bpixel image (x y) color))))))
