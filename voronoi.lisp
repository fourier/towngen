(in-package :cl-user)
(defpackage town-gen
  (:use :cl :alexandria))

(in-package town-gen)

(defparameter *point-precision* 0.00000001
  "Precision for considering 2 points equal")

(defstruct point
  (x 0.0 :type float)
  (y 0.0 :type float))


(defclass triangle ()
  ((p1 :initarg :p1 :type point)
   (p2 :initarg :p2 :type point)
   (p3 :initarg :p3 :type point)
   (c  :type point :documentation "Circumncenter of the triangle")
   (r  :type float :documentation "Radius of the circumcircle")))


(defmethod distance ((p1 point) (p2 point))
  (sqrt
   (+ 
    (expt (- (point-x p1) (point-x p2)) 2)
    (expt (- (point-y p1) (point-y p2)) 2))))


(defmethod equals ((p1 point) (p2 point))
  (and (< (abs (- (point-x p1) (point-x p2))) *point-precision*)
       (< (abs (- (point-y p1) (point-y p2))) *point-precision*)))


(defmethod initialize-instance :after ((triangle triangle)
                                       &key &allow-other-keys)
  (with-slots (p1 p2 p3 c r) triangle
    (let ((x1 (point-x p1)) (y1 (point-y p1))
          (x2 (point-x p2)) (y2 (point-y p2))
          (x3 (point-x p3)) (y3 (point-y p3))
          (tmp (copy-structure p2)))
      ;; calculate the determinant for points orientation,
      ;; to see if they are oriented counter-clockwise or clockwise
      ;; | x1 y1 1 |
      ;; | x2 y2 1 | > 0 means clockwise
      ;; | x3 y3 1 |
      ;; = x1 y2 - x1 y3 - x2 y1 + x2 y3 + x3 y1 - x3 y2
      (let ((det
              (- 
               (+ (* x1 y2) (* x2 y3) (* x3 y1))
               (+ (* x1 y3) (* x2 y1) (* x3 y2)))))
        ;; we need counter-clockwise triangles (CCW)
        (when (> det 0)
          (setf p2 (copy-structure p3)
                p3 tmp))
        ;; now let's calculate circumcenter of a circle
        ;; and its radious
        (multiple-value-setq (c r)
          (circumcenter triangle))))))


(defmethod print-object ((triangle triangle) out)
  (with-slots (p1 p2 p3 c r) triangle
    (format out "triangle~%p1 ~a~%p2 ~a~%p3 ~a~%circumcenter ~a radius ~a~%"
            p1 p2 p3 c r)))
          

(defmethod has-edge ((triangle triangle) (a point) (b point))
  "Determine if the points a and b are the edge of the
given triangle"
  (with-slots (p1 p2 p3) triangle
    (or (and (equals p1 a) (equals p2 b))
        (and (equals p2 a) (equals p1 b))
        (and (equals p2 a) (equals p3 b))
        (and (equals p3 a) (equals p2 b))
        (and (equals p3 a) (equals p1 b))
        (and (equals p1 a) (equals p3 b)))))

(defmethod circumcenter ((triangle triangle))
  "Circumcenter of the triangle.
See https://en.wikipedia.org/wiki/Circumscribed_circle#Cartesian_coordinates_2
for details.
Return values (circumcenter, radius)"
  (with-slots (p1 p2 p3 c r) triangle
    (let* ((x1 (point-x p1)) (y1 (point-y p1))
           (x2 (point-x p2)) (y2 (point-y p2))
           (x3 (point-x p3)) (y3 (point-y p3))
           (d (* 2
                 (+
                  (* x1 (- y2 y3))
                  (* x2 (- y3 y1))
                  (* x3 (- y1 y2)))))
           (cx (/
                (+
                 (* (+ (expt x1 2) (expt y1 2)) (- y2 y3))
                 (* (+ (expt x2 2) (expt y2 2)) (- y3 y1))
                 (* (+ (expt x3 2) (expt y3 2)) (- y1 y2)))
                d))
           (cy (/
                (+
                 (* (+ (expt x1 2) (expt y1 2)) (- x3 x2))
                 (* (+ (expt x2 2) (expt y2 2)) (- x1 x3))
                 (* (+ (expt x3 2) (expt y3 2)) (- x2 x1)))
                d))
           (c (make-point :x cx :y cy)))
      (values c (distance c p1)))))


(defmethod in-circumcircle-p ((triangle triangle) (p point))
  "Determines if the point p is in the circumcircle of the
triangle."
  (with-slots (c r) triangle
    (<= (distance c p) r)))


    
(defclass region ()
  ((seed :type point)
   (vertices)))
  
