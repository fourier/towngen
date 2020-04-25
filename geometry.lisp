;;; geometry.lisp --- Geometric primitives and functions

;; Copyright (C) 2020 Alexey Veretennikov
;;
;; Author: Alexey Veretennikov <alexey dot veretennikov at gmail dot com>

(in-package town-gen)

;; enable infix syntax reader 
(named-readtables:in-readtable cmu-infix:syntax)

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


(defmacro with-coords ((p1 &optional
                           (p2 nil p2-p)
                           (p3 nil p3-p)
                           (p4 nil p4-p))
                       &body body)
  "Expands to the let-binding of x1,y1,x2 etc
from the points p1, [p2, [p3, [p4]]]"
  `(let ((x1 (point-x ,p1))
         (y1 (point-y ,p1))
         ,@(when p2-p
             `((x2 (point-x ,p2))
               (y2 (point-y ,p2))))
         ,@(when p3-p
             `((x3 (point-x ,p3))
               (y3 (point-y ,p3))))
         ,@(when p4-p
             `((x4 (point-x ,p4))
               (y4 (point-y ,p4)))))
     ,@body))
  

(defmethod distance ((p1 point) (p2 point))
  (with-coords (p1 p2)
    #I( sqrt( (x1 - x2)^^2 + (y1 - y2)^^2))))


(defmethod equals ((p1 point) (p2 point))
  (let ((eps *point-precision*))
    (with-coords (p1 p2)
      #I( abs(x1-x2) < eps and abs(y1-y2) < eps))))


(defmethod initialize-instance :after ((triangle triangle)
                                       &key &allow-other-keys)
  (with-slots (p1 p2 p3 c r) triangle
    ;; calculate the determinant for points orientation,
    ;; to see if they are oriented counter-clockwise or clockwise
    ;; | x1 y1 1 |
    ;; | x2 y2 1 | > 0 means clockwise
    ;; | x3 y3 1 |
    ;; = x1 y2 - x1 y3 - x2 y1 + x2 y3 + x3 y1 - x3 y2
    (with-coords (p1 p2 p3)
      (let* ((tmp (copy-structure p2))
             (det #I(x1*y2-x1*y3-x2*y1+x2*y3+x3*y1-x3*y2)))
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


(defun circumcenter-points (p1 p2 p3)
  "Circumcenter of the triangle.
See https://en.wikipedia.org/wiki/Circumscribed_circle#Cartesian_coordinates_2
for details.
Return values (circumcenter, radius), or nil if the points
are collinear"
  (with-coords (p1 p2 p3)
    (let ((d #I(2*(x1*(y2-y3) + x2*(y3-y1) + x3*(y1-y2)))))
      (if (> (abs d) *point-precision*)
          (let ((cx
                 #I(((x1^^2 + y1^^2) * (y2-y3) +
                     (x2^^2 + y2^^2) * (y3-y1) +
                     (x3^^2 + y3^^2) * (y1-y2)) / d))
                (cy
                 #I(((x1^^2 + y1^^2) * (x3-x2) +
                     (x2^^2 + y2^^2) * (x1-x3) +
                     (x3^^2 + y3^^2) * (x2-x1)) / d)))
            (let* ((c (make-point :x cx :y cy))
                   (r (distance c p1)))
              (values c r)))
          (values)))))

(defmethod circumcenter ((triangle triangle))
  "Circumcenter of the triangle.
See https://en.wikipedia.org/wiki/Circumscribed_circle#Cartesian_coordinates_2
for details.
Return values (circumcenter, radius)"
  (with-slots (p1 p2 p3 c r) triangle
    (multiple-value-bind (cc rr)
        (circumcenter-points p1 p2 p3)
      (setf c cc r rr))))

(defmethod in-circumcircle-p ((triangle triangle) (p point))
  "Determines if the point p is in the circumcircle of the
triangle."
  (with-slots (c r) triangle
    (<= (distance c p) r)))


(defmethod lowest-point-from-circumcenter ((triangle triangle))
  "Returns the lowest (y-coordinate) point lying on the
circumcircle of the triangle"
  (with-slots (c r ) triangle
    (make-point :x (point-x c) :y (+ r (point-y c)))))

(defmethod parabola-intersections ((focus1 point) (focus2 point) (l number))
  "Calculate intersections of 2 arcs with the same directrix,
given by their focuses"
  (with-coords (focus1 focus2)
    (flet ((f (x)
             #I((x - x1)^^2/(2*(y1 - l)) + (l + y1)/2))
           (close (x y)
             (let ((eps *point-precision*))
               #I( abs(x-y) < eps))))
      ;; calculate coefficients in quadratic equation
      (let* ((a #I(y2 - y1))
             (b #I(-(y2 - l)*x1 + (y1 - l)*x2))
             (c #I((y1 - y2)*(y1 - l)*(y2 - l) + (y2 - l)*x1^^2 - (y1 - l)*x2^^2))
             ;; calculate the discriminant
             (d #I(b^^2 - a*c)))
        ;; depending on discriminant solve the equation
        (cond ((close d 0)
               (let ((x #I(-b / a)))
                 (list (make-point :x x :y (f x)))))
              ((> d 0)
               (let ((x+ #I((-b + sqrt(d))/a))
                     (x- #I((-b - sqrt(d))/a)))
                 (list (make-point :x x+ :y (f x+))
                       (make-point :x x- :y (f x-)))))
              (t nil))))))
