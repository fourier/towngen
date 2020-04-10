(in-package town-gen)

;; enable infix syntax reader 
(named-readtables:in-readtable cmu-infix:syntax)


(defclass arc ()
  ((focus :initarg :focus
          :accessor arc-focus
          :type point
          :documentation "Focus point of parabola")
  (directrix :initarg :directrix
        :accessor arc-directrix
        :type float
        :documentation "Directrix of parabola, y-coordinate"))
  (:documentation "Vertical (along y) parabolic arc by focus and directrix"))

(defmethod print-object ((self arc) stream)
  "Printer for ARC"
  (format stream "Focus: ~a directrix: ~a~%"
          (if (slot-boundp self 'focus)
              (arc-focus self)
              "(not defined)")
          (if (slot-boundp self 'directrix)
              (arc-directrix self)
              "(not defined)")))

(defmethod arc-intersections ((arc1 arc) (arc2 arc))
  "Calculate intersections of 2 arcs with the same directrix"
  (flet ((close (x y)
           (let ((eps *point-precision*))
             #I( abs(x-y) < eps))))
    (with-coords ((arc-focus arc1)
                  (arc-focus arc2))
      (let ((l (arc-directrix arc1))
            (l2 (arc-directrix arc2)))
        ;; sanity check
        (unless (close l l2)
          (error
           "The directrix for arc1 and arc2 are different: ~f vs ~f"
           l (arc-directrix arc2)))
        (flet ((f (x)
                 #I((x - x1)^^2/(2*(y1 - l)) + (l + y1)/2)))
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
                  (t nil))))))))
