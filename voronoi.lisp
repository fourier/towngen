;;; voronoi.lisp --- Voronoi diagram implementation

;; Copyright (C) 2020 Alexey Veretennikov
;;
;; Author: Alexey Veretennikov <alexey dot veretennikov at gmail dot com>

(in-package town-gen)

(declaim (optimize (debug 3)))

(defstruct circle-event
  (node nil :type point)
  (arc nil :type btree-node))


(defclass voronoi ()
  ((nodes :initform nil :initarg :nodes
          :reader voronoi-nodes
          :documentation "A list of nodes to build Voronoi diagram from")
   (boundig-box :initform nil
                :reader voronoi-bounding-box
                :documentation "a list of 4 coordinates of a bounding box: top left - bottom right")
   (edges :initform nil
          :reader voronoi-edges
          :documentation "A list of generated edges")
   (beachline :initform nil
              :type btree
              :reader voronoi-beachline
              :documentation "A beach line of the Fortune's method. Binary tree")
   (queue :type prio-queue
          :reader voronoi-queue
          :documentation "Priority queue for the Fortune's method")

   (move-sweepline :reader voronoi-move-sweepline
    :documentation "Fuction of one argument - the current position of the sweepline. Updates the beachline nodes with
a new sweepline position. This function is set then the beachline first time created, following the pattern let-over-lambda")
   (intersection :reader voronoi-intersection
                 :documentation "Function of 2 arguments - 2 points, which calculates the intersection of the corresponding parabolas where 2 points are focuses and sweepline is a common directrix"))
  (:documentation "Fortune's algorithm for Voronoi diagrams"))


(defmethod event-get-x ((node point))
  (point-x node))


(defmethod event-get-x ((evt circle-event))
  (point-x (circle-event-node evt)))


(defmethod event-get-y ((node point))
  (point-y node))


(defmethod event-get-y ((evt circle-event))
  (point-y (circle-event-node evt)))


(defun make-sweepline-functions (&optional (initial-sweepline-pos 0))
  "Creates a pair of 2 functions - one to update
sweepline position and second to calculate intersection of 2
arcs given their focus points (and common directrix - sweepline)"
  (let ((sweepline-pos initial-sweepline-pos))
    (cons 
     (lambda (new-pos)
       (setf sweepline-pos new-pos))
     (lambda (p1 p2)
       (let ((intersections
              (parabola-intersections p1 p2 sweepline-pos)))
         (format t "Intersections: ~a~%" intersections)
         (cond ((null intersections)
                (error "points are on the same vertical line"))
               ((null (cdr intersections))
                (car intersections))
               (t
                (destructuring-bind (bp1 bp2) intersections
                  (if (< (point-y bp1) (point-y bp1))
                      bp1 bp2)))))))))


(defmethod initialize-instance :after ((self voronoi)
                                       &key &allow-other-keys)
  ;; create a priority queue
  (with-slots (nodes queue move-sweepline intersection) self
    (unless nodes (error "No nodes in Voronoin diagram"))
    (destructuring-bind (modifier . divider)
        (make-sweepline-functions)
      (setf move-sweepline modifier
            intersection divider
            ;; Push to the priority queue all nodes sorting
            ;; by Y coordinate
            queue (make-instance 'prio-queue
                                 :test
                                 (lambda (a b)
                                   (< (event-get-y a)
                                      (event-get-y b))))))
    (loop for node in nodes do (prio-queue-push queue node))))


(defmethod make-voronoi (nodes)
  "Create instance of the Voronoi class.
No generation performed yet"
  (make-instance 'voronoi :nodes nodes))


(defmethod handle-voronoi-queue-event :before ((self voronoi) event)
  ;; move the sweepline
  (format t "Sweepline at ~a~%" (event-get-y event))
  (funcall (voronoi-move-sweepline self) (event-get-y event)))


(defmethod handle-voronoi-queue-event ((self voronoi) (circle-event circle-event))
  "Handle circle event of the Voronoi diagram"
  (format t "Circle event: ~a~%" circle-event)
  (btree-remove-leaf (voronoi-beachline self) (circle-event-arc circle-event)))


(defmethod create-binary-tree ((self voronoi) (point point))
  "Create the first element in the binary tree"
  (with-slots (beachline) self
    (setf beachline
          (make-instance 'btree
                         :comparator
                         (lambda (a b)
                           (> (event-get-x a)
                              (event-get-x b)))
                         :divider (lambda (a b)
                                    (make-point :x 
                                                (/
                                                 (+ (event-get-x a)
                                                    (event-get-x b))
                                                 2.0)
                                                :y 0.0))))
    (btree-insert beachline point)))


(defmethod handle-voronoi-queue-event ((self voronoi) (point point))
  "Handle node event of the Voronoi diagram"
  (format t "Node event: ~a~%" point)
  (with-slots (beachline queue) self
    (let ((sweepline (point-y point)))
      (if (null beachline)
          ;; create the first element in binary tree
          (create-binary-tree self point)
          ;; beachline not empty
          ;; insert into the beach line
          (let ((new-node (btree-insert beachline point)))
            ;; check all triples of nearest points containing
            ;; new point, if they form a circle which has
            ;; lowest point below the sweepline
            (flet ((check-circle-event (n1 n2 n3)
                     (let ((p1 (btree-node-value n1))
                           (p2 (btree-node-value n2))
                           (p3 (btree-node-value n3)))
                       (multiple-value-bind (c r)
                           (circumcenter-points p1 p2 p3)
                         ;; points non collinear
                         (when (and c r)
                           ;; lowest point of the circle
                           (let ((circle-event-point
                                  (make-point :x (point-x c)
                                              :y (- (point-y c) r))))
                             ;; only if the lowest point below
                             ;; the sweepline
                             (format t "circle point ~a sweepline ~a~%"
                                     (point-y circle-event-point)
                                     sweepline)
                             (when (< (point-y circle-event-point)
                                      sweepline)
                               (format t "creating circle event ~a~%" circle-event-point)
                               (prio-queue-push
                                queue
                                (make-circle-event
                                 :node circle-event-point
                                 :arc n2)))))))))
              ;; first triple left-left-center, where center
              ;; is a new point
              (when-let* ((l (btree-node-find-left-neighbor new-node))
                          (ll (btree-node-find-left-neighbor l)))
                (check-circle-event l ll new-node))
              ;; second triple left-center-right
              (when-let* ((l (btree-node-find-left-neighbor new-node))
                          (r (btree-node-find-right-neighbor new-node)))
                (check-circle-event l new-node r))
              ;; third triple center-right-right
              (when-let* ((r (btree-node-find-right-neighbor new-node))
                          (rr (btree-node-find-right-neighbor r)))
                (check-circle-event new-node r rr))))))))


(defmethod voronoi-generate ((self voronoi))
  ;; implementation follows the link
  ;; https://web.archive.org/web/20110601031438/http://cgm.cs.mcgill.ca/~mcleish/644/Projects/DerekJohns/Sweep.htm#DataStructures
  (with-slots (nodes queue) self
    (loop for evt = (prio-queue-pop queue)
          while evt
          ;; pickup the event from the queue
          ;; the handle-voronoi-queue-event will
          ;; dispatch by event type
          do (handle-voronoi-queue-event self evt)))
  self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-simple-diagragm-10points ()
  ;; from Wolfram Mathematica:
  ;; pts = {{5.51989, 5.47211}, {5.31936, 5.18963}, {1.75213, 9.95644}, {3.22207, 7.62254}, {9.51712, 9.3285}, {4.20739, 2.09659}, {7.22886, 5.36499}, {8.76779, 1.28443}, {3.94233, 6.88071}, {9.98516, 9.91357}};
  ;; VoronoiMesh[pts]
  (let* ((pts '((5.51989 5.47211) 
                (5.31936 5.18963)
                (1.75213 9.95644)
                (3.22207 7.62254)
                (9.51712 9.3285)
                (4.20739 2.09659)
                (7.22886 5.36499)
                (8.76779 1.28443)
                (3.94233 6.88071)
                (9.98516 9.91357)))
         (nodes
          (loop for (x y) in pts collect
                (make-point :x x :y y)))
         (v (make-instance 'voronoi :nodes nodes)))
    (voronoi-generate v)))


    
                          
             


