;;; voronoi.lisp --- Voronoi diagram implementation

;; Copyright (C) 2020 Alexey Veretennikov
;;
;; Author: Alexey Veretennikov <alexey dot veretennikov at gmail dot com>

(in-package town-gen)

(defstruct circle-event
  (node nil :type point)
  (arc nil :type arc))

(defclass voronoi ()
  ((nodes :initform nil :initarg :nodes
          :reader voronoi-nodes
          :documentation "A list of nodes to build Voronoi diagram from")
   (edges :initform nil
          :reader voronoi-edges
          :documentation "A list of generated edges")
   (beachline :initform nil
              :reader voronoi-beachline
              :documentation "A beach line of the Fortune's method")
   (sweepline :initform 0
              :type float
              :accessor voronoi-sweepline
              :documentation "A y-coordinate of the sweepline")
   (queue :type prio-queue
          :reader voronoi-queue
          :documentation "Priority queue for the Fortune's method"))
  (:documentation "Fortune's algorithm for Voronoi diagrams"))


(defmethod event-get-y ((node point))
  (point-y node))

(defmethod event-get-y ((evt circle-event))
  (point-y (circle-event-node evt)))


(defmethod initialize-instance :after ((self voronoi)
                                       &key &allow-other-keys)
  ;; create a priority queue
  (with-slots (nodes queue) self
    (unless nodes (error "No nodes in Voronoin diagram"))
    (setf queue (make-instance 'prio-queue
                               :test
                               (lambda (a b)
                                 (> (event-get-y a)
                                    (event-get-y b)))))
    (loop for node in nodes do (prio-queue-push queue node))))

(defmethod make-voronoi (nodes)
  "Create instance of the Voronoi class.
No generation performed yet"
  (make-instance 'voronoi :nodes nodes))

(defmethod handle-voronoi-queue-event ((circle-event circle-event))
  "Handle circle event of the Voronoi diagram"
  (format t "Circle event: ~a~%" circle-event))

(defmethod handle-voronoi-queue-event ((point point))
  "Handle node event of the Voronoi diagram"
    (format t "Node event: ~a~%" point))


(defmethod voronoi-generate ((self voronoi))
  (with-slots (nodes queue) self
    (loop for evt = (prio-queue-pop queue)
          while evt
          do (handle-voronoi-queue-event evt))))
