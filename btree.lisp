;;; btree.lisp --- Binary tree for Voronoi diagram

;; Copyright (C) 2020 Alexey Veretennikov
;;
;; Author: Alexey Veretennikov <alexey dot veretennikov at gmail dot com>

(in-package town-gen)

(defclass btree ()
  ((value :initarg :value
          :reader btree-value
          :documentation "Value of the tree node")
   (comparator :initarg :comparator
               :initform #'<
               :reader comparator
               :documentation "Function used to compare 2 value")
   (divider :initarg :divider
            :reader btree-divider
            :initform (lambda (x y) (/ (+ x y) 2.0))
            :documentation "Calculates the new middle node between new inserted and old")
  (left :initform nil
        :initarg :left
        :type (or null btree)
        :accessor btree-left
        :documentation "Left leaf of the tree")
  (right :initform nil
         :initarg :right
         :type (or null btree)
         :accessor btree-right
         :documentation "Right leaf of the tree")
  (parent :initform nil
          :initarg :parent
          :type (or null btree)
          :accessor btree-parent
          :documentation "Parent of the node"))
  (:documentation "Binary tree for Fortune's method.
This binary tree has a property that there are no subtrees
having only one leaf. All roots of the tree have either 0
leafs, or 2."))

(defmethod btree-< ((btree btree) value)
  "Compare btree node with value"
  (funcall (slot-value btree 'comparator)
           (btree-value btree) value))

(defmethod btree-make-from ((btree btree) value)
  "Create a new node with the same functions and parent
as old but with a new value"
  (make-instance 'btree
                 :value value
                 :comparator (slot-value btree 'comparator)
                 :divider (slot-value btree 'divider)
                 :parent (slot-value btree 'parent)))
                 
(defmethod btree-leaf-p ((btree btree))
  "Determine if the btree is a leaf, i.e. has no other leafs"
  (with-slots (left right) btree
    (and (null left) (null right))))

(defmethod btree-insert ((btree btree) new-value)
  "Insert the VALUE into the binary tree.
Returns the new root if necessary"
  ;; decide where to put, left or right
  (with-slots (left right value) btree
    ;; case the first root
    (if (btree-leaf-p btree)
        ;; new root
        (let* ((new-root
                (btree-make-from btree
                                 (funcall (btree-divider btree) 
                                          value new-value)))
               (new-leaf (btree-make-from btree new-value)))
          ;; update parents
          (setf (btree-parent btree) new-root
                (btree-parent new-leaf) new-root)
          ;; where to place old root
          (if (btree-< new-root value) ;; add to the right
              (setf (btree-right new-root) btree
                    (btree-left new-root) new-leaf)
              ;; or add to left
              (setf (btree-left new-root) btree
                    (btree-right new-root) new-leaf))
              new-root)
        ;; the tree has other nodes, find the closest one
        (progn 
          (if (btree-< btree new-value)
              (setf right (btree-insert right new-value))
              (setf left (btree-insert left new-value)))
          btree))))

(defmethod btree-crawl ((btree btree) selector
                        &optional (stop-then
                                   (lambda (x prev)
                                     (declare (ignore x prev))
                                     nil)))
  "Climb the binary tree from current node using specified
SELECTOR (i.e. btree-right or btree-left or btree-parent), until
the last element reached with no possible SELECTOR or
STOP-THEN returned T.
The STOP-THEN is a boolean function accepting 2 arguments:
current element and previous element.
Return: (values NODE PREVIOUS STOPPED)
Here NODE is the last node traversed
PREVIOUS is the previous node,
STOP-THEN is a boolean flag indicating if the iteration
prematurely stopped"
  (loop for node = btree then (funcall selector node)
        and prev = node
        and prev2 = prev
        while node
        when (funcall stop-then node prev)
        do
        (return (values node prev t))
        end
        finally (return (values prev prev2 nil))))
        

(defmethod btree-find-neighbor ((btree btree) selector1 selector2)
  (with-slots (parent) btree
    ;; only do search if its a leaf and it has a parent
    (when (and (btree-leaf-p btree) parent)
      (flet ((stop-criteria (n p)
               ;; stop on nil
               (or (not n)
                   ;; othewise check if the another branch
                   ;; is not the previous
                   (when-let ((s (funcall selector1 n)))
                     (not (eq s p))))))
          ;; find the first parent which has left/right subtree
          ;; not equal to us
          (multiple-value-bind (common-parent prev)
              (btree-crawl btree #'btree-parent #'stop-criteria)
            (subtree (funcall selector1 common-parent)))
            (format t "Start crawl down at ~a~%"
                    (btree-value common-parent))
            ;; now got by the opposite branch of the subtree
            ;; of the common parent
            (btree-crawl subtree selector2))))))
        
(defmethod btree-find-left-neighbor ((btree btree))
  (btree-find-neighbor btree #'btree-left #'btree-right))

(defmethod btree-find-right-neighbor ((btree btree))
  (btree-find-neighbor btree #'btree-right #'btree-left))


(defmethod btree-remove ((btree btree))
  "Remove the node from the binary tree"
  (with-slots (left right parent) btree
    (unless (or (null parent) ; only if not root of the tree
                (not (btree-leaf-p btree))) ; and a leaf
      )))

(defmethod btree-dot ((btree btree) &optional (stream t))
  "Generate the DOT syntax for drawing the binary tree.
Prints output to the stream STREAM"
  (labels ((p (b)
           (with-slots (parent left right value) b
             (if parent
                 (format stream "    ~a -> ~a~%~a~:[~;[style=filled,color=\"0 0 0.7\"]~];~%"
                         (btree-value parent) value value
                         (btree-leaf-p b))
                 (format stream "    ~a;~%" value))
             (when left (p left))
             (when right (p right)))))
    ;; preamble
    (format stream "digraph G {~%")
    (p btree)
    ;; postamble
    (format stream "}~%")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-btree ()
  (let ((b (make-instance 'btree :value 10)))
    (setf b (btree-insert b 5))
    (setf b (btree-insert b 7))
    b))

(defun test-btree1 ()
  (let ((b (make-instance 'btree :value 4)))
    (setf b (btree-insert b 1))
    (setf b (btree-insert b 2))
    (setf b (btree-insert b 5))
    b))


(defun test-btree2 ()
  (let ((b (make-instance 'btree :value 20)))
    (setf b (btree-insert b 11))
    (setf b (btree-insert b 14))
    (setf b (btree-insert b 17))
    (setf b (btree-insert b 13))
    (setf b (btree-insert b 19))
    (setf b (btree-insert b 15))
    (setf b (btree-insert b 5))
    (setf b (btree-insert b 2))
    (setf b (btree-insert b 18))
    b))

(defun btree-dot1 (btree)
  (with-open-file (s "C:/Sources/lisp/towngen/graph1.gv" :direction :output :if-exists :supersede)
    (btree-dot btree s)))