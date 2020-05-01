;;; btree.lisp --- Binary tree for Voronoi diagram

;; Copyright (C) 2020 Alexey Veretennikov
;;
;; Author: Alexey Veretennikov <alexey dot veretennikov at gmail dot com>

(in-package town-gen)

(declaim (optimize (debug 3)))

(defclass btree ()
  ((root :initarg :root :initform nil
         :accessor btree-root
         :documentation "Root node of the tree")
   (comparator :initarg :comparator
               :initform #'<
               :reader btree-comparator
               :documentation "Function used to compare 2 value")
   (divider :initarg :divider
            :reader btree-divider
            :initform (lambda (x y) (/ (+ x y) 2.0))
            :documentation "Calculates the new middle node between new inserted and old"))
  (:documentation "Binary tree node for Fortune's method.
This binary tree has a property that there are no subtrees
having only one leaf. All roots of the tree have either 0
leafs, or 2."))

(defclass btree-node ()
  ((value :initarg :value
          :reader btree-node-value
          :documentation "Value of the tree node")
   (left :initform nil
         :initarg :left
         :type (or null btree-node)
         :accessor btree-node-left
         :documentation "Left leaf of the tree")
   (right :initform nil
          :initarg :right
          :type (or null btree-node)
          :accessor btree-node-right
          :documentation "Right leaf of the tree")
   (parent :initform nil
           :initarg :parent
           :type (or null btree)
           :accessor btree-node-parent
           :documentation "Parent of the node"))
  (:documentation "Node of the binary tree"))

(defmethod print-object ((self btree-node) stream)
  "Printer for BTREE-NODE object"
  (format stream "'~a' <- '~a' -> '~a' (parent: ~a)"
          (when-let (l (btree-node-left self))
            (btree-node-value l))
          (btree-node-value self)
          (when-let (r (btree-node-right self))
            (btree-node-value r))
          (when-let (p (btree-node-parent self))
            (btree-node-value p))))
          


(defmethod btree-< ((btree btree) (node btree-node) value)
  "Compare btree node with value"
  (funcall (btree-comparator btree)
           (btree-node-value node) value))

(defmethod btree-node-make-from ((node btree-node) value)
  "Create a new node with the same functions and parent
as old but with a new value"
  (make-instance 'btree-node
                 :value value
                 :parent (slot-value node 'parent)))
                 
(defmethod btree-node-leaf-p ((node btree-node))
  "Determine if the node is a leaf, i.e. has no other leafs"
  (with-slots (left right) node
    (and (null left) (null right))))

(defmethod btree-insert ((btree btree) new-value)
  "Insert the VALUE into the binary tree.
Returns the new node created."
  (with-slots (root) btree
    (if (null root) ;; no root yet
        (setf root
              (make-instance 'btree-node
                             :value new-value))
        ;; root is not empty, call helper 
        (multiple-value-bind (new-root new-node)
            (btree-insert-helper btree root new-value)
          (setf root new-root)
          new-node))))

(defmethod btree-insert-into-leaf ((btree btree)
                                   (node btree-node)
                                   new-value)
  "Insert the VALUE into the binary tree where the NODE is a leaf.
The new root is created using divider and the original and new-value
nodes are placed as leafs of this new root
Returns the values: (new root , new node)"
  (when (btree-node-leaf-p node)
    (with-slots (divider) btree
      (with-slots (value) node
        (let* ((new-root (make-instance 'btree-node
                                        :value 
                                        (funcall divider 
                                                 value
                                                 new-value)
                                        :parent
                                        (btree-node-parent node)))
               (new-leaf (make-instance 'btree-node
                                        :value new-value
                                        :parent new-root)))
          ;; update parent of the old node to point to the new root
          (setf (btree-node-parent node) new-root)
          ;; where to place old root
          (if (btree-< btree new-root value)
              ;; add to the right
              (setf (btree-node-right new-root) node
                    (btree-node-left new-root) new-leaf)
              ;; or add to left
              (setf (btree-node-left new-root) node
                    (btree-node-right new-root) new-leaf))
          (values new-root new-leaf))))))


(defmethod btree-insert-helper ((btree btree) (node btree-node) new-value)
  "Insert the VALUE into the binary tree.
Returns the values: (new root (if necessary), new node)"
  (with-slots (divider comparator) btree
    (with-slots (value) node
      ;; leaf case
      (if (btree-node-leaf-p node)
          (btree-insert-into-leaf btree node new-value)
          ;; the tree case, find where to descend (left/right)
          (let ((leaf-symbol
                 (if (btree-< btree node new-value) 'right 'left)))
            (multiple-value-bind (new-root new-node)
                ;; descend
                (btree-insert-helper btree (slot-value node leaf-symbol) new-value)
              ;; now we received potential new root and a new node
              (setf (slot-value node leaf-symbol) new-root)
              (setf (btree-node-parent new-root) node)
              (values node new-node)))))))


(defmethod btree-node-crawl ((start btree-node) selector
                             &optional (stop-then
                                        (lambda (x prev)
                                          (declare (ignore x prev))
                                          nil)))
  "Climb the binary tree from current node START using specified
SELECTOR (i.e. btree-node-right or btree-node-left or
btree-node-parent), until the last element reached with
no possible SELECTOR or STOP-THEN returned T.
The STOP-THEN is a boolean function accepting 2 arguments:
current element and previous element.
Return: (values NODE PREVIOUS STOPPED)
Here NODE is the last node traversed
PREVIOUS is the previous node,
STOP-THEN is a boolean flag indicating if the iteration
prematurely stopped"
  (loop for node = start then (funcall selector node)
        and prev = node
        and prev2 = prev
        while node
        for r = (funcall stop-then node prev)
        when r
        do
        (return (values node prev r))
        end
        finally (return (values prev prev2 r))))
        

(defmethod btree-node-find-neighbor ((node btree-node)
                                     selector1 selector2)
  "Find the neighbor for the NODE on the SELECTOR1 from
the element. SELECTOR2 is another selector.
SELECTOR1/2 could be #'btree-left/right"
  (with-slots (parent) node
    ;; only do search if its a leaf and it has a parent
    (when (and (btree-node-leaf-p node) parent)
      (flet ((stop-criteria (n p)
               ;; stop on nil
               (or (not n)
                   ;; othewise check if the another branch
                   ;; is not the previous
                   (when-let ((s (funcall selector1 n)))
                     (not (eq s p))))))
        ;; find the first parent which has left/right subtree
        ;; not equal to us
        (multiple-value-bind (common-parent prev found)
            (btree-node-crawl node #'btree-node-parent #'stop-criteria)
          (declare (ignore prev))
          (when found 
            ;; now go down by the opposite branch of the subtree
            ;; of the common parent
            (btree-node-crawl (funcall selector1 common-parent)
                         selector2)))))))
        
(defmethod btree-node-find-left-neighbor ((node btree-node))
  "Find the left leaf neighbor of the leaf NODE."
  (btree-node-find-neighbor node #'btree-node-left #'btree-node-right))

(defmethod btree-node-find-right-neighbor ((node btree-node))
  "Find the right leaf neighbor of the leaf NODE."
  (btree-node-find-neighbor node #'btree-node-right #'btree-node-left))


(defmethod btree-remove-leaf ((btree btree))
  "Remove the leaf node from the binary tree"
  (with-slots (parent) btree
    (unless (or (null parent) ; only if not root of the tree
                (not (btree-leaf-p btree))) ; and a leaf
      (let ((pparent (btree-parent parent))
            (sibling (if (eq (btree-left parent) btree)
                         (btree-right parent)
                         (btree-left parent))))
        ;; update the parent of the sibling to one
        ;; level up
        (setf (btree-parent sibling) pparent)
        (unless pparent
          (format t "no parent's parent"))
        ;; promote the sibling up one level
        (when pparent
            ;; if our parent is left branch, replace
            ;; our parent with sibling
            (if (eq (btree-left pparent) parent)
                (setf (btree-left pparent) sibling)
                ;; otherwise do it with right branch
                (setf (btree-right pparent) sibling))
            ;; update the pparent value - if both
            ;; its left and right values are leafs
            (when (and (btree-leaf-p (btree-left pparent))
                       (btree-leaf-p (btree-right pparent)))
              (setf (slot-value pparent 'value)
                    (funcall (btree-divider pparent)
                             (btree-value (btree-left pparent))
                             (btree-value (btree-right pparent))))))
        ;; disconnect the leaf
        (setf parent nil)
        ;; return new parent
        pparent))))
        

(defmethod btree-dot ((btree btree) &optional (stream t))
  "Generate the DOT syntax for drawing the binary tree.
Prints output to the stream STREAM"
  (labels ((p (b)
           (with-slots (parent left right value) b
             (if parent
                 (format stream "    \"~a\" -> \"~a\"~%\"~a\"~:[~;[style=filled,color=\"0 0 0.7\"]~];~%"
                         (btree-node-value parent) value value
                         (btree-node-leaf-p b))
                 (format stream "    \"~a\";~%" value))
             (when left (p left))
             (when right (p right)))))
    ;; preamble
    (format stream "digraph G {~%")
    (p (btree-root btree))
    ;; postamble
    (format stream "}~%")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-btree ()
  (let ((b (make-instance 'btree)))
    (btree-insert b 10)
    (btree-insert b 5)
    (btree-insert b 7)
    b))

(defun test-btree1 ()
  (let ((b (make-instance 'btree)))
    (btree-insert b 4)
    (btree-insert b 1)
    (btree-insert b 2)
    (btree-insert b 5)
    b))

(defun test-btree2-make ()
  (let ((b (make-instance 'btree)))
    (btree-insert b 20)
    (btree-insert b 11)
    (btree-insert b 14)
    (btree-insert b 17)
    (btree-insert b 13)
    (btree-insert b 19)
    (btree-insert b 15)
    (btree-insert b 5)
    (btree-insert b 2)
    (btree-insert b 18)
    b))

(defun test-btree2 ()
  (let* ((tree (test-btree2-make))
         (leftmost (btree-crawl tree #'btree-left))
         (rightmost (btree-crawl tree #'btree-right))
         (lst '(2 5 11 13 14 15 17 18 19 20)))
    (assert (equal lst
                   (loop for n = leftmost then (btree-find-right-neighbor n)
                         and p = n
                         while n
                         collect (values (btree-value n) p))))

    (assert (equal (nreverse (copy-list lst))
                   (loop for n = rightmost then (btree-find-left-neighbor n)
                         and p = n
                         while n
                         collect (values (btree-value n) p))))))

(defun btree-dot1 (btree)
  (with-open-file (s #+windows "C:/Sources/lisp/towngen/graph1.gv" #-windows "~/Sources/lisp/towngen/graph1.gv" :direction :output :if-exists :supersede)
    (btree-dot btree s)))
