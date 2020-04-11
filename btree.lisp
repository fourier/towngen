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
         :documentation "Right leaf of the tree"))
  (:documentation "Binary tree for Fortune's method.
This binary tree has a property that there are no subtrees
having only one leaf. All roots of the tree have either 0
leafs, or 2."))

(defmethod btree-< ((btree btree) value)
  "Compare btree node with value"
  (funcall (slot-value btree 'comparator)
           (btree-value btree) value))

(defmethod btree-make-from ((btree btree) value)
  "Create a new node with the same functions as old but
with a new value"
  (make-instance 'btree
                 :value value
                 :comparator (slot-value btree 'comparator)
                 :divider (slot-value btree 'divider)))
                 
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
        (let ((new-leaf (btree-make-from btree new-value))
              (new-root
               (btree-make-from btree
                                (funcall (btree-divider btree) 
                                         value new-value)))
              (crap (format t "Leaf ~a inserting ~a"
                            value new-value)))
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

;;(defmethod btree-remove ((btree btree) value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-btree ()
  (let ((b (make-instance 'btree :value 10)))
    (setf b (btree-insert b 5))
    (setf b (btree-insert b 7))
    b))
