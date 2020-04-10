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
  (:documentation "Binary tree"))

(defmethod btree-< ((btree btree) value)
  (funcall (slot-value btree 'comparator)
           (btree-value btree) value))
                        
(defmethod btree-insert ((btree btree) value)
  "Insert the VALUE into the binary tree"
  ;; decide where to put, left or right
  (with-slots (left right) btree
    (if (btree-< btree value)
        (if left (btree-insert left value)
            (setf left (make-instance 'btree :value value)))
        (if right (btree-insert right value)
            (setf right (make-instance 'btree :value value))))))

;;(defmethod btree-remove ((btree btree) value)