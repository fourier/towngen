(in-package town-gen)

;; simple tree operations
(deftype tree () 'list)

(defun make-tree (head &optional (children nil))
  (cons head children))
(defun tree-head (tree) (car tree))
(defun tree-children (tree) (cdr tree))


;; priority queue structure
(defclass prio-queue ()
  ((top)
   (storage :initform nil)
   (test-function :initarg :test :initform #'> :reader prio-queue-test-function)))


(defmethod prio-queue-top ((q prio-queue))
  (slot-value q 'top))


(defmethod prio-queue-merge ((q1 prio-queue) (q2 prio-queue))
  (let ((new-top
          (cond ((and (slot-boundp q1 'top)
                      (slot-boundp q2 'top))
                 (if (funcall (slot-value q1 'test-function)
                              (slot-value q1 'top)
                              (slot-value q2 'top))
                     (slot-value q1 'top)
                     (slot-value q2 'top)))
                ((slot-boundp q1 'top) (slot-value q1 'top))
                ((slot-boundp q2 'top) (slot-value q2 'top))
                (t (error "Attempt to merge to empty prio queues"))))
        (q
          (make-instance 'prio-queue
                         :test (slot-value q1 'test-function))))
    (setf (slot-value q 'top) new-top
          (slot-value q 'storage)
          (append (slot-value q1 'storage)
                  (slot-value q2 'storage)))
    q))


(defmethod prio-queue-insert ((q prio-queue) el)
  (let ((q1 (make-instance 'prio-queue
                           :test (slot-value q 'test-function))))
    (setf (slot-value q1 'top) el
          (slot-value q1 'storage) (make-tree el))
    (prio-queue-merge q1 q)))


(defmethod prio-queue-pop ((q prio-queue))
  (with-slots (top storage test-function) q
    ;; 1. save the top element and make its
    ;; children roots of another trees
    (let ((result top))
      (setf storage
            (append (tree-children top) storage))
      ;; 
    
              



  


