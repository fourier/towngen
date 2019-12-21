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
  (tree-head (slot-value q 'top)))


(defmethod prio-queue-merge ((q1 prio-queue) (q2 prio-queue))
  (let* ((test-f (slot-value q1 'test-function))
         (new-top
           (cond ((and (slot-boundp q1 'top)
                       (slot-boundp q2 'top))
                  (let ((t1 (tree-head (slot-value q1 'top)))
                        (t2 (tree-head (slot-value q2 'top))))
                    (if (funcall test-f t1 t2)
                        (slot-value q1 'top)
                        (slot-value q2 'top))))
                ((slot-boundp q1 'top) (slot-value q1 'top))
                ((slot-boundp q2 'top) (slot-value q2 'top))
                (t (error "Attempt to merge to empty prio queues"))))
         (q
           (make-instance 'prio-queue
                          :test test-f)))
    (setf (slot-value q 'top) new-top
          (slot-value q 'storage)
          (append (slot-value q1 'storage)
                  (slot-value q2 'storage)))
    q))


(defmethod prio-queue-push ((q prio-queue) el)
  (let ((q1 (make-instance 'prio-queue
                           :test (slot-value q 'test-function))))
    (setf (slot-value q1 'top) (make-tree el)
          (slot-value q1 'storage) (list (make-tree el)))
    (prio-queue-merge q1 q)))


(defmethod prio-queue-pop ((q prio-queue))
  (with-slots (top storage test-function) q
    ;; 1. save the top element and make its
    ;;    children roots of another trees
    (let ((result top)
                                        ;(lengths (make-hash-table))
          )
      (format t "was ~d~%" (length storage))
      (setf storage
            (delete top storage)
            ;;(append (tree-children top) storage))
            )
      (format t "now ~d~%" (length storage))
      (setf top
            (reduce (lambda (x y)
                      (if (funcall test-function
                                   (tree-head x)
                                   (tree-head y))
                          x y))
                    storage))
      (tree-head result))))
      ;; ...and update top element
      ;; 2. merge all the trees with the same number
      ;;    of children
      
    
              



  


