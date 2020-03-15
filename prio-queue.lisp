(in-package town-gen)


;;----------------------------------------------------------------------------
;; Conditions
;;----------------------------------------------------------------------------
(define-condition empty-prio-queue-error (error)
  ((text :initarg :text :reader text :initform "Empty priority queue")))

(defun raise-empty ()
  (error 'empty-prio-queue-error))


;; priority tree entry - an entry and its neighbours in
;; double-linked list
(defclass prio-queue-entry (circular-list)
  ((parent :initform nil :initarg :parent
           :accessor prio-queue-entry-parent
           :documentation "Parent element")
   (child :initform nil :initarg :child
          :accessor prio-queue-entry-child
          :documentation "Child list, circular-list")
   (degree :initform 0
           :accessor prio-queue-entry-degree
           :documentation "Amount of children")
   (marked :initform nil
           :accessor prio-queue-entry-marked
           :documentation "if it is marked")))
           
(defun mkentry (value)
  "Constructor for the priority queue element,
initializing the prio-queue-entry and a double-linked list"
  (make-instance 'prio-queue-entry :entry value))


(defmethod print-object ((self prio-queue-entry) stream)
  (format stream "Entry: ~a~%" 
          (circular-list-entry self)))


(defmethod prio-queue-entry-insert-child ((parent prio-queue-entry)
                                          (entry prio-queue-entry))
  "Inserts the ENTRY into child list of PARENT,
destructively for ENTRY - modifying its left/right pointers"
  (with-slots (child) parent
  (if child 
      (circular-list-append child entry)
      (setf (circular-list-left entry) entry
            (circular-list-right entry) entry
            child entry))))


;; priority queue structure
(defclass prio-queue ()
  ((top :initform nil :type prio-queue-entry :documentation "A top (min) element of the priority queue")                  
   (roots :initform nil :type prio-queue-entry :documentation "A double-linked list of prio-queue elements")
   (count :initform 0 :type fixnum :documentation "Total amount of elements in the queue")
   (test-function :initarg :test :initform #'> :reader prio-queue-test-function :documentation "Test function for elements")))


(defmethod prio-queue-top ((q prio-queue))
  "Returns the value of the top element of the tree.
Complexity: O(1)"
  (when-let (top (slot-value q 'top))
    (circular-list-entry top)))


(defmethod prio-queue-push ((q prio-queue) el)
  "Insert the element into the priority queue.
Complexity: O(1)"
  (let ((entry (mkentry el)))
    ;; insert the element into roots list, no merge
    (prio-queue-roots-insert q entry)
    (with-slots (count test-function top) q
      ;; update top if necessary
      (when (or (null top)
                (funcall test-function
                         (circular-list-entry top)
                         el))
        (setf top entry))
      ;; update the count of elements
      (incf count)))
  q)


(defmethod prio-queue-roots-insert ((q prio-queue) (entry prio-queue-entry))
  "Insert an entry into the right of the roots list"
  (with-slots (roots) q
    (if (null roots)
        (setf roots entry)
        ;; otherwise add to the right of the roots list
        (circular-list-append roots entry))))


(defmethod prio-queue-roots-remove ((q prio-queue) (el prio-queue-entry))
  "Remove entry EL from the roots list"
  (with-slots (roots) q
    (assert (not (null roots)))
    (setf roots (circular-list-remove roots el)))
  q)
    
    
(defmethod prio-queue-pop ((q prio-queue))
  (with-slots (count top) q
    (when top
      (let ((r (circular-list-right top))
            (top-value (circular-list-entry top)))
        ;; move all children to the roots list
        (when-let (child (prio-queue-entry-child top))
          (circular-list-iterate
           child
           (lambda (e)
             (prio-queue-roots-insert q e)
             (setf (prio-queue-entry-parent e) nil))))
        ;; remove the top element from roots list
        (prio-queue-roots-remove q top)
        (if (eq r top)
            (setf top nil)
            (progn
              (setf top r)
              (prio-queue-consolidate q)))
        (decf count)
      top-value))))



(defmethod prio-queue-link ((q prio-queue)
                            (y prio-queue-entry)
                            (x prio-queue-entry))
  "Remove Y from the root list of Q and make Y a child of X"
  (prio-queue-roots-remove q y)
  (prio-queue-entry-insert-child x y)
  (incf (slot-value x 'degree))
  (setf (slot-value x 'marked) nil))


(defmethod prio-queue-consolidate ((q prio-queue))
  (with-slots (count test-function top) q
    (flet ((cmp (x y)
             (funcall test-function
                      (circular-list-entry x)
                      (circular-list-entry y))))
      (let ((degrees (make-hash-table))
            (nodes))
        ;; collect NODES = copy of the roots list, which
        ;; will be destructively traversed below
        (circular-list-iterate (slot-value q 'roots)
                               (lambda (e) (push e nodes)))
        ;; iterate oven each root of the prio queue
        (loop for w in nodes
              for x = w
              for d = (prio-queue-entry-degree x)
              do 
              (loop for y = (gethash d degrees)
                    while y
                    when (cmp x y)
                    do
                    (let ((tmp x)) (setf x y y tmp))
                    end
                    do
                    (prio-queue-link q y x)
                    (remhash d degrees)
                    (incf d))
              (setf (gethash d degrees) x))
        (setf top nil)
        (loop for v being the hash-value of degrees
              when (or (null top)
                       (cmp v top))
              do (setf top v))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-prio-queue-pop ()
  (let ((q (make-instance 'prio-queue)))
    (loop for i in '(15 10 20 18)
          do (prio-queue-push q i))
    (loop with expected = (list 10 15 18 20)
          for j below 2
          for x = (prio-queue-pop q)
          for y = (pop expected)
          while expected
          do (assert (equal x y)))))