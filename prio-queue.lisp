(in-package town-gen)

;; simple tree operations
(deftype tree () 'list)

(defun make-tree (head &optional (children nil))
  (cons head children))
(defun tree-head (tree) (car tree))
(defun tree-children (tree) (cdr tree))
(defun tree-add-child (tree child)
  (cons (car tree) (push child (cdr tree))))


;;----------------------------------------------------------------------------
;; Conditions
;;----------------------------------------------------------------------------
(define-condition empty-prio-queue-error (error)
  ((text :initarg :text :reader text :initform "Empty priority queue")))

(defun raise-empty ()
  (error 'empty-prio-queue-error))


;; priority tree entry - an entry and its neighbours in
;; double-linked list
(defstruct prio-queue-entry
  (entry)                               ; entry itself
  (parent nil)                          ; parent node
  (left nil)                            ; left node in list
  (right nil)                           ; right node in list
  (child nil)                           ; child list
  (degree 0)                            ; amount of children
  (marked nil))                         ; if it is marked
           
(defun mkentry (el)
  "Constructor for the priority queue element,
initializing the prio-queue-entry and a double-linked list"
  (let ((entry (make-prio-queue-entry :entry el)))
    (setf (prio-queue-entry-left entry) entry
          (prio-queue-entry-right entry) entry)
    entry))


(defmethod print-object ((self prio-queue-entry) stream)
  (format stream "Entry: ~a~%" 
          (prio-queue-entry-entry self)))

(defmethod prio-queue-entry-insert-right ((entry prio-queue-entry) (other-entry prio-queue-entry))
  "Insert the entry to the right of the double-linked list,
shifting if necessary"
  ;;  [l]-[c]-[r] => [l]-[c]-[el]-[r]
  ;; here [c] = entry and [el] = other-entry
  (symbol-macrolet
      ((c entry)
       (l (prio-queue-entry-left entry))
       (r (prio-queue-entry-right entry))
       (el other-entry)
       (l1 (prio-queue-entry-left other-entry))
       (r1 (prio-queue-entry-right other-entry)))
    (setf
     ;; update pointers left/right on a new node itself
     l1 c
     r1 r)
    ;; now update the left pointer of the right node
    ;; if it is not self of course
    (unless (eq r c)
      (setf (prio-queue-entry-left r) el))
     ;; ...and the right node itself
    (setf r el))
  entry)

  

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
    (prio-queue-entry-entry top)))


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
                         (prio-queue-entry-entry top)
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
        (prio-queue-entry-insert-right roots entry))))

      
    
(defmethod prio-queue-pop ((q prio-queue))
  (with-slots (count test-function top roots) q
    (when top
      (when-let (child (prio-queue-entry-child top))
        ;; move all children to the roots list
        (prio-queue-list-iterate
         child
         (lambda (e)
           (prio-queue-roots-insert q e)
           (setf (prio-queue-entry-parent e) nil))))
      ;; remove the top element from roots list
      (prio-queue-roots-remove q top)
      (if (eq (prio-queue-entry-right top) top)
          ;; if no right branches, it was the last element
          (setf top nil
                roots nil)
          ;; otherwise
          (progn
            (setf top (prio-queue-entry-right top))
            (prio-queue-consolidate q)))
      (decf count)
      (prio-queue-entry-entry top))))


(defmethod prio-queue-list-iterate((elt prio-queue-entry) func)
  "Iterate over double-linked list ELT applying function FUNC to each element"
  (loop for next = elt then (prio-queue-entry-right next)
        for started = nil then t
        for counter below 10
        until (and started (or (null next) (eq elt next)))
        do (funcall func next)))



  


