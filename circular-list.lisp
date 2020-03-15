(in-package town-gen)

(defclass circular-list ()
  ((entry :initform nil :initarg :entry
          :accessor circular-list-entry
          :documentation "Element of the list itself")
  (left :initform nil :initarg :left
        :accessor circular-list-left
        :documentation "Left element")
  (right :initform nil :initarg :right
         :accessor circular-list-right
         :documentation "Right element"))
  (:documentation "Circular list class"))

(defmethod print-object ((self circular-list) stream)
  (format stream "'~a' <- '~a' -> '~a'~%"
          (when (circular-list-left self)
            (circular-list-entry (circular-list-left self)))
          (circular-list-entry self)
          (when (circular-list-right self)
            (circular-list-entry (circular-list-right self)))))

(defmethod initialize-instance :after ((self circular-list)
                                       &key &allow-other-keys)
  "Initializes the first element of the list, setting up
the pointers to the left/right elements to self"
  (setf (circular-list-left self) self
        (circular-list-right self) self))


(defmethod circular-list-append-entry ((self circular-list)
                                       entry)
  (circular-list-append self
                        (make-instance 'circular-list :entry entry)))

(defmethod circular-list-append ((self circular-list)
                                 (other circular-list))
  "Insert the entry to the right end of the double-linked list.
Returns the updated list element (OTHER)"
  ;;  +-----------+                +---------------------+
  ;;  |           |                |                     |
  ;;  |          \|/               |                    \|/
  ;;  +- self -> last -+    =>     +- self <-> last <-> other
  ;;     /|\           |              /|\                |
  ;;      |            |               |                 |
  ;;      +------------+               +-----------------+
  ;;
  (let ((last (circular-list-left self)))
    (setf
     ;; update pointers left/right on a new node itself
     (circular-list-left other) last 
     (circular-list-right other) self
     ;; update pointers on the last node
     (circular-list-left self) other
     (circular-list-right last) other))
  other)


(defmethod circular-list-remove ((start circular-list)
                                 (other circular-list))
  "Remove the entry OTHER from the double-linked list START
Returns the updated START element"
  ;; Let's go through the different cases
  ;; 1. The start is empty
  (cond ((null start) start)
        ;; 2. the start and other are the same and no other elements
        ((and (eq other start) (eq start (circular-list-right start)))
         nil)
        ;; 3. the start and end are the same and there are others
        ((eq other start)
         (let ((last (circular-list-left start))
               (new-start (circular-list-right start)))
           (setf (circular-list-left new-start) last
                 (circular-list-right last) new-start)
           new-start))
        ;; 4. the start and end are not the same and other is the last
        ((eq other (circular-list-left start))
         (let* ((last (circular-list-left start))
                (almost-last (circular-list-left last)))
           (setf (circular-list-right almost-last) start
                 (circular-list-left start) almost-last)
           start))
        ;; if the element is in the middle of not empty list
        (t (let ((prev (circular-list-left other))
                 (next (circular-list-right other)))
             (setf (circular-list-right prev) next
                   (circular-list-left next) prev))
           start)))
                 
                                      

(defun circular-list-iterate (self func)
  (when self
    (loop for started = nil then t
          for next = self then (circular-list-right next)
          until (and started (eq next self))
          do
          (funcall func next))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests


(defun test-circlular-add-iterate ()
  ;; test that iterate through the list with one entry
  ;; and left/right pointers works
  (let ((lst (make-instance 'circular-list :entry 10))
        (count 0))
    (circular-list-iterate lst
                           (lambda (e)
                             (declare (ignore e))
                             (incf count)))
    (assert (= count 1)))

  ;; test of the insert to the right of the double
  ;; linked list
  ;; NOTE that insert right will always lead to push into
  ;; the right, so all right elements will be moved
  (let ((lst (make-instance 'circular-list :entry 11))
        (data '(10 4 20 6 25 18))
        (expected-list '(11 10 4 20 6 25 18))
        (result)
        (count 0))
    (dolist (x data)
      (circular-list-append-entry lst x))
    (circular-list-iterate lst
                                (lambda (e)
                                  (incf count)
                                  (push (circular-list-entry e) result)))
    (assert (= count (1+ (length data))))
    (assert (equal expected-list (nreverse result)))))

(defun test-circlular-list-remove ()
  ;; test remove the element from the middle
  (let ((q (make-instance 'circular-list :entry 10))
        (result))
    (circular-list-append-entry q 20)
    (circular-list-append-entry q 30)
    (let ((el (circular-list-right q)))
      (setf q (circular-list-remove q el)))
    (circular-list-iterate q
                             (lambda (e)
                               (push (circular-list-entry e)
                                     result)))
    (assert (equal '(10 30) (nreverse result))))
  ;; test removal of the rightmost element
  (let ((q (make-instance 'circular-list :entry 10))
        (result))
    (circular-list-append-entry q 20)
    (circular-list-append-entry q 30)
    ;; q = 10 20 30
    (let ((el (circular-list-right
               (circular-list-right q))))
      (setf q (circular-list-remove q el)))
    ;; removed 30, q = 10 20
    (circular-list-iterate q (lambda (e)
                               (push (circular-list-entry e)
                                     result)))
    (print result)
    (assert (equal '(10 20) (nreverse result))))
  ;; test removal of the head of the list
  (let ((q (make-instance 'circular-list :entry 10))
        (result))
    (circular-list-append-entry q 20)
    (circular-list-append-entry q 30)
    ;; BUG
    (setq q (circular-list-remove q q))
    (circular-list-iterate q (lambda (e)
                               (push (circular-list-entry e)
                                     result)))
    (assert (equal '(20 30) (nreverse result))))
  ;; test removal the only element of the list
  (let ((q (make-instance 'circular-list :entry 10))
        (result))
    (setq q (circular-list-remove q q))
    (circular-list-iterate q (lambda (e)
                               (push (circular-list-entry e)
                                     result)))
    (assert (null result))))
