;;; ui.lisp --- Simple UI for Voronoi diagram

;; Copyright (C) 2020 Alexey Veretennikov
;;
;; Author: Alexey Veretennikov <alexey dot veretennikov at gmail dot com>

(in-package town-gen)

(declaim (optimize (debug 3)))

;; enable infix syntax reader 
(named-readtables:in-readtable cmu-infix:syntax)


(define-interface voronoi-ui ()
  ((voronoi :initform nil)
   (sweepline :initform nil))
  (:panes
   (draw-board output-pane
               :min-width 500
               :min-height 500
               :draw-with-buffer t
               :resize-callback 'on-resize-draw-board
               :display-callback 'on-redisplay-draw-board)
   (generate-button push-button :text "Next step" :callback
'on-generate-button))
  (:layouts
   (main-layout column-layout '(draw-board
                                generate-button)
                :adjust :center
                :y-ratios '(1 nil)
                :internal-border 20))
  (:default-initargs :title "Voronoi diagram"
   :layout 'main-layout))


(defmethod initialize-instance :after ((self voronoi-ui) &key &allow-other-keys)
  (let* ((pts '((5.51989 5.47211) 
                (5.31936 5.18963)
                (1.75213 9.95644)
                (3.22207 7.62254)
                (9.51712 9.3285)
                (4.20739 2.09659)
                (7.22886 5.36499)
                (8.76779 1.28443)
                (3.94233 6.88071)
                (9.98516 9.91357)))
         (nodes
          (loop for (x y) in pts collect
                (make-point :x x :y y)))
         (v (make-instance 'voronoi :nodes nodes
                           :sweepline-event-callback
                           (lambda (y)
                             (sweepline-event self Y)))))
    (setf (slot-value self 'voronoi) v)
    (gp:invalidate-rectangle (slot-value self 'draw-board))))


(defmethod sweepline-event ((self voronoi-ui) y)
  (setf (slot-value self 'sweepline) y)
  (format t "Sweepline moved to ~a~%" y)
  (capi:apply-in-pane-process self 'gp:invalidate-rectangle (slot-value self 'draw-board)))


(defun on-generate-button (data self)
  "Callback called then the user press Generate button"
  (declare (ignore data))           
  (with-slots (draw-board voronoi) self
    (voronoi-generate-step voronoi)
    ;; force redisplay
    (gp:invalidate-rectangle draw-board)))



(defun on-redisplay-draw-board (pane x y width height)
  (let ((interface (element-interface pane)))
    (with-slots (voronoi sweepline) interface
      ;; calculate draw area. The draw area is approximately 15%
      ;; from borders.
      (let* ((border-x #I( 15 * (width / 100) ))
             (border-y #I( 15 * (height / 100) ))
             (area-x (+ x border-x))
             (area-y (+ y border-y))
             (area-w (- width (* 2 border-x )))
             (area-h (- height (* 2 border-y )))
             (pixmap (gp:create-pixmap-port pane width height :background :grey :clear t))
             (bb (voronoi-bounding-box voronoi)))
        (destructuring-bind (sx sy ox oy)
            (scale-shift-box bb (list area-x area-y
                                      (- width border-x)
                                      (- height border-y)))
          (format t "sx ~a sy ~a ox ~a oy ~a~%"
                  sx sy ox oy)
          ;; draw border
          (gp:draw-rectangle pixmap area-x area-y area-w area-h :filled t :foreground :grey85)
          (flet ((new-x (x) (+ ox (* sx x)))
                 (new-y (y) (- height (+ oy (* sy y)))))
            (loop for p in (voronoi-nodes voronoi)
                  for x = (new-x (point-x p))
                  for y = (new-y (point-y p))
                  do 
                  (gp:draw-circle pixmap x y 2
                                  :foreground :red :filled t))
            (when sweepline
              (format t "drawing sweepline from ~a ~a to ~a ~a~%"
                            area-x (new-y sweepline)
                            (+ area-w area-x) (new-y sweepline))
              (gp:draw-line pixmap
                            area-x (new-y sweepline)
                            (+ area-w area-x) (new-y sweepline)
                          :foreground :blue))))
        ;; show the pixmap. 
        (gp:copy-pixels pane pixmap 0 0 width height 0 0)))))


(defun on-resize-draw-board (pane x y width height)
  (on-redisplay-draw-board pane x y width height))


(defun main-ui ()
  (display (make-instance 'voronoi-ui)))