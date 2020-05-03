;;; ui.lisp --- Simple UI for Voronoi diagram

;; Copyright (C) 2020 Alexey Veretennikov
;;
;; Author: Alexey Veretennikov <alexey dot veretennikov at gmail dot com>

(in-package town-gen)

(declaim (optimize (debug 3)))

(define-interface voronoi-ui ()
  ((voronoi :initarg nil))
  (:panes
   (draw-board output-pane
               :min-width 500
               :min-height 500
               :draw-with-buffer t
               :resize-callback 'on-resize-draw-board
               :display-callback 'on-redisplay-draw-board)
   (generate-button push-button :text "Generate" :callback
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
  (setf (slot-value self 'voronoi) (test-simple-diagragm-10points))
  (on-generate-button nil self))


(defun on-generate-button (data self)
  "Callback called then the user press Generate button"
  (declare (ignore data))           
  (with-slots (draw-board) self
    ;; force redisplay
    (gp:invalidate-rectangle draw-board)))


(defun on-redisplay-draw-board (pane x y width height)
  (let ((interface (element-interface pane)))
    (with-slots (voronoi) interface
      ;; calculate draw area
      (let* ((area-x (+ 5 x))
             (area-y (+ 5 y))
             (area-w (- width 10 ))
             (area-h (- height 10))
             (pixmap (gp:create-pixmap-port pane width height :background :grey :clear t)))
        ;; draw border
        (gp:draw-rectangle pixmap area-x area-y area-w area-h :filled t :foreground :grey85)
;;;         (dolist (p (voronoi-nodes voronoi))
;;;           (with-coords (p)
;;;             (gp:draw-points
(gp:draw-points pixmap '(10 10 20 20 30 30))
;;;         (dolist (w walls)
;;;           (destructuring-bind (x1 y1 x2 y2) w
;;;             (gp:draw-line pixmap
;;;                           (+ area-x (* x1 cell-w))
;;;                           (+ area-y (* y1 cell-h))
;;;                           (+ area-x (* x2 cell-w))
;;;                           (+ area-y (* y2 cell-h))
;;;                           :thickness 5
;;;                           :foreground :purple)))

        ;; show the pixmap. 
        (gp:copy-pixels pane pixmap 0 0 width height 0 0)))))


(defun on-resize-draw-board (pane x y width height)
  (on-redisplay-draw-board pane x y width height))


(defun main-ui ()
  (display (make-instance 'voronoi-ui)))