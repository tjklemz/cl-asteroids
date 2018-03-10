(ql:quickload :cl-glfw3)
(ql:quickload :cl-glfw3-examples)

(defpackage #:myapp
  (:use #:cl #:cl-glfw3 #:alexandria #:trivial-main-thread))

(in-package #:myapp)

(defparameter *inputs* (list :up nil :down nil :left nil :right nil))

(defun wrap (object)
  (let ((s 2.5))
    (when (< (getf object :x) -1.25)
      (incf (getf object :x) s))
    (when (> (getf object :x) 1.25)
      (decf (getf object :x) s))
    (when (< (getf object :y) -1.25)
      (incf (getf object :y) s))
    (when (> (getf object :y) 1.25)
      (decf (getf object :y) s))))

(defun create-ship ()
  (list
    :x 0 :y 0 :rot (/ pi 2) :x-velocity 0 :y-velocity 0 :thrust 0 :rot-velocity 0
    :draw (lambda () (draw-triangle 0.1))
    :input (lambda (ship)
      (setf (getf ship :thrust) 0)
      (when (getf *inputs* :up) (incf (getf ship :thrust) 0.001))
      (when (getf *inputs* :down) (decf (getf ship :thrust) 0.001))
      (setf (getf ship :rot-velocity) 0)
      (when (getf *inputs* :left) (incf (getf ship :rot-velocity) 0.1))
      (when (getf *inputs* :right) (decf (getf ship :rot-velocity) 0.1)))))

(defun create-bullet ()
  (list
    :x 0 :y 0 :rot (/ pi 2) :x-velocity 0 :y-velocity 0 :thrust 0 :rot-velocity 0 :life 100
    :draw (lambda () (draw-line 0.1))
    :input (lambda (bullet) nil)))

(defun fire-bullet (object)
  (let ((bullet (create-bullet))
        (rot (getf object :rot)))
    (setf (getf bullet :x) (getf object :x))
    (setf (getf bullet :y) (getf object :y))
    (setf (getf bullet :rot) rot)
    ;(setf (getf bullet :x-velocity) (* (+ 0.01 speed) (cos rot)))
    (setf (getf bullet :x-velocity) (+ (getf object :x-velocity) (* 0.075 (cos rot))))
    ;(setf (getf bullet :y-velocity) (* (+ 0.01 (if (< (getf object :y-velocity) 0) 0 speed)) (sin rot)))
    (setf (getf bullet :y-velocity) (+ (getf object :y-velocity) (* 0.075 (sin rot))))
    (setf *objects* (cons bullet *objects*))))

(defparameter *ship* (create-ship))
(defparameter *objects* (list *ship*))

(defparameter *width* 600)
(defparameter *height* 600)

(defun draw-triangle (s)
  (let ((s2 (/ s 2)))
    (gl:with-primitives :line-loop
      (gl:vertex 0 s2)
      (gl:vertex s2 (- s2))
      (gl:vertex (- s2) (- s2)))))

(defun draw-line (s)
  (gl:with-primitives :lines
    (gl:vertex 0 0)
    (gl:vertex 0 s)))

(defun draw (shape)
  (gl:with-pushed-matrix
    (gl:color 1 1 1)
    (gl:translate (getf shape :x) (getf shape :y) 0)
    (gl:rotate (- (/ (* (getf shape :rot) 180) pi) 90) 0 0 1)
    (funcall (getf shape :draw))))

(defun calc (object)
  (incf (getf object :y-velocity) (* (getf object :thrust) (sin (getf object :rot))))
  (incf (getf object :x-velocity) (* (getf object :thrust) (cos (getf object :rot))))
  (incf (getf object :rot) (getf object :rot-velocity))
  (incf (getf object :y) (getf object :y-velocity))
  (incf (getf object :x) (getf object :x-velocity))
  (wrap object))

(defun render ()
  (gl:clear :color-buffer)
  (loop for object in *objects* do (funcall (getf object :input) object) (calc object) (draw object)))

(defun set-viewport (width height)
  (gl:clear-color 0.2 0.2 0.2 0.2)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -1 1 -1 1 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(def-key-callback handle-keys (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close))
  (when (and (eq key :r) (eq action :press))
    (setf *ship* (create-ship))
    (setf *objects* (list *ship*)))
  (when (eq key :up)
    (setf (getf *inputs* :up) (not (eq action :release))))
  (when (eq key :down)
    (setf (getf *inputs* :down) (not (eq action :release))))
  (when (eq key :left)
    (setf (getf *inputs* :left) (not (eq action :release))))
  (when (eq key :right)
    (setf (getf *inputs* :right) (not (eq action :release))))
  (when (and (eq key :space) (eq action :press))
    (fire-bullet *ship*)))

(defun run-example ()
  ;; Graphics calls on OS X must occur in the main thread
  (with-body-in-main-thread ()
    (with-init-window (:title "OpenGL particle test" :width *width* :height *height*)
      (set-key-callback 'handle-keys)
      (set-viewport *width* *height*)
      (loop until (window-should-close-p)
            do (render)
            do (swap-buffers)
            do (poll-events)))))
