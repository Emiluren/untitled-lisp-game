;;;; untitled-lisp-game.lisp

(in-package #:untitled-lisp-game)

(defvar *array* nil)
(defvar *stream* nil)
(defvar *running* nil)

(defparameter *triangle-data*
   (list (list (v!  0.5 -0.36 0) (v! 0 1 0 1))
         (list (v!    0   0.5 0) (v! 1 0 0 1))
         (list (v! -0.5 -0.36 0) (v! 0 0 1 1))))

(defstruct-g pos-col
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

(defun-g vert ((vert pos-col) &uniform (mat :mat4))
  (values (* mat (v! (pos vert) 1.0))
          (col vert)))

(defun-g frag ((color :vec4))
  color)

(defpipeline-g prog-1 ()
  (vert pos-col)
  (frag :vec4))

(defun current-window-size ()
  (destructuring-bind (w h)
      (cepl.host:window-size (current-surface (cepl-context)))
    (v! w h)))

;; Here is what we do each frame:
(defun step-game ()
  ;; Advance the host environment frame
  (step-host)
  ;; Keep the REPL responsive while running
  (update-repl-link)
  ;; Clear the drawing buffer
  (clear)
  ;; Render data from GPU datastream
  (let ((sint (sin (/ (sdl2:get-ticks) 1000))))
      (map-g #'prog-1 *stream* :mat (m4:translation (v! 0.0 sint 0.0))))
  ;; Display newly rendered buffer
  (swap))

(defun window-size-callback (size &rest _)
  (declare (ignore _))
  (let ((dimensions (v! (aref size 0) (aref size 1))))
    (setf (viewport-resolution (current-viewport)) dimensions)))

(defun run-loop ()
  (setf *running* t
        *array* (make-gpu-array *triangle-data* :element-type 'pos-col)
        *stream* (make-buffer-stream *array*))
  ;; Make the viewport fill the whole screen
  (setf (viewport-resolution (current-viewport)) (current-window-size))
  (whilst-listening-to ((#'window-size-callback (window 0) :size))
    (loop :while (and *running*
                      (not (shutting-down-p))) :do
       (continuable (step-game)))))

(defun stop-loop ()
  (setf *running* nil))
