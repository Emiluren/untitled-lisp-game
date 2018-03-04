;;;; untitled-lisp-game.lisp

(in-package #:untitled-lisp-game)

(defvar *array* nil)
(defvar *stream* nil)
(defvar *test-scene* nil)
(defvar *test-stream* nil)
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

(defun-g bob-vert ((vert g-pt) &uniform (mat :mat4))
  (values (* mat (v! (pos vert) 70))
          (tex vert)))

(defun-g bob-frag ((tex :vec2))
  (v! tex 1.0 1.0))

(defpipeline-g bob-prog ()
  (bob-vert g-pt)
  (bob-frag :vec2))

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
  ;; (map-g #'prog-1 *stream* :mat (m4:translation (v! 0 0 0)))
  (map-g #'bob-prog *test-stream* :mat (m4:rotation-x (/ -3.14 2)))
  ;; Display newly rendered buffer
  (swap))

(defun window-size-callback (size &rest _)
  (declare (ignore _))
  (let ((dimensions (v! (aref size 0) (aref size 1))))
    (setf (viewport-resolution (current-viewport)) dimensions)))

(defun load-model (filename)
  )

(defun run-loop ()
  (setf *running* t
        *array* (make-gpu-array *triangle-data* :element-type 'pos-col)
        *stream* (make-buffer-stream *array*)
        *test-scene* (untitled-lisp-game.model-parsers:load-file "assets/boblampclean.md5mesh")
        *test-stream* (make-buffer-stream
                       (first (first *test-scene*))
                       :index-array (second (first *test-scene*))))
  ;; Make the viewport fill the whole screen
  (setf (viewport-resolution (current-viewport)) (current-window-size))
  (gl:clear-color 0.2 0.2 0.2 1.0)
  (skitter:whilst-listening-to ((#'window-size-callback (skitter:window 0) :size))
    (loop
       while (and *running*
                  (not (shutting-down-p)))
       do (continuable (step-game)))))

(defun stop-loop ()
  (setf *running* nil))
