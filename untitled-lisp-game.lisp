;;;; untitled-lisp-game.lisp

(in-package #:untitled-lisp-game)

(defparameter *array* nil)
(defparameter *stream* nil)
(defparameter *running* nil)

;; Lisp data for triangle vertices
(defparameter *triangle-data*
   (list (list (v!  0.5 -0.36 0) (v! 0 1 0 1))
         (list (v!    0   0.5 0) (v! 1 0 0 1))
         (list (v! -0.5 -0.36 0) (v! 0 0 1 1))))

;; A struct that works on gpu and cpu
(defstruct-g pos-col
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

;; A GPU vertex shader using a Lisp syntax (see Varjo)
(defun-g vert ((vert pos-col))
  (values (v! (pos vert) 1.0)
          (col vert)))

;; A GPU fragment shader
(defun-g frag ((color :vec4))
  color)

;; Composing those gpu functions into a pipeline
(defpipeline-g prog-1 ()
  (vert pos-col)
  (frag :vec4))

;; Here is what we do each frame:
(defun step-demo ()
  ;; Advance the host environment frame
  (step-host)
   ;; Keep the REPL responsive while running
  (update-repl-link)
  ;; Clear the drawing buffer
  (clear)
  ;; Render data from GPU datastream
  (map-g #'prog-1 *stream*)
  ;; Display newly rendered buffer
  (swap))


(defun run-loop ()
  (setf *running* t
        *array* (make-gpu-array *triangle-data* :element-type 'pos-col)
        *stream* (make-buffer-stream *array*))
  (loop :while (and  *running*
             (not (shutting-down-p))) :do
     (continuable (step-demo))))

(defun stop-loop ()
  (setf *running* nil))
