;;;; untitled-lisp-game.lisp

(in-package #:untitled-lisp-game)

(defvar *test-scene* nil)
(defvar *running* nil)

(defparameter *triangle-data*
   (list (list (v!  0.5 -0.36 0) (v! 0 1 0 1))
         (list (v!    0   0.5 0) (v! 1 0 0 1))
         (list (v! -0.5 -0.36 0) (v! 0 0 1 1))))

(defclass game-object ()
  ((position :initform (v! 0 0 0) :initarg :pos :accessor pos)
   (mesh :initarg :mesh :reader mesh)))

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
  (untitled-lisp-game.meshes:render *test-scene*)
  ;; Display newly rendered buffer
  (swap))

(defun window-size-callback (size &rest _)
  (declare (ignore _))
  (let ((dimensions (v! (aref size 0) (aref size 1))))
    (setf (viewport-resolution (current-viewport)) dimensions)))

(defun load-bob ()
  (setf *test-scene* (untitled-lisp-game.meshes:load-file
                      "assets/boblampclean.md5mesh"
                      '(:ai-process-flip-u-vs
                        :ai-process-flip-winding-order
                        :ai-process-triangulate
                        :ai-process-gen-smooth-normals))))

(defun run-loop ()
  (setf *running* t)
  (load-bob)
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
