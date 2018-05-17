;;;; untitled-lisp-game.lisp

(in-package #:untitled-lisp-game)

(defun try-compile-shaders (shaders)
  (restart-case (compile-shader-dictionary shaders)
    (retry-shader-compilation () shaders)))

(defvar *bob-mesh* nil)
(defvar *running* nil)

(defclass game-object ()
  ((position :initform (kit.math:vec3 0 0 0) :initarg :pos :accessor pos)
   (mesh :initarg :mesh :reader mesh)))

(defun load-bob ()
  (setf *bob-mesh* (ulg.meshes:load-file
                    "assets/boblampclean.md5mesh"
                    '(:ai-process-flip-u-vs
                      :ai-process-flip-winding-order
                      :ai-process-triangulate
                      :ai-process-gen-smooth-normals))))

(defclass vao-shader-window (gl-window)
  ((view-matrix :initform (kit.glm:ortho-matrix -2 2 -2 2 -2 2))
   (programs :initform nil)))

(defmethod initialize-instance :after ((w vao-shader-window)
                                       &key shaders &allow-other-keys)
  (setf (idle-render w) t)
  (gl:viewport 0 0 800 600)

  (with-slots (programs) w
    ;; Compile shaders using the dictionary name specified via :shaders
    (setf programs (try-compile-shaders shaders)))

  ;; Now that we have a opengl context we can load the mesh
  (load-bob))

(defmethod render ((window vao-shader-window))
  (with-slots (view-matrix vao programs) window
    (gl:clear-color 0.2 0.2 0.2 1.0)
    (gl:clear :color-buffer)

    ;; Now we just tell GL to draw the contents:
    (ulg.meshes:render *bob-mesh* programs view-matrix)))

(defdict game-programs ()
  (program :mesh-prog (:view-m :bone-offsets :tex)
           (:vertex-shader (:file "shader.vert"))
           (:fragment-shader (:file "shader.frag"))))

(defun launch ()
  (kit.sdl2:start)
  (sdl2:in-main-thread ()
    (sdl2:gl-set-attr :context-major-version 3)
    (sdl2:gl-set-attr :context-minor-version 3)
    (sdl2:gl-set-attr :context-profile-mask 1))
  (make-instance 'vao-shader-window :shaders 'game-programs))
