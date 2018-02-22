;;;; opengl-test.lisp

(in-package #:opengl-test)

;; This function is taken from https://github.com/AlexCharlton/cl-glfw3/blob/master/examples/fragment-shader.lisp
(defun check-shader-error (shader)
  "Get the current error status of a shader, throw error if status"
  (let ((error-string (gl:get-shader-info-log shader)))
    (unless (equalp error-string "")
      (progn

        ;; Print to console & then throw error
        (format t "~A~%" error-string)
        (error
         'compile-error
         :message error-string)))))

(defun load-shader (vertex-shader-file fragment-shader-file)
  (let ((vert-shader (gl:create-shader :vertex-shader))
        (frag-shader (gl:create-shader :fragment-shader)))
    (gl:shader-source vert-shader (read-file-into-string vertex-shader-file))
    (gl:shader-source frag-shader (read-file-into-string fragment-shader-file))
    (gl:compile-shader vert-shader)
    (gl:compile-shader frag-shader)
    (check-shader-error vert-shader)
    (check-shader-error frag-shader)
    (let ((shader-prog (gl:create-program)))
      (gl:attach-shader shader-prog vert-shader)
      (gl:attach-shader shader-prog frag-shader)
      (gl:link-program shader-prog)
      (gl:use-program shader-prog)
      shader-prog)))

(glfw:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 w h))

(defun buffer-with-data (data data-type target)
  (let ((buf (gl:gen-buffer))
        (arr (gl:alloc-gl-array data-type (length data))))
    (dotimes (i (length data))
      (setf (gl:glaref arr i) (aref data i)))
    (gl:bind-buffer target buf)
    (gl:buffer-data target :static-draw arr)
    (gl:free-gl-array arr)
    (gl:bind-buffer target 0)
    buf))

(defparameter vertex-pos-location 0)

(defun init-triangle ()
  (let ((vao (gl:gen-vertex-array))
        (vertex-buffer (buffer-with-data #(-0.5 0.0 0.0
                                           0.5 0.0 0.0
                                           0.0 0.5 0.0) :float :array-buffer)))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vertex-buffer)
    (gl:enable-vertex-attrib-array vertex-pos-location)
    (gl:vertex-attrib-pointer vertex-pos-location 3 :float nil 0 (cffi:null-pointer))
    vao))

(defun render (triangle-vao shader-prog mat-location)
  (gl:clear :color-buffer :depth-buffer)
  (gl:use-program shader-prog)
  (gl:bind-vertex-array triangle-vao)
  (let ((time (sin (glfw:get-time))))
    (gl:uniform-matrix-4fv mat-location
                           (marr (mtranslation (vec 0 time 0)))))
  (gl:draw-arrays :triangles 0 3))

(defparameter *running* nil)

(defun main ()
  (setf *running* t)
  (with-body-in-main-thread ()
    (glfw:with-init-window (:title "OpenGL test" :width 600 :height 400
                                   :context-version-major 3
                                   :context-version-minor 3
                                   :opengl-profile :opengl-core-profile)
      (glfw:set-window-size-callback 'update-viewport)
      (gl:viewport 0 0 600 400)
      (gl:clear-color 0.2 0.2 0.2 0.2)
      (let* ((shader-prog (load-shader "shader.vert" "shader.frag"))
             (triangle-vao (init-triangle))
             (mat-location (gl:get-uniform-location shader-prog "mat")))
        (loop
           while (and
                  *running*
                  (not (glfw:window-should-close-p)))
           do (progn
                (render triangle-vao shader-prog mat-location)
                (glfw:swap-buffers)
                (glfw:poll-events)))))))
