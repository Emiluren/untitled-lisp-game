(in-package :untitled-lisp-game.meshes)

(defclass mesh-part ()
  ((gstream :initarg :gstream :accessor gstream)
   (texture-index :initform 0 :initarg :texture-index :accessor texture-index)))

(defclass mesh ()
  ((parts :initform () :initarg :parts :accessor parts)
   (samplers :initform #() :initarg :samplers :accessor samplers)))

(defun-g vert ((vert g-pnt) &uniform (mat :mat4))
  (values (* mat (v! (pos vert) 70))
          (tex vert)))

(defun-g frag ((tc :vec2) &uniform (tex :sampler-2d))
  (texture tex tc))

(defpipeline-g mesh-prog ()
  (vert g-pnt)
  (frag :vec2))

(defgeneric render (mesh)
  (:documentation "Render a mesh"))

(defmethod render ((m mesh))
  (with-slots (parts samplers) m
    (declare (type (simple-array sampler) samplers))
    (dolist (part parts)
      (map-g #'mesh-prog (gstream part)
             :mat (m4:* (m4:rotation-y 2.6)
                        (m4:rotation-x (/ -3.14 2)))
             :tex (aref samplers (texture-index part))))))

(defun mesh-data->mesh-part (mesh-data)
  (let ((gstream (make-buffer-stream (vertex-data mesh-data)
                                     :index-array (indices mesh-data)
                                     :primitive (primitive-type mesh-data))))
    (make-instance 'mesh-part
                   :gstream gstream
                   :texture-index (material-index mesh-data))))

(defun load-file (file-path &optional flags)
  (let ((scene (ai:import-into-lisp file-path :processing-flags flags)))
    (make-instance 'mesh
                   :parts (mapcar #'mesh-data->mesh-part (scene-meshes->gpu scene))
                   :samplers (map 'vector #'load-texture (scene->texture-files scene)))))
