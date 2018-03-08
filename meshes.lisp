(in-package :untitled-lisp-game.meshes)

(defclass mesh-part ()
  ((gstream :initarg :gstream :accessor gstream)
   (texture-index :initform 0 :initarg :material :accessor material)))

(defclass mesh ()
  ((parts :initform () :initarg :parts :accessor parts)
   (textures :initform () :initarg textures :accessor textures)))

(defun-g vert ((vert g-pt) &uniform (mat :mat4))
  (values (* mat (v! (pos vert) 70))
          (tex vert)))

(defun-g frag ((tex :vec2))
  (v! tex 1.0 1.0))

(defpipeline-g mesh-prog ()
  (vert g-pt)
  (frag :vec2))

(defgeneric render (mesh)
  (:documentation "Render a mesh"))

(defmethod render ((m mesh))
  (with-slots (parts textures) m
    (declare (ignore textures))
    (dolist (part parts)
      (map-g #'mesh-prog (gstream part) :mat (m4:rotation-x (/ -3.14 2))))))

(defun mesh-data->mesh-part (mesh-data)
  (let ((gstream (make-buffer-stream (vertices mesh-data)
                                     :index-array (indices mesh-data))))
    (make-instance 'mesh-part :gstream gstream)))

(defun mesh-datas->mesh (mesh-datas)
  (make-instance 'mesh
                 :parts (mapcar #'mesh-data->mesh-part mesh-datas)))

(defun load-file (file-path)
  (mesh-datas->mesh (scene-meshes->gpu (ai:import-into-lisp file-path))))
