(in-package :untitled-lisp-game.meshes)

(defclass mesh-part ()
  ((gstream :initarg :gstream :accessor gstream)
   (texture-index :initform 0 :initarg :texture-index :accessor texture-index)))

(defclass mesh ()
  ((parts :initform () :initarg :parts :accessor parts)
   (samplers :initform #() :initarg :samplers :accessor samplers)
   (global-inverse-transform :initform (m4:identity)
                             :initarg :global-inverse-transform
                             :accessor global-inverse-tranform)))

(defun-g vert ((pnt g-pnt) (bone-data vertex-bone-data)
               &uniform (mat :mat4) (bones (:mat4 100)))
  (with-accessors ((ids vertex-bone-data-bone-ids)
                   (ws vertex-bone-data-weights))
      bone-data
    (let* ((bone-transform (+ (* (aref bones (aref ids 0))
                                 (aref ws 0))
                              (* (aref bones (aref ids 1))
                                 (aref ws 2))
                              (* (aref bones (aref ids 0))
                                 (aref ws 0))
                              (* (aref bones (aref ids 0))
                                 (aref ws 0))))
           (local-position (* bone-transform
                              (v! (pos pnt) 70))))
      (values (* mat local-position)
              (tex pnt)))))

(defun-g frag ((tc :vec2) &uniform (tex :sampler-2d))
  (texture tex tc))

(defpipeline-g mesh-prog ()
  (vert g-pnt vertex-bone-data)
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

(defun mesh->bone-mapping (ai-mesh)
  (loop with bone-mapping = (make-hash-table :test 'equal)
     for bone across (ai:bones ai-mesh)
     for bone-index upfrom 0
     do (setf (gethash (ai:name bone) bone-mapping) bone-index)
     finally (return bone-mapping)))

(defun mesh-data->mesh-part (mesh-data)
  (let ((gstream (make-buffer-stream (list (vertex-data mesh-data)
                                           (bone-data mesh-data))
                                     :index-array (indices mesh-data)
                                     :primitive (primitive-type mesh-data))))
    (make-instance 'mesh-part
                   :gstream gstream
                   :texture-index (material-index mesh-data))))

(defun load-file (file-path &optional flags)
  (let ((scene (ai:import-into-lisp file-path :processing-flags flags)))
    (make-instance 'mesh
                   :parts (mapcar #'mesh-data->mesh-part
                                  (scene-meshes->gpu scene))
                   :samplers (map 'vector #'load-texture
                                  (scene->texture-files scene))
                   :global-inverse-transform (m4:inverse
                                              (ai:transform
                                               (ai:root-node scene))))))
