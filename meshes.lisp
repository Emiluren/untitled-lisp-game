;; TODO: Maybe split this into multiple files
(in-package :untitled-lisp-game.meshes)

(defclass my-indexed-vao (vao)
  ((ibuff)
   (kit.gl.vao::index :initform 0)))

(defmethod initialize-instance :after ((vao my-indexed-vao) &key &allow-other-keys)
  (vao-bind vao)
  (with-slots (ibuff) vao
    (setf ibuff (gl:gen-buffer))))

(defmethod gl-delete-object ((vao my-indexed-vao))
  (with-slots (ibuff) vao
    (gl:delete-buffers (list ibuff)))
  (call-next-method))

(defun my-indexed-vao-buffer-index-vector (vao indices)
  (with-slots (ibuff) vao
    (let* ((sv (static-vectors:make-static-vector
		(length indices)
		:element-type '(unsigned-byte 16)
		:initial-contents indices))
	   (ptr (static-vectors:static-vector-pointer sv))
	   (byte-size 2))
      (%gl:bind-buffer :element-array-buffer ibuff)
      (%gl:buffer-data :element-array-buffer byte-size ptr :static-draw)
      (static-vectors:free-static-vector sv))))

;; TODO: maybe switch this to use defstruct instead
(defclass mesh-data ()
  ((vao :initarg :vao :reader vao)
   (material-index :initarg :material-index :reader material-index)))

;;; Define the layout of the VAO
(defvao mesh-vao-data ()
  (:separate ()
    (position :float 3)
    (normal :float 3)
    (texture-coord :float 2)
    (bone-ids :int 4)
    (bone-weights :int 4)))

(defun scene-meshes->gpu (scene)
  (map 'list #'mesh->gpu (ai:meshes scene)))

(defun mesh->offset-list (mesh)
  (map 'list #'ai:offset-matrix (ai:bones mesh)))

(defstruct vertex-bones
  (ids ())
  (weights ()))

(defstruct bone-vertex
  (id 0)
  (weight 0.0))

(defun transpose-bone-vertex-list (bone-vertices)
  "Take the bone data from the model file and transform it so that it
  can be used in the shader."
  (let* ((reversed-tail (reverse (last bone-vertices 4)))
         (missing-elements-n (- 4 (length reversed-tail)))
         (extended (append (loop repeat missing-elements-n
                              collect (make-bone-vertex))
                           reversed-tail))
         (ids (mapcar #'bone-vertex-id extended))
         (weights (mapcar #'bone-vertex-weight extended)))
    (make-vertex-bones :ids ids :weights weights)))

(defun make-initial-bone-lists-array (n)
  (loop repeat n
     collect () into list
     finally (return (make-array n :initial-contents list))))

(defun bone-lists->vertex-bones (bone-lists)
  (loop for vb in (map 'list #'transpose-bone-vertex-list bone-lists)
     nconc (vertex-bones-ids vb) into ids
     nconc (vertex-bones-weights vb) into weights
     finally (return (make-vertex-bones :ids ids
                                        :weights weights))))

(defun mesh->vertex-bones (mesh)
  (loop
     with num-vertices = (length (ai:vertices mesh))
     with bone-lists = (make-initial-bone-lists-array num-vertices)
     for bone across (ai:bones mesh)
     for bone-index upfrom 0
     do (loop
           for vertex-weight across (ai:weights bone)
           for vertex-id = (ai:id vertex-weight)
           do (push (make-bone-vertex :id vertex-id
                                      :weight (ai:weight vertex-weight))
                    (aref bone-lists vertex-id)))
     finally (return (bone-lists->vertex-bones bone-lists))))

(defun list->array (list &key (element-type t))
  (make-array (length list) :initial-contents list :element-type element-type))

(defun flatten-nested-array (array)
  (let ((flat (aops:flatten (aops:combine array))))
    ;; Copy to a new array to ensure the result is a "simple-array".
    (make-array (length flat)
		:initial-contents flat
		:element-type (array-element-type flat))))

(defun mesh->gpu (mesh)
  (let* ((verts (flatten-nested-array (ai:vertices mesh)))
         (norms (flatten-nested-array (ai:normals mesh)))
         (texcoords (when (> (length (ai:texture-coords mesh)) 0)
                      (flatten-nested-array (aref (ai:texture-coords mesh) 0))))
         (faces (ai:faces mesh))
         (vbs (mesh->vertex-bones mesh))
         (bone-ids (list->array (vertex-bones-ids vbs)
                                :element-type '(signed-byte 32)))
         (bone-weights (list->array (vertex-bones-weights vbs)
                                    :element-type 'single-float))
         (indices (list->array (loop for % across faces
                                     append (coerce % 'list))))
	 ;; I can't figure out how to use vao-indexed so use my own version
         (vao (make-instance 'my-indexed-vao
                             :type 'mesh-vao-data
                             :primitive :triangles
                             :vertex-count (/ (length verts) 3))))
    ;; Copy the data into the VAO 
    ;; TODO: Lookup shader locations automatically and skip buffering
    ;; if unable to find the location.
    ;; Hm the vbo parameter to vao-buffer-* refers to the internal array
    ;; and not to the GL shader location.
    (loop for i from 0 to 4
       for vector in (list verts norms texcoords bone-ids bone-weights)
	  do (vao-buffer-vector vao i vector :usage :static-draw))
    (my-indexed-vao-buffer-index-vector vao indices)
    (make-instance 'mesh-data
                   :vao vao
                   :material-index (ai:material-index mesh))))

(defparameter *identity-bones*
  (make-array 100 :initial-element (sb-cga:identity-matrix)))

(defclass mesh ()
  ((parts :initform () :initarg :parts :accessor parts)
   (textures :initform #() :initarg :textures :accessor textures)
   (global-inverse-transform :initform (sb-cga:identity-matrix)
                             :initarg :global-inverse-transform
                             :accessor global-inverse-tranform)))

(defgeneric render (mesh programs view-matrix)
  (:documentation "Render a mesh using the supplied shader."))

(defmethod render ((m mesh) programs view-matrix)
  (use-program programs :mesh-prog)
  (uniformi programs :tex 0)
  (uniform-matrix programs :view-m 4 (vector view-matrix))
  (uniform-matrix programs :bone-offsets 4 *identity-bones*)
  (with-slots (parts textures) m
    (dolist (part parts)
      (with-slots (vao material-index) part
        (kit.gl.tex:tex-bind (aref textures material-index))
        (vao-draw-elements vao :type :unsigned-int)))))

(defun mesh->bone-mapping (ai-mesh)
  (loop with bone-mapping = (make-hash-table :test 'equal)
     for bone across (ai:bones ai-mesh)
     for bone-index upfrom 0
     do (setf (gethash (ai:name bone) bone-mapping) bone-index)
     finally (return bone-mapping)))

(defun load-file (file-path &optional flags)
  (let ((scene (ai:import-into-lisp file-path :processing-flags flags)))
    (make-instance 'mesh
                   :parts (scene-meshes->gpu scene)
                   :textures (map 'vector #'ulg.textures:load-texture
                                  (ulg.textures:scene->texture-files scene))
                   :global-inverse-transform (sb-cga:inverse-matrix
                                              (ai:transform
                                               (ai:root-node scene))))))
