(in-package :untitled-lisp-game.meshes)

;; TODO: maybe switch this to use defstruct instead
(defclass mesh-data ()
  ((vertex-data :initarg :vertex-data :reader vertex-data)
   (bone-data :initarg :bone-data :reader bone-data)
   (indices :initarg :indices :reader indices)
   (primitive-type :initarg :primitive-type :reader primitive-type)
   (material-index :initarg :material-index :reader material-index)))

(defstruct-g vertex-bone-data
  (bone-ids :ivec4)
  (weights :vec4))

(defun scene-meshes->gpu (scene)
  (map 'list #'mesh->gpu (ai:meshes scene)))

(defun mesh->offset-list (mesh)
  (map 'list #'ai:offset-matrix (ai:bones mesh)))

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
         (ids (map '(vector vari.types:v-int)
                   #'bone-vertex-id extended))
         (weights (map '(vector single-float)
                       #'bone-vertex-weight extended)))
    ;; I'm not really a big fan of using a nested list instead of a
    ;; struct here but it works better with Cepl's make-gpu-array
    (list ids weights)))

(defun list->array (list)
  (make-array (length list) :initial-contents list))

(defun mesh->vertex-bone-data-list (mesh)
  (loop
     with num-vertices = (length (ai:vertices mesh))
     with vertex-bones = (loop repeat num-vertices
                            collect () into bone-list
                            finally (return (list->array bone-list)))
     for bone across (ai:bones mesh)
     for bone-index upfrom 0
     do (loop
           for vertex-weight across (ai:weights bone)
           for vertex-id = (ai:id vertex-weight)
           do (push (make-bone-vertex :id vertex-id
                                      :weight (ai:weight vertex-weight))
                    (aref vertex-bones vertex-id)))
     finally (return (map 'list
                          #'transpose-bone-vertex-list
                          vertex-bones))))

(defun mesh->gpu (mesh)
  (let* ((verts (ai:vertices mesh))
         (norms (ai:normals mesh))
         (tcs (ai:texture-coords mesh))
         (texcoords (when (> (length tcs) 0) (aref tcs 0)))
         (faces (ai:faces mesh))
         (n-len (length norms))
         (tc-len (length texcoords))
         (vertex-type (calc-type verts norms texcoords))
         (set (remove nil `(,verts
                            ,@(when n-len (list norms))
                            ,@(when tc-len (list texcoords))))))
    (make-instance 'mesh-data
                   :vertex-data (make-gpu-array (apply #'map 'list #'list set)
                                                :element-type vertex-type)
                   :bone-data (make-gpu-array (mesh->vertex-bone-data-list mesh)
                                              :element-type 'vertex-bone-data)
                   :indices (make-gpu-array (loop for % across faces
                                               append (coerce % 'list))
                                            :element-type :ushort)
                   :material-index (ai:material-index mesh)
                   :primitive-type :triangles)))

(defun calc-type (verts norms texcoords)
  (apply #'cepl-utils:symb-package :cepl
         (remove nil (cons :g- (list (and verts :p)
                                     (and (> (length norms) 0) :n)
                                     (and (> (length texcoords) 0) :t))))))
