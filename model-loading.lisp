(in-package :untitled-lisp-game.meshes)

(defclass mesh-data ()
  ((vertex-data :initarg :vertex-data :reader vertex-data)
   (indices :initarg :indices :reader indices)
   (primitive-type :initarg :primitive-type :reader primitive-type)
   (material-index :initarg :material-index :reader material-index)))

(defun scene-meshes->gpu (scene)
  (map 'list #'mesh->gpu (ai:meshes scene)))

(defun mesh->gpu (mesh)
  (let* ((v (ai:vertices mesh))
         (n (ai:normals mesh))
         (tcs (ai:texture-coords mesh))
         (tc (when (> (length tcs) 0) (aref tcs 0)))
         (f (ai:faces mesh))
         (n-len (length n))
         (tc-len (length tc))
         (set (remove nil `(,v
                            ,@(when n-len (list n))
                            ,@(when tc-len (list tc))))))
    (make-instance 'mesh-data
                   :vertex-data (make-gpu-array (apply #'map 'list #'list set)
                                                :element-type (calc-type v n tc))
                   :indices (make-gpu-array (loop for % across f append (coerce % 'list))
                                            :element-type :ushort)
                   :material-index (ai:material-index mesh)
                   :primitive-type :triangles)))

(defun calc-type (v n tc)
  (apply #'cepl-utils:symb-package :cepl
         (remove nil (cons :g- (list (and v :p)
                                     (and (> (length n) 0) :n)
                                     (and (> (length tc) 0) :t))))))
