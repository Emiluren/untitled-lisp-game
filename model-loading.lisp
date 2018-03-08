(in-package :untitled-lisp-game.meshes)

(defclass mesh-data ()
  ((vertices :initarg :vertices :reader vertices)
   (indices :initarg :indices :reader indices)
   (primitive-type :initarg :primitive-type :reader primitive-type)))

(defun meshes->lists (scene)
  (map 'list #'mesh->lists (ai:meshes scene)))

(defun mesh->lists (mesh)
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
    `((:data ,(calc-type v n tc)
             ,(apply #'map 'list #'list set))
      (:indices ,(loop :for % :across f :append (coerce % 'list)))
      (:materials ))))

(defun calc-type (v n tc)
  (apply #'cepl-utils:symb-package :cepl
         (remove nil (cons :g- (list (and v :p)
                                     (and (> (length n) 0) :n)
                                     (and (> (length tc) 0) :t))))))
(defun mesh-list->gpu (mesh-list)
  (let ((vertices (destructuring-bind (_ type data) (assoc :data mesh-list)
                    (declare (ignore _))
                    (make-gpu-array data :element-type type)))
        (indices (destructuring-bind (_ data) (assoc :indices mesh-list)
                   (declare (ignore _))
                   (make-gpu-array data :element-type :ushort))))
    (make-instance 'mesh-data
                   :vertices vertices
                   :indices indices)))

(defun scene-meshes->gpu (scene)
  (mapcar #'mesh-list->gpu (meshes->lists scene)))

