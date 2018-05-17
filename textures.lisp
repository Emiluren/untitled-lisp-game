(in-package :untitled-lisp-game.textures)

;; Not actually used but fun to write :P
(defmacro doto (object &rest body)
  (let ((value-name (gensym)))
    `(let ((,value-name ,object))
       ,@(loop for sexp in body
            collect `(,(first sexp) ,value-name ,@(rest sexp)))
       ,value-name)))

(defun scene->texture-files (scene)
  (loop
     for table across (ai:materials scene)
     collect (caddr (assoc :ai-texture-type-diffuse
                          (gethash "$tex.file" table)))))

(defun load-texture (filename)
  (let ((path (merge-pathnames filename "assets/")))
    (multiple-value-bind (data-ptr width height)
        (cl-soil:load-image path)
      (let ((texture (make-instance 'kit.gl.tex:texture
                                    :size (vector width height))))
        (kit.gl.tex:tex-image-2d texture :data data-ptr)
        (cl-soil:free-image-data data-ptr)
        texture))))
