(in-package :untitled-lisp-game.meshes)

(defun scene->texture-files (scene)
  (loop
     for table across (ai:materials scene)
     collect (caddr (assoc :ai-texture-type-diffuse
                          (gethash "$tex.file" table)))))

(defun load-texture (filename)
  (sample (dirt:load-image-to-texture (merge-pathnames filename "assets/"))))
