;;;; package.lisp

;; I have no idea what I'm doing here. To intern keywords or not to
;; intern, that is the question...

(uiop:define-package :untitled-lisp-game
    (:nicknames :ulg)
  (:use #:cl #:kit.sdl2 #:kit.gl #:kit.gl.vao #:kit.gl.shader)
  (:export #:launch))

(uiop:define-package :untitled-lisp-game.meshes
    (:nicknames :ulg.meshes)
  (:use #:cl #:kit.math #:kit.gl #:kit.gl.vao #:kit.gl.shader)
  (:export #:load-file
           #:render
           #:parts
           #:samplers))

(uiop:define-package :untitled-lisp-game.textures
    (:nicknames :ulg.textures)
  (:use #:cl)
  (:export #:scene->texture-files
           #:load-texture))
