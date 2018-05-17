;;;; untitled-lisp-game.asd

;; Something with program-op is not working...
#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable T :compression T))

(asdf:defsystem #:untitled-lisp-game
  :description "Describe untitled-lisp-game here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-opengl
               #:glkit
               #:sdl2kit
               #:mathkit
               #:sb-cga
               #:swank
               #:classimp
               #:cl-soil
	       #:array-operations
	       #:static-vectors)
  :serial t
  :components ((:file "package")
               (:file "untitled-lisp-game")
               (:file "meshes")
               (:file "textures"))
  :build-operation "asdf:program-op"
  :build-pathname "build/untitled-lisp-game"
  :entry-point "untitled-lisp-game:launch")

