;;;; untitled-lisp-game.asd

(asdf:defsystem #:untitled-lisp-game
  :description "Describe untitled-lisp-game here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cepl
               #:cepl.sdl2
               #:swank
               #:livesupport
               #:cepl.skitter.sdl2
               #:classimp
               #:dirt)
  :serial t
  :components ((:file "package")
               (:file "untitled-lisp-game")
               (:file "meshes")))

