;;;; opengl-test.asd

(asdf:defsystem #:opengl-test
  :description "Describe opengl-test here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-opengl
               #:cl-glfw3
               #:3d-vectors
               #:3d-matrices
               #:classimp
               #:cl-soil
               #:alexandria
               #:trivial-main-thread)
  :serial t
  :components ((:file "package")
               (:file "opengl-test")))

