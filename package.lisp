;;;; package.lisp

(uiop:define-package untitled-lisp-game
    (:use #:cl #:cepl #:rtg-math
          :livesupport))

(uiop:define-package :untitled-lisp-game.model-parsers
  (:use :cl :cepl #:rtg-math)
  (:export :load-file
           :meshes->lists
           :mesh->lists
           :mesh-list->gpu
           :mesh->gpu
           :scene-meshes->gpu
           :calc-type))
