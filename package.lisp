;;;; package.lisp

(uiop:define-package untitled-lisp-game
    (:use #:cl #:cepl #:rtg-math
          :livesupport)
  (:export :launch))

(uiop:define-package :untitled-lisp-game.meshes
    (:use :cl :cepl #:rtg-math)
  (:export :load-file
           :render
           :parts
           :samplers))
