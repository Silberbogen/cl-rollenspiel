;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; cl-hilfsroutinen.asd

(asdf:defsystem #:cl-rollenspiel
  :description "Routinen, die bei der Erstellung eines Rollenspiels helfen"
  :author "Sascha Biermanns <skkd PUNKT h4k1n9 AT yahoo PUNKT de>"
  :license "ISC"
  :serial t
  :components ((:static-file "LICENSE")
               (:file "package")
			   (:file "cl-rollenspiel")))

