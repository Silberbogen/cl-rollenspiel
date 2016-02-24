;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: packages.lisp
;;;; ------------------------------------------------------------------------
;;;; Author: Sascha K. Biermanns, <skkd PUNKT h4k1n9 AT yahoo PUNKT de>
;;;; Lizenz: GPL v3
;;;; Copyright (C) 2011-2015 Sascha K. Biermanns
;;;; This program is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the
;;;; Free Software Foundation; either version 3 of the License, or (at your
;;;; option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;;;; Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License along
;;;; with this program; if not, see <http://www.gnu.org/licenses/>. 
;;;; ------------------------------------------------------------------------


(require :cl-hilfsroutinen)


(defpackage #:cl-rollenspiel
  (:nicknames :rollenspiel :rpg)
  (:use :common-lisp)
  (:export
   #:auswahl
   #:ereignis
   #:erhöhe
   #:inventar
   #:kampf
   #:mahlzeit
   #:person
   #:spieler
   #:versuche-dein-glück
   #:w4
   #:w6
   #:w8
   #:w10
   #:w12
   #:w20
   #:w100
   #:würfelpool
   #:zahlen-auswahl
   ))



