;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: packages.lisp
;;;; ------------------------------------------------------------------------
;;;; Author: Sascha K. Biermanns, <skkd PUNKT h4k1n9 AT yahoo PUNKT de>
;;;; Lizenz: ISC
;;;; Copyright (C) 2015 Sascha K. Biermanns
;;;; Permission to use, copy, modify, and/or distribute this software for any
;;;; purpose with or without fee is hereby granted, provided that the above
;;;; copyright notice and this permission notice appear in all copies.
;;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;; ------------------------------------------------------------------------

(require :cl-hilfsroutinen)

(defpackage #:cl-rollenspiel
  (:nicknames :rollenspiel :rpg)
  (:use :common-lisp)
  (:export
   #:w4
   #:w6
   #:w8
   #:w10
   #:w12
   #:w20
   #:w100
   ))



