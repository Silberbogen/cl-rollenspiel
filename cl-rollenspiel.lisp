;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: cl-rollenspiel.lisp
;;;; Beschreibung: Routinen zur Unterstützung bei Rollenspielen
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


(in-package #:cl-rollenspiel)
(use-package :cl-hilfsroutinen)


(defun w4 (&optional (anzahl 1))
  "Simuliert Würfe mit einem vierseitigen Würfel"
  (wurfserie anzahl 4))



(defun w6 (&optional (anzahl 1))
  "Simuliert Würfe mit einem sechsseitigen Würfel"
  (wurfserie anzahl 6))



(defun w8 (&optional (anzahl 1))
  "Simuliert Würfe mit einem achtseitigen Würfel"
  (wurfserie anzahl 8))



(defun w10 (&optional (anzahl 1))
  "Simuliert Würfe mit einem zehnseitigen Würfel"
  (wurfserie anzahl 10))



(defun w12 (&optional (anzahl 1))
  "Simuliert Würfe mit einem zwölfseitigen Würfel"
  (wurfserie anzahl 12))



(defun w20 (&optional (anzahl 1))
  "Simuliert Würfe mit einem zwanzigseitigen Würfel"
  (wurfserie anzahl 20))



(defun w100 (&optional (anzahl 1))
  "Simuliert Würfe mit einem 100seitigen Würfel"
  (wurfserie anzahl 100))



(defun wurfserie (&optional (anzahl 1) (seiten 6))
  "Simuliert mehrere Würfe mit einer Anzahl Würfeln"
  (when (plusp anzahl)
	(do ((i 0 (1+ i))
		 (summe 0)
         geworfene-augen)
		((= i anzahl)
		 (values summe geworfene-augen))
      (let ((wert (würfelwurf seiten)))
        (incf summe wert)
        (push wert geworfene-augen)))))
