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


;;; #########################
;;; # Wird NICHT exportiert #
;;; #########################


(defun wurfserie (&optional (anzahl 1) (seiten 6))
  "HILFS-FUNKTION: Simuliert mehrere Würfe mit einer Anzahl Würfeln"
  (if (plusp anzahl)
	(do ((i 0 (1+ i))
		 (summe 0)
         geworfene-augen)
		((= i anzahl)
		 (values summe geworfene-augen))
      (let ((wert (würfelwurf seiten)))
        (incf summe wert)
        (push wert geworfene-augen)))
    (error "~&Der Aufruf erfolgt mittels (WURFSERIE ANZAHL) oder (WURFSERIE ANZAHL SEITEN), wobei ANZAHL und SEITEN positive Integerzahlen darstellen müssen.")))


;;; ###################
;;; # Wird EXPORTIERT #
;;; ###################


(defun w4 (&optional (anzahl 1))
  "FUNKTION: Simuliert ANZAHL Würfe mit einem vierseitigen Würfel"
  (wurfserie anzahl 4))


(defun w6 (&optional (anzahl 1))
  "FUNKTION: Simuliert ANZAHL Würfe mit einem sechsseitigen Würfel"
  (wurfserie anzahl 6))


(defun w8 (&optional (anzahl 1))
  "FUNKTION: Simuliert ANZAHL Würfe mit einem achtseitigen Würfel"
  (wurfserie anzahl 8))


(defun w10 (&optional (anzahl 1))
  "FUNKTION: Simuliert ANZAHL Würfe mit einem zehnseitigen Würfel"
  (wurfserie anzahl 10))


(defun w12 (&optional (anzahl 1))
  "FUNKTION: Simuliert ANZAHL Würfe mit einem zwölfseitigen Würfel"
  (wurfserie anzahl 12))


(defun w20 (&optional (anzahl 1))
  "FUNKTION: Simuliert ANZAHL Würfe mit einem zwanzigseitigen Würfel"
  (wurfserie anzahl 20))


(defun w100 (&optional (anzahl 1))
  "FUNKTION: Simuliert ANZAHL Würfe mit einem 100seitigen Würfel"
  (wurfserie anzahl 100))


(defun würfelpool (&optional (anzahl 1) (schwellenwert 1))
  "FUNKTION: Würfelt einen Würfelpool mit ANZAHL sechsseitiger Würfel, wobei ein SCHWELLENWERT an geworfenen Fünfen oder Sechsen als Erfolg gibt.
Die Funktion liefert folgende Rückgabewerte in der aufgeführten Reihenfolge zurück:
- Gesammtsumme aller Würfelaugen
- Eine Liste aller geworfenen Augen
- boolsche Antwort, ob ein Erfolg vorliegt
- ANZAHL Nettoerfolge, wenn vorhanden, sonst NIL
- boolsche Antwort, ob ein Patzer vorliegt
- boolsche Antwort, ob ein kritischer Patzer vorliegt"
  (multiple-value-bind (wert würfel) (w6 anzahl)
    (let ((erfolge 0)
          (einser 0)
          erfolg
          nettoerfolge
          patzer
          kritischer-patzer)
      (dolist (wurf würfel)
        (when (>= wurf 5)
          (incf erfolge))
        (when (= wurf 1)
          (incf einser)))
      (when (>= erfolge schwellenwert)
        (setf erfolg 't))
      (setf nettoerfolge (- erfolge schwellenwert))
      (when (> einser (truncate (/ anzahl 2)))
        (setf patzer 't))
      (when (= einser anzahl)
        (setf kritischer-patzer 't))
      (values wert würfel erfolg (if (plusp nettoerfolge) nettoerfolge) patzer kritischer-patzer))))
        
