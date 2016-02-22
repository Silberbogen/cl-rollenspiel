;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: cl-rollenspiel.lisp
;;;; Beschreibung: Routinen zur Unterstützung bei Rollenspielen
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


(in-package #:cl-rollenspiel)
(use-package :cl-hilfsroutinen)


;;; #########################
;;; # Wird NICHT exportiert #
;;; #########################


(defun wurfserie (&optional (anzahl 1) (seiten 6))
  "HILFS-FUNKTION: Simuliert mehrere Würfe mit einer Anzahl Würfeln"
  (check-type anzahl (integer 1 *))
  (check-type seiten (integer 1 *))
  (do ((i 0 (1+ i))
	   (summe 0)
	   geworfene-augen)
	  ((= i anzahl)
	   (values summe geworfene-augen))
	(let ((wert (würfelwurf seiten)))
	  (incf summe wert)
	  (push wert geworfene-augen))))


;;; ###################
;;; # Wird EXPORTIERT #
;;; ###################


(defun auswahl (orte ctrl &rest args &aux (anzahl (length orte)))
  "AUSWAHL empfängt eine Liste von Funktionen in ORTE und einen Abfragestring CTRL samt eventueller Argumente in ARGS und liefert als Rückgabewert die gewünschte Funktion aus ORTE zurück."
  (check-type orte list)
  (check-type ctrl string)
  (let ((auswahl (apply #'zahlen-auswahl anzahl ctrl args)))
	(elt orte (1- auswahl))))


(defun w4 (&optional (anzahl 1))
  "FUNKTION: Simuliert ANZAHL Würfe mit einem vierseitigen Würfel"
  (check-type anzahl (integer 1 *))
  (wurfserie anzahl 4))


(defun w6 (&optional (anzahl 1))
  "FUNKTION: Simuliert ANZAHL Würfe mit einem sechsseitigen Würfel"
  (check-type anzahl (integer 1 *))
  (wurfserie anzahl 6))


(defun w8 (&optional (anzahl 1))
  "FUNKTION: Simuliert ANZAHL Würfe mit einem achtseitigen Würfel"
  (check-type anzahl (integer 1 *))
  (wurfserie anzahl 8))


(defun w10 (&optional (anzahl 1))
  "FUNKTION: Simuliert ANZAHL Würfe mit einem zehnseitigen Würfel"
  (check-type anzahl (integer 1 *))
  (wurfserie anzahl 10))


(defun w12 (&optional (anzahl 1))
  "FUNKTION: Simuliert ANZAHL Würfe mit einem zwölfseitigen Würfel"
  (check-type anzahl (integer 1 *))
  (wurfserie anzahl 12))


(defun w20 (&optional (anzahl 1))
  "FUNKTION: Simuliert ANZAHL Würfe mit einem zwanzigseitigen Würfel"
  (check-type anzahl (integer 1 *))
  (wurfserie anzahl 20))


(defun w100 (&optional (anzahl 1))
  "FUNKTION: Simuliert ANZAHL Würfe mit einem 100seitigen Würfel"
  (check-type anzahl (integer 1 *))
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
  (check-type anzahl (integer 1 *))
  (check-type schwellenwert (integer 1 *))
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


(defun zahlen-auswahl (max ctrl &rest args)
  "ZAHLEN-AUSWAHL gibt den String CTRL samt eventueller Argumente ARGS aus und erzwingt die Eingabe einer Zahl zwischen 1 und MAX. Diese Zahl wird zurückgeliefert."
  (do ((danach nil t)
	   (ctrl (concatenate 'string ctrl " > ")))
	  (nil)
	(when danach
	  (format *query-io* "~&Bitte gib eine Zahl zwischen 1 und ~A ein.~%" max))
	(apply #'format *query-io* ctrl args)
	(let* ((antw (string-trim " " (read-line *query-io*))))
	  (unless (string-equal "" (nur-ziffern antw))
		(let ((antwort (parse-integer (nur-ziffern antw))))
		  (if (and (> antwort 0) (<= antwort max))
			  (return-from zahlen-auswahl antwort)))))))

