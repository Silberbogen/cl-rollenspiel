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


;;; ######################
;;; # Hier wird's global #
;;; ######################


(defparameter *ereignis* (make-hash-table)
  "Hält fest, ob gewisse Ereignisse eingetroffen sind")


(defparameter *inventar* (make-hash-table)
  "Das Inventar des Spielers")


(defparameter *person* (make-hash-table)
  "Verwaltet Aufenthaltsorte anderer Personen")


(defparameter *spieler* (make-hash-table)
  "Die Charakterwerte des Spielers")


(defparameter *zug* 'nil
  "Speichert den Verlauf des Weges, den der Spieler nimmt")


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


(defun ereignis (key &optional wert)
  "EREIGNIS dient dazu, Marker zu Ereignissen zu erstellen und abzufragen. Zum Abfragen die der Syntax (EREIGNIS SCHLÜSSEL), zum Erstellen/Ändern eines Ereignisses werden lautet die Synatx (EREIGNIS SCHLÜSSEL WERT)."
  (if (null wert)
	  (gethash key *ereignis* nil)
	  (setf (gethash key *ereignis* nil) wert)))


(defun erhöhe (key wert)
  "ERHÖHE dient dazu, die 6 Charakterwerte eines Spielers separat zu behandeln: GEWANDHEIT, STÄRKE, GLÜCK - und ihre Maximalwert: MAX-GEWANDHEIT, MAX-GLÜCK, MAX-STÄRKE. Die Syntax ist hierbei unterschiedlich. Während sich aus dem Spielablauf ergibt, das GEWANDHEIT, GLÜCK und STÄRKE um einen Betrag erhöht werden, wird bei ihren MAX-Gegenstücken ein neuer Wert Absolutwert gesetzt."
  (case key
	(gewandheit
	 (let ((max (spieler 'max-gewandheit)))
	   (if (<= (+ (spieler 'gewandheit) 4) max)
		   (spieler 'gewandheit wert)
		   (spieler 'gewandheit max))))
	(glück
	 (let ((max (spieler 'max-glück)))
	   (if (<= (+ (spieler 'glück) 4) max)
		   (spieler 'glück wert)
		   (spieler 'glück max))))
	(stärke
	 (let ((max (spieler 'max-stärke)))
	   (if (<= (+ (spieler 'stärke) 4) max)
		   (spieler 'stärke wert)
		   (spieler 'stärke max))))
	(max-gewandheit
	 (spieler 'max-gewandheit wert))
	(max-glück
	 (spieler 'max-glück wert))
	(max-stärke
	 (spieler 'max-stärke wert))))


(defun inventar (key &optional (wert 0))
  (typecase wert
	(number
	 (if (zerop wert)
		 (gethash key *inventar* 0)
		 (let ((aktuell (gethash key *inventar* 0)))
		   (cond ((zerop aktuell)
				  (values nil 0))
				 ((< (+ aktuell wert) 0)
				  (values nil '<0))
				 ((zerop (+ aktuell wert))
				  (remhash key *inventar*)
				  (values t 0))
				 (t
				  (values t (incf (gethash key *inventar* 0) wert)))))))
	(otherwise
	 (setf (gethash key *inventar* nil) wert))))


(defun kampf (gegner &optional flucht treffer-verboten)
  (flet ((kampfrunde (gegner)
		   (let ((g1 (+ (spieler 'gewandheit) (w6 2) (spieler 'angriffsbonus)))
				 (g2 (+ (third gegner) (w6 2))))
			 (when (ereignis 'unsichtbar)
			   (incf g1 2))
			 (when (= g1 g2)
			   (return-from kampfrunde 't)) ; Unentschieden, nicht getroffen
			 (textausgabe "~A: Stärke ~A, Gewandheit ~A, Glück ~A" (spieler 'name)
						  (spieler 'stärke) (spieler 'gewandheit) (spieler 'glück))
			 (textausgabe "~A: Stärke ~A, Gewandheit ~A" (first gegner)
						  (second gegner) (third gegner))
			 (when flucht
			   (if (j-oder-n-p "Möchtest du die Möglichkeit nutzen und fliehen?")
				   (return-from kampf flucht)))
			 (let* ((glückstest (and (plusp (spieler 'glück))
									 (j-oder-n-p "Möchtest du dein Glück versuchen?")))
					(wirklichglück (and glückstest (versuche-dein-glück))))
			   (when (> g1 g2) ; Der Spieler geht als Sieger hervor
				 (if (and (ereignis 'nur-silber-trifft) (not (inventar 'silberwaffe)))
					 (return-from kampfrunde 't)) ; Das Monster ist imun - dumm
				 (if (ereignis 'unsichtbar)
					 (decf (second gegner) 2)) ; Unsichtbare treffen besser ;)
				 (when (and (or (plusp (inventar 'gewehr)) (plusp (inventar 'pistole)))
							(plusp (inventar 'patronen)))
				   (decf (second gegner))
				   (inventar 'patronen -1))
				 (when glückstest
				   (if wirklichglück
					   (decf (second gegner) 4)
					   (decf (second gegner)))
				   (return-from kampfrunde 't))
				 (decf (second gegner) 2)
				 (return-from kampfrunde 't))
			   (when (< g1 g2)
				 (let ((schildbonus (if (and (plusp (inventar 'schild)) (= (w6) 6)) 1 0)))
				   (when (ereignis 'unsichtbar)
					 (let ((wurf (w6)))
					   (if (or (= wurf 2) (= wurf 4))
						   (if (< (spieler 'stärke) (spieler 'max-stärke))
							   (spieler 'stärke 1))
						   (if (= wurf 6)
							   (return-from kampfrunde 't)))))
				   (when glückstest
					 (if wirklichglück
						 (spieler 'stärke (+ -1 schildbonus))
						 (spieler 'stärke (+ -4 schildbonus)))
					 (return-from kampfrunde 'nil))
				   (spieler 'stärke (+ -2 schildbonus))
				   (return-from kampfrunde 'nil)))))
		   't)
		 (gesamt-stärke ()
		   (let ((accu 0))
			 (dolist (i gegner accu)
			   (incf accu (second i))))))
	(do ((kein-treffer-erhalten 't))
		((or (<= (spieler 'stärke) 0) (<= (gesamt-stärke) 0)))
	  (dolist (i gegner)
		(if (plusp (spieler 'stärke))
			(if (plusp (second i))
				(progn
				  (setf kein-treffer-erhalten (kampfrunde i))
				  (if (and treffer-verboten (not kein-treffer-erhalten))
					  (return-from kampf 'nil))))
			(return-from kampf 'nil))))
	(when (plusp (spieler 'stärke))
	  't)))


(defun mahlzeit ()
  (when (j-oder-n-p "Möchtest du eine Mahlzeit zu dir nehmen")
	(if (plusp (inventar 'proviant))
		(progn
		  (textausgabe "Nachdem du dich versichert hast, das niemand in der Nähe ist, entnimmst du ein Proviantpaket aus deinem Rucksack. Genüsslich und so leise wie möglich verzehrst du es. Du kannst spüren, wie etwas Kraft in deinen müden Körper zurückkehrt.")
		  (inventar 'proviant -1)
		  (erhöhe 'stärke 4))
		(textausgabe "Nachdem du dich versichert hast, das niemand in der Nähe ist, durchwühlst du deinen Rucksack auf der Suche nach einem Proviantpaket. Nach einigen Minuten und mehrfachem aus- und einpacken des Rucksacks gibst du verzweifelt auf. Es ist tatsächlich kein einziger Brotkrummen mehr übrig."))))


(defun person (key &optional wert)
  (if (null wert)
	  (gethash key *person* nil)
	  (setf (gethash key *person* nil) wert)))


(defun spieler (key &optional (wert 0))
  (typecase wert
	(number
	 (if (zerop wert)
		 (gethash key *spieler* 0)
		 (incf (gethash key *spieler* 0) wert)))
	(otherwise
	 (setf (gethash key *spieler* nil) wert))))


(defun versuche-dein-glück ()
  (let ((glück (spieler 'glück)))
	(when (plusp glück)
	  (spieler 'glück -1)
	  (if (<= (w6 2) glück)
			't))))


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

