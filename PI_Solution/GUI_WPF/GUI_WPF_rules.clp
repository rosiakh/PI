;;;*****************
;;;* Configuration *
;;;*****************
   
(defglobal ?*target* = gui) ; console, cgi, or gui

;;; ***************************
;;; * DEFTEMPLATES & DEFFACTS *
;;; ***************************

(deftemplate MAIN::text-for-id
   (slot id)
   (slot text))

(deftemplate UI-state
   (slot id (default-dynamic (gensym*)))
   (slot display)
   (slot relation-asserted (default none))
   (slot response (default none))
   (multislot valid-answers)
   (multislot display-answers)
   (slot state (default middle)))
   
;;;***************************
;;;* DEFFUNCTION DEFINITIONS *
;;;***************************

(deffunction MAIN::find-text-for-id (?id)
   ;; Search for the text-for-id fact
   ;; with the same id as ?id
   (bind ?fact
      (find-fact ((?f text-for-id))
                  (eq ?f:id ?id)))
   (if ?fact
      then
      (fact-slot-value (nth$ 1 ?fact) text)
      else
      ?id))
      
(deffunction MAIN::translate-av (?values)
   ;; Create the return value
   (bind ?result (create$))
   ;; Iterate over each of the allowed-values
   (progn$ (?v ?values)
      ;; Find the associated text-for-id fact
      (bind ?nv
         (find-text-for-id ?v))
      ;; Add the text to the return value
      (bind ?result (create$ ?result ?nv)))
   ;; Return the return value
   ?result)

(deffunction MAIN::replace-spaces (?str)
   (bind ?len (str-length ?str))
   (bind ?i (str-index " " ?str))
   (while (neq ?i FALSE)
      (bind ?str (str-cat (sub-string 1 (- ?i 1) ?str) "-" (sub-string (+ ?i 1) ?len ?str)))
      (bind ?i (str-index " " ?str)))
   ?str)

(deffunction MAIN::sym-cat-multifield (?values)
   (bind ?rv (create$))
   (progn$ (?v ?values)
      (bind ?rv (create$ ?rv (sym-cat (replace-spaces ?v)))))
   ?rv)

(deffunction MAIN::multifield-to-delimited-string (?mv ?delimiter)
   (bind ?rv "")
   (bind ?first TRUE)
   (progn$ (?v ?mv)
      (if ?first
         then
         (bind ?first FALSE)
         (bind ?rv (str-cat ?v))
         else
         (bind ?rv (str-cat ?rv ?delimiter ?v))))
   ?rv)

;;;*****************
;;;* STATE METHODS *
;;;*****************
      
;;; GUI target (iOS and JNI)

(defmethod handle-state ((?state SYMBOL (eq ?state greeting))
                         (?target SYMBOL (eq ?target gui))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?valid-answers MULTIFIELD))
   (assert (UI-state (display ?message)
                     (relation-asserted greeting)
                     (state ?state)
                     (valid-answers yes)
                     (display-answers yes)))
   (halt))		;(halt) zatrzymuje wynokywanie i wraca  "run" - agenda pozostaje bez zmian - można kontynouwać kolejnym "run"

(defmethod handle-state ((?state SYMBOL (eq ?state interview))
                         (?target SYMBOL (eq ?target gui))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?response PRIMITIVE)
                         (?valid-answers MULTIFIELD)
                         (?display-answers MULTIFIELD))
   (assert (UI-state (display ?message)
                     (relation-asserted ?relation-asserted)
                     (state ?state)
                     (response ?response)
                     (valid-answers ?valid-answers)
                     (display-answers ?display-answers)))
   (halt))
 
(defmethod handle-state ((?state SYMBOL (eq ?state conclusion))
                         (?target SYMBOL (eq ?target gui))
                         (?display LEXEME))
   (assert (UI-state (display ?display)
                     (state ?state)
                     (valid-answers)
                     (display-answers)))
   (assert (conclusion))
   (halt))

;============================ IZW WYWOŁANE PRZEZ GRONKOWCE ====================

;============================ START RULE ======================================

(defrule system-banner ""
  (not (greeting yes))
  =>
  (handle-state greeting
                ?*target*
                (find-text-for-id WelcomeMessage)
                greeting
                (create$)))

;============================ QUERY RULES =====================================

(defrule etiologia ""

   (greeting yes)
   (not (conclusion))

   =>

   (bind ?answers (create$ gronkowiec paciorkowiec nieznany))
   (handle-state interview
                 ?*target*
                 (find-text-for-id etiologia)
				 czynnik-etiologiczny
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

;============================ PACIORKOWIEC ====================================

(defrule P-czy-pelna-wrazliwosc-na-penicyline ""

   (czynnik-etiologiczny paciorkowiec)
   (not (conclusion))

   =>

   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id P_czy_pelna_wrazliwosc_na_penicyline)
				 P-wrazliwosc-na-penicyline
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule P-czy-alegria-na-laktamowe ""

   (or
		(P-wrazliwosc-na-penicyline no)
		(P-wrazliwosc-na-penicyline yes))
   (not (conclusion))

   =>

   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id P_czy_alergia_na_laktamowe)
                 P-alergia-na-laktamowe
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule P-czy-proteza-zastawkowa ""

   (P-wrazliwosc-na-penicyline yes)
   (P-alergia-na-laktamowe no)
   (not (conclusion))
   
   =>

   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id P_czy_proteza_zastawkowa)
                 P-proteza-zastawkowa
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

;============================ PACIORKOWIEC CONCLUSIONS =====================================

(defrule P-conclusion-1 ""
	(declare (salience 10))
	(P-alergia-na-laktamowe no)
	(P-proteza-zastawkowa yes)
	(P-wrazliwosc-na-penicyline yes)
	=>
	(handle-state conclusion ?*target* (find-text-for-id P_conclusion_1)))

(defrule P-conclusion-2 ""
	(declare (salience 10))
	(P-alergia-na-laktamowe no)
	(P-proteza-zastawkowa no)
	(P-wrazliwosc-na-penicyline yes)
	=>
	(handle-state conclusion ?*target* (find-text-for-id P_conclusion_2)))

(defrule P-conclusion-3 ""
	(declare (salience 10))
	(P-alergia-na-laktamowe yes)
	(P-wrazliwosc-na-penicyline yes)
	=>
	(handle-state conclusion ?*target* (find-text-for-id P_conclusion_3)))

(defrule P-conclusion-4 ""
	(declare (salience 10))
	(P-alergia-na-laktamowe no)
	(P-wrazliwosc-na-penicyline no)
	=>
	(handle-state conclusion ?*target* (find-text-for-id P_conclusion_4)))

(defrule P-conclusion-5 ""
	(declare (salience 10))
	(P-alergia-na-laktamowe yes)
	(P-wrazliwosc-na-penicyline no)
	=>
	(handle-state conclusion ?*target* (find-text-for-id P_conclusion_5)))

;============================ GRONKOWIEC ======================================

(defrule G-czy-proteza ""

   (czynnik-etiologiczny gronkowiec)
   (not (conclusion))

   =>

   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id G_czy_proteza)
				 G-proteza
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule G-czy-mssa-zastawki-wlasnej ""

   (G-proteza no)
   (not (conclusion))

   =>

   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id G_czy_mssa_zastawki_wlasnej)
                 G-mssa-zastawki-wlasnej
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule G-czy-mssa-protezy ""

   (G-proteza yes)
   (not (conclusion))
   
   =>

   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id G_czy_mssa_protezy)
                 G-mssa-protezy
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

;============================ GRONKOWIEC CONCLUSIONS =====================================

(defrule G-conclusion-1 ""
	(declare (salience 10))
	(G-mssa-zastawki-wlasnej yes)
	=>
	(handle-state conclusion ?*target* (find-text-for-id G_conclusion_1)))

(defrule G-conclusion-2 ""
	(declare (salience 10))
	(G-mssa-zastawki-wlasnej no)
	=>
	(handle-state conclusion ?*target* (find-text-for-id G_conclusion_2)))

(defrule G-conclusion-3 ""
	(declare (salience 10))
	(G-mssa-protezy yes)
	=>
	(handle-state conclusion ?*target* (find-text-for-id G_conclusion_3)))

(defrule G-conclusion-4 ""
	(declare (salience 10))
	(G-mssa-protezy no)
	=>
	(handle-state conclusion ?*target* (find-text-for-id G_conclusion_4)))

;============================ NIEZNANY ====================================

(defrule N-czy-proteza ""

   (czynnik-etiologiczny nieznany)
   (not (conclusion))

   =>

   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id N_czy_proteza)
				 N-proteza
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule N-czy-alegria-na-laktamowe ""

   (N-proteza no)
   (not (conclusion))

   =>

   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id N_czy_alergia_na_laktamowe)
                 N-alergia-na-laktamowe
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule N-czy-wczesne-IZW ""

   (N-proteza yes)
   (not (conclusion))
   
   =>

   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id N_czy_wczesne_IZW)
                 N-wczesne-IZW
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

;============================ NIEZNANY CONCLUSIONS =====================================

(defrule N-conclusion-1 ""
	(declare (salience 10))
	(N-alergia-na-laktamowe no)
	=>
	(handle-state conclusion ?*target* (find-text-for-id N_conclusion_1)))

(defrule N-conclusion-2 ""
	(declare (salience 10))
	(N-alergia-na-laktamowe yes)
	=>
	(handle-state conclusion ?*target* (find-text-for-id N_conclusion_2)))

(defrule N-conclusion-3 ""
	(declare (salience 10))
	(N-wczesne-IZW yes)
	=>
	(handle-state conclusion ?*target* (find-text-for-id N_conclusion_3)))

(defrule N-conclusion-4 ""
	(declare (salience 10))
	(N-wczesne-IZW no)
	=>
	(handle-state conclusion ?*target* (find-text-for-id N_conclusion_4)))