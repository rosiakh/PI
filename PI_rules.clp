(defrule start
=>
	(printout t "Podaj czynnik etiologiczny sposrod: gronkowce, paciorkowce, nieznany" crlf)
	(assert (czynnik-etiologiczny (read))))
	
;=========================IZW wywolane przez paciorkowce==================

(defrule IZW-wywolane-przez-paciorkowce-P
	?x <- (czynnik-etiologiczny paciorkowce)
=>
	(retract ?x)
	(printout t "Czy wystepuje pelna wrazliwosc na penicyline?" crlf)
	(retract ?x)
	(assert (pelna-wrazliwosc (read))))
	
; LEWA CZesc

(defrule pelna-wrazliwosc-na-penicyline-P
	?x <- (pelna-wrazliwosc tak)
=>
	(retract ?x)
	(printout t "Czy wystepuja alergia na antybiotyki b-laktamowe?" crlf)
	(assert (alergia-na-antybiotyki-b-laktamowe-PL read)))
	
(defrule wankomycyna-P
	?x <- (alergia-na-antybiotyki-b-laktamowe-PL tak)
=>
	(retract ?x)
	(printout t "Antybiotykoterapia: 
	
	-wankomycyna przez 4 tyg." crlf)
	(assert (koniec)))
	
(defrule czy-proteza-zastawkowa-P
	?x <- (alergia-na-antybiotyki-b-laktamowe-PL nie)
=>
	(retract ?x)
	(printout t "Czy pacjent ma proteze zastawkowa?" crlf)
	(assert (proteza-zastawkowa-P (read))))
	
(defrule leczenie-standardowe-P
	?x <- (proteza-zastawkowa-P tak)
=>
	(retract ?x)
	(printout t "Antybiotykoterapia: leczenie standardowe (4 tyg.)
	
	-penicylina G 
	lub
	-amoksycylina 
	lub
	-ceftriakson" crlf)
	(assert (koniec)))
	
(defrule leczenie-krotsze-P
	?x <- (proteza-zastawkowa-P nie)
=>
	(retract ?x)
	(printout t "Antybiotykoterapia: leczenie krotsze (2 tyg.)
	
	-penicylina G 
	lub
	-amoksycylina 
	lub
	-ceftriakson
	+
	-gentamycyna 
	lub
	-netylmycyna" crlf)
	(assert (koniec)))
	
; PRAWA CZesc
	
(defrule opornosc-na-penicyline-P
	?x <- (pelna-wrazliwosc nie)
=>
	(retract ?x)
	(printout t "Czy wystepuje alergia na antybiotyki b-laktamowe?" crlf)
	(assert (alergia-na-antybiotyki-b-laktamowe-PP (read))))	
	
(defrule trzecie-leczenie-P
	?x <- (alergia-na-antybiotyki-b-laktamowe-PP nie)
=>
	(retract ?x)
	(printout t "Antybiotykoterapia:
	
	-penicylina G przez 4 tyg. 
	lub
	-amoksycylina przez 4 tyg.
	+
	-gentamycyna przez 2 tyg." crlf)
	(assert (koniec)))
	
(defrule czwarte-leczenie-P
	?x <- (alergia-na-antybiotyki-b-laktamowe-PP tak)
=>
	(retract ?x)
	(printout t "Antybiotykoterapia: 
	
	-wankomycyna przez 4 tyg.
	+
	-gentamycyna przez 2 tyg." crlf)
	(assert (koniec)))
	
	
;=========================IZW wywolane przez gronkowce====================

(defrule IZW-wywolane-przez-gronkowce-G
	?x <- (czynnik-etiologiczny gronkowce)
=>
	(retract ?x)
	(printout t "Czy pacjent posiada proteze zastawkowa?" crlf)
	(assert (proteza-zastawkowa-G (read))))
	
; LEWA CZesc

(defrule IZW-zastawki-wlasnej-G
	?x <- (proteza-zastawkowa-G nie)
=>
	(retract ?x)
	(printout t "Czy jest to MSSA?" crlf)
	(assert (mssa-GL (read))))
	
(defrule mssa-zastawka-wlasna-G
	?x <- (mssa-GL tak)
=>
	(retract ?x)
	(printout t "Antybiotykoterapia:
	
	-(flu)kloksacylina lub oksacylina przez 4-6 tyg.
	+
	-gentamycyna przez 3-5 dni" crlf)
	(assert (koniec)))
	
(defrule mrsa-zastawka-wlasna-G
	?x <- (mssa-GL nie)
=>
	(retract ?x)
	(printout t "Antybiotykoterapia:
	
	-wankomycyna przez 4-6 tyg.
	+
	-gentamycyna przez 3-5 dni" crlf)
	(assert (koniec)))
	
; PRAWA CZesc

(defrule IZW-protezy-zastawkowej-G
	?x <- (proteza-zastawkowa-G tak)
=>
	(retract ?x)
	(printout t "Czy jest to MSSA?" crlf)
	(assert (mssa-GP (read))))
	
(defrule mssa-proteza-zastawkowa-G
	?x <- (mssa-GP tak)
=>
	(retract ?x)
	(printout t "Antybiotykoterapia:
	
	-(flu)kloksacylina lub oksacylina przez >= 6 tyg.
	+
	-ryfampicyna przez >= 6 tyg.
	+
	-gentamycyna przez 2 tyg." crlf)
	(assert (koniec)))
	
(defrule mrsa-proteza-zastawkowa-G
	?x <- (mssa-GP nie)
=>
	(retract ?x)
	(printout t "Antybiotykoterapia:
	
	-wankomycyna przez >= 6 tyg.
	+
	-ryfampicyna przez 6 tyg.
	+
	-gentamycyna przez 2 tyg." crlf)
	(assert (koniec)))

;=========================Czynnik etiologiczny nieznany===================
	
(defrule potwierdzone-IZW-N
	?x <- (czynnik-etiologiczny nieznany)
=>
	(retract ?x)
	(printout t "Czy pacjent posiada proteze zastawkowa?" crlf)
	(assert (proteza-zastawkowa-N (read))))
	
	
; LEWA CZesc	
	
(defrule IZW-zastawki-wlasnej-N
	(or
		?x <- (proteza-zastawkowa-N nie)
		?x <- (wczesne-IZW tak)
	)
=>
	(retract ?x)
	(printout t "Czy wystepuje alergia na antybiotyki b-laktamowe?" crlf)
	(assert (alergia-na-antybiotyki-b-laktamowe-NL (read))))
	
(defrule pierwsze-leczenie-N
	?x <- (alergia-na-antybiotyki-b-laktamowe-NL nie)
=>
	(retract ?x)
	(printout t "Antybiotykoterapia:
	
	-ampicylina z sulbaktamem przez 4-6 tyg. 
	lub
	-amoksycylina z klawulanianem przez 4-6 tyg.
	+
	-gentamycyna przez 4-6 tyg." crlf)
	(assert (koniec)))
	
(defrule drugie-leczenie-N
	?x <- (alergia-na-antybiotyki-b-laktamowe-NL tak)
	
=>
	(retract ?x)
	(printout t "Antybiotykoterapia:
	
	-wankomycyna przez 4-6 tyg.
	+
	-gentamycyna przez 4-6 tyg.
	+
	-cyproflaksacyna przez 4-6 tyg." crlf)
	(assert (koniec)))
	
; PRAWA CZesc

(defrule IZW-protezy-zastawkowej-N
	?x <- (proteza-zastawkowa-N tak)
=>
	(retract ?x)
	(printout t "Czy jest to wczesne IZW ( <12 mies. po operacji)?" crlf)
	(assert (wczesne-IZW (read))))
	
(defrule wczesne-IZW-N
	?x <- (wczesne-IZW tak)
=>
	(retract ?x)
	(printout t "Antybiotykoterapia:
	
	-wankomycyna przez 6 tyg.
	+
	-gentamycyna przez 2 tyg.
	+
	-ryfampicyna" crlf)
	(assert (koniec)))

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
