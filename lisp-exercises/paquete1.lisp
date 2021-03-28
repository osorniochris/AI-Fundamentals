;Taller de Programación en Common Lisp: Ejercicios paquete 1
;Osornio Sánchez Christopher

;1a.
;salida: (B C)
(nth 4 '(((1 2) 3) 4 (5 (6)) A (B C) D (E (F G))))

;1b.
;salida: 31622400
(* 366 24 60 60)

;1c.
;Caso de prueba
;entrada: x = 10, y = 20
;salida: T
(and (not (= x 0)) (<= x y))

;1d.
;salida: (-1.0 -2.5)
(list (/ (+ -7 (sqrt (- (expt 7 2) (* 4 2 5)))) (* 2 2)) (/ (- -7 (sqrt (- (expt 7 2) (* 4 2 5)))) (* 2 2)))

;2a.
;salida: 6
(+ (* 2 4) (- 6 8))

;2b.
;salida: 15/16
(/ (+ 5 (+ -3 4)) (+ 6 (/ 2 5)))

;2c.
;salida: #C(7.355944 -11.196843)
(sqrt (/ (- 1.4502 (- -4 (/ 3 8))) (expt -1 (expt (- 3 5) (/ 1 3))))) 

;2d.
;salida: #C(1.4500145 -0.065120235)
(expt (/ (expt (/ 65.402 (sqrt -1)) (/ 1 5)) 0.17) (/ 1 7))

;3a.
;salida: ; Evaluation aborted on #<SB-INT:SIMPLE-READER-ERROR "unmatched close parenthesis" {C1E94D1}>. 
(cdar '((one two) three four)))

;3b.
;salida: ((EVA LISA) KARL SVEN EVA LISA KARL SVEN) 
(append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven))

;3c.
;salida: (EVA GITAN LISA GITAN KARIN) 
(subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin))

;3d.
;salida: (EVA LISA ANNA)
(remove 'sven '(eva sven lisa sven anna))

;3e.
;salida: (KARL ADAM NILSSON)
(butlast '(karl adam nilsson gregg alisson vilma) 3)

;3f.
;salida: C
(nth 2 '(a b c d e))

;3g.
;salida: (C D E)
(nthcdr 2 '(a b c d e))

;3h.
;salida: (C B)
(intersection '(a b c) '(x b z c))

;3i.
;salida: (4)
(cdadar '(((((1 2 3) z) y) (x 4)) 7 8 (a b c (5 (6 7 8)))))

;4.
;Caso de prueba: (Recombina '((A . 1) (B . 2) (C . 3)) )
;salida: (((1 2) . A) ((2 3) . C) ((3 2 1) . B))
(defun Recombina (lista)
	   (let ((x (rest (first lista)))
		 (y (rest (second lista)))
		 (z (rest (third lista)))
		 (A (first (first lista)))
		 (B (first (second lista)))
		 (C (first (third lista))))
	     (list (cons (list x y) A) (cons (list y z) C) (cons (list z y x) B))))

;5.
;Caso de prueba: (RealNoCero? '#C(1.4500145 -0.065120235) )
;salida: NIL
(defun RealNoCero? (N)
	   (and (not (= N 0)) (realp N)))

;6. 
;Caso de prueba: (Analiza NIL)
;salida: (T NIL T NIL T)
(defun Analiza (x)
	   (list (atom x) (numberp x) (listp x) (consp x) (null x)))

;7.
;Caso de prueba: (Intercala '(1 a 2 4 6) '(b 3) )
;salida: (1 B A 3 2 4 6)
(defun Intercala (lista1 lista2)
	   (let ((lista-aux '( ) ) (acumulador 0))
	     (dotimes (i (length lista1) lista-aux)
	       (setq acumulador (+ acumulador 1))
	       (setq lista-aux (append lista-aux (list (nth i lista1))))
	       (if (< i (length lista2)) (setq lista-aux (append lista-aux (list (nth i lista2))))))
	     (if (< acumulador (length lista2)) (do ((n acumulador (+ n 1))) ((>= n (length lista2)) lista-aux)
						 (setq lista-aux (append lista-aux (list (nth n lista2))))) lista-aux)))

;8. 
;Caso de prueba: (MismoTipo? '(1234 "Una cadena" (a b c) (a.c)) '(345 "otra cadena" (x y z) (w.v)))
;salida: T
(defun MismoTipo? (lista1 lista2)
	   (let ((iguales T) (aux-equal T) (elemento1 NIL) (elemento2 NIL) (lista-aux1 '( ) ) (lista-aux2 '( ) ))
	     (dotimes (i (length lista1) iguales)
	       (setq elemento1 (nth i lista1))
	       (setq elemento2 (nth i lista2))
	       (setq lista-aux1 (list (atom elemento1) (listp elemento1)
				      (numberp elemento1) (null elemento1) (consp elemento1) (stringp elemento1)))
	       (setq lista-aux2 (list (atom elemento2) (listp elemento2)
				      (numberp elemento2) (null elemento2) (consp elemento2) (stringp elemento2)))
	       (setq aux-equal (equal lista-aux1 lista-aux2))
	       (setq iguales (and iguales aux-equal)))))

;9.
;Caso de prueba: (APalíndromo "Hola" )
;salida: "HolaaloH"
(defun APalíndromo (cadena)
	   (concatenate 'string cadena (reverse cadena)))

;10.
;Caso de prueba: (Bisiesto? 1988)
;salida: T
(defun Bisiesto? (año)
	   (and (= (mod año 4) 0) (or (not (= (mod año 100) 0)) (= (mod año 400) 0))))