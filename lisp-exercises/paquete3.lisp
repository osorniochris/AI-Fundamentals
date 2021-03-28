;Taller de Programación en Common Lisp: Ejercicios paquete 3 (Recursividad)
;Osornio Sánchez Christopher

;1.
;Caso de prueba: (ElemInPos 0 '(1 2 4 0 5) 3)
;salida: T
(defun ElemInPos (elem lista pos)
	   (cond ((< pos 0) NIL)
		 ((equal elem (nth pos lista)) T)
		 (T (ElemInPos elem (rest lista) (- pos 1)))))

;2.
;Caso de prueba: (Inicio-en '(1 2 3 6 6 6 5 2 1) 6)
;salida: (6 6 6 5 2 1)
(defun Inicio-en (lista elemento)
	   (cond ((equal elemento (first lista)) lista)
		 (T (Inicio-en (rest lista) elemento))))

;3.
;Caso de prueba: (Termina-en '(a b c d e f f f h) 'f)
;salida: (A B C D E F F F)
(defun Termina-en (lista elemento)
	   (cond ((equal elemento (first (last lista))) lista)
		 (T (Termina-en (subseq lista 0 (- (length lista) 1)) elemento))))

;4.
;Caso de prueba: (Primer-impar '(a v x T 2 3 4 1))
;salida: (3 5)
;Función auxiliar para implementar el contador
(defun Primer-impar (lista)
	   (Primer-impar-aux lista 0))
;Función recursiva
(defun Primer-impar-aux (lista index)
	   (if (integerp (first lista)) 
	       (if (oddp (first lista)) (list (first lista) index) (Primer-impar-aux (rest lista) (+ index 1))) 
	       (Primer-impar-aux (rest lista) (+ index 1))))

;5.
;Caso de prueba: (Ultimo-real-positivo '(1 3 A (x y z) NIL 2.3 3 3 3 X))
;salida: (3 4)

;6.
;Caso de prueba: (Conteo '(a b c 1 2.3 5 3/2 2 (a b c) 89 ((a b) c))
;salida: (6 . 2)
;Función auxiliar para implementar contadores
(defun Conteo (lista)
	   (Conteo-aux lista 0 0))
;Función recursiva
(defun Conteo-aux (lista numeros listas)
	   (cond ((null lista) (cons numeros listas))
		 ((numberp (first lista)) (Conteo-aux (rest lista) (+ numeros 1) listas))
		 ((listp (first lista)) (Conteo-aux (rest lista) numeros (+ listas 1)))
		 (T (Conteo-aux (rest lista) numeros listas))))

;7.
;Caso de prueba: (Aplana '(((a b) (c x)) (d e) f) )
;salida: (A B C X D E F)
(defun Aplana (lista)
	   (cond ((null lista) lista)
		 ((listp (first lista)) (append (Aplana (first lista)) (Aplana (rest lista))))
		 (T (cons (first lista) (Aplana (rest lista))))))

;10. 
;Caso de prueba: (Suma-numérica '(a "cadena" NIL 23 4 2.5 #\B 4/2))
;salida: 31.5
;Función auxiliar para implementar la suma
(defun Suma-numérica (lista)
	   (Suma-numérica-aux lista 0))
;Función recursiva
(defun Suma-numérica-aux (lista suma)
	   (cond ((null lista) suma)
		 ((numberp (first lista)) (Suma-numérica-aux (rest lista) (+ suma (first lista))))
		 (T (Suma-numérica-aux (rest lista) suma))))

;12.
;Caso de prueba: (Filtra-múltiplos 2 '(2 1 3 5 6 10 20 7 3 11) )
;salida: (1 3 5 7 3 11)
(defun Filtra-múltiplos (numero lista)
	   (cond ((null lista) NIL)
		 ((zerop (mod (first lista) numero)) (Filtra-múltiplos numero (rest lista)))
		 (T (cons (first lista) (Filtra-múltiplos numero (rest lista))))))

;16.
;Caso de prueba: (Cambia '(a b c d a e f a g h a) 'a 33)
;salida: (33 B C D 33 E F 33 G H 33)
(defun Cambia (lista elem1 elem2)
	   (cond ((null lista) lista)
		 ((equal (first lista) elem1) (cons elem2 (Cambia (rest lista) elem1 elem2)))
		 (T (cons (first lista) (Cambia (rest lista) elem1 elem2)))))

;19.
;Caso de prueba: (Aplana '(((a b) (c x)) (d e) f) )
;salida: (A B C X D E F)
(defun Aplana (lista)
	   (cond ((null lista) lista)
		 ((listp (first lista)) (append (Aplana (first lista)) (Aplana (rest lista))))
		 (T (cons (first lista) (Aplana (rest lista))))))

;20.
;Caso de prueba: (Elimina '(a b d 5 "abc" T 22.2 30 1) 20)
;salida: (22.2 30)
(defun Elimina (lista n)
	   (if (null lista) lista
	       (if (numberp (first lista))
		   (if (<= (first lista) n) (Elimina (rest lista) n) (cons (first lista) (Elimina (rest lista) n)))
		   (Elimina (rest lista) n))))