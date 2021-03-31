;Taller de Programación en Common Lisp: Ejercicios paquete 2 (Estructuras algorítmicas)
;Osornio Sánchez Christopher

;1.
;Caso de prueba: (ElemInPos 0 '(1 2 4 0 5) 3)
;salida: T
(defun ElemInPos (elem lista pos)
	   (equal (nth pos lista) elem))

;2.
;Caso de prueba: (Inicio-en '(1 2 3 6 6 6 5 2 1) 6)
;salida: (6 6 6 5 2 1)
(defun Inicio-en (lista elemento)
	   (let ((index 0) (lon (length lista)) (contador 0))
	     (dolist (i (reverse lista))
	       (if (equal i elemento) (setq index (- lon contador 1)))
	       (setq contador (+ contador 1)))
	     (subseq lista index)))

;3.
;Caso de prueba: (Termina-en '(a b c d e f f f h) 'f)
;salida: (A B C D E F F F)
(defun Termina-en (lista elemento)
	   (let ((index 0) (contador 0))
	     (dolist (i lista)
	       (if (equal i elemento) (setq index contador))
	       (setq contador (+ contador 1)))
	     (subseq lista 0 (+ index 1))))

;4.
;Caso de prueba: (Primer-impar '(a v x T 2 3 4 1))
;salida: (3 5)
(defun Primer-impar (lista)
	   (let ((impar NIL) (index NIL) (contador 0) (lon (length lista)))
	     (dolist (i (reverse lista) index)
	       (when (integerp i)
		 (when (oddp i)
		   (setq impar i)
		   (setq index (- lon contador 1))))
	       (setq contador (+ contador 1)))
	     (list impar index)))

;5.
;Caso de prueba: (Ultimo-real-positivo '(1 3 A (x y z) NIL 2.3 3 3 3 X))
;salida: (3 4)
(defun Ultimo-real-positivo (lista)
	   (let ((numero 0) (contadores NIL) (aux 0))
	     (setq contadores (make-hash-table :test 'equal))
	     (dolist (i lista)
	       (when (numberp i)
		 (when (and (realp i) (>= i 0))
		   (setq numero i)))
	       (setq aux (+ (gethash i contadores 0) 1))
	       (setf (gethash i contadores) aux))
	     (list numero (gethash numero contadores))))

;6.
;Caso de prueba: (Conteo '(a b c 1 2.3 5 3/2 2 (a b c) 89 ((a b) c))
;salida: (6 . 2)
(defun Conteo (lista)
	   (let ((numeros 0) (listas 0))
	     (dolist (i lista)
	       (if (numberp i) (setq numeros (+ numeros 1)))
	       (if (listp i) (setq listas (+ listas 1))))
	     (cons numeros listas)))

;7.
;Caso de prueba: (Aplana '(((a b) (c x)) (d e) f) )
;salida: (A B C X D E F)
(defun Aplana (lista)
	   (let ((pila '( ) ) (elemento '( ) ) (resultado '( ) ) (aux '( ) ))
	     (push lista pila)
	     (loop do 
		  (setq elemento (first pila))
		  (setq pila (remove elemento pila :test #'equal))
		  (loop for i in elemento
		       if (listp i)
		         do (push i pila)
		       else
		         do (setq aux (append aux (list i))))
		  (setq resultado (append aux resultado))
		  (setq aux '( ) )
		  until (null pila))
	     resultado))

;8. 
;Caso de prueba: (Diagonal '((1 a b c) (d 2 e f) (g h 3 i) (j k l 4)))
;salida: (1 2 3 4)
(defun Diagonal (matriz)
	   (let ((col 0) (lista-aux '( ) ))
	     (dolist (i matriz lista-aux)
	       (setq lista-aux (append lista-aux (list (nth col i))))
	       (setq col (+ col 1)))))

;9. 
;Caso de prueba: (Analiza-lista '(a (a b c) 1 (b.c) 2.4 NIL) )
;salida: (A L A L A N)
(defun Analiza-lista (lista)
	   (let ((resultado '( ) ))
	     (dolist (i lista resultado)
	       (if (null i) (setq resultado (append resultado (list 'N)))
		   (progn (if (listp i) (setq resultado (append resultado (list 'L)))
			      (setq resultado (append resultado (list 'A)))))))))

;10. 
;Caso de prueba: (Suma-numérica '(a "cadena" NIL 23 4 2.5 #\B 4/2))
;salida: 31.5
(defun Suma-numérica (lista)
	   (loop for i in lista when (numberp i) sum i))

;11.

;12.
;Caso de prueba: (Filtra-múltiplos 2 '(2 1 3 5 6 10 20 7 3 11) )
;salida: (1 3 5 7 3 11)
(defun Filtra-múltiplos (numero lista)
	   (loop for i in lista
		unless (zerop (mod i numero))
		collect i))

;13.
;Caso de prueba: (Celdas '((a b) ((c)) (d e)) )
;salida: 9
(defun Celdas (lista)
	   (let ((pila '( ) ) (elemento '( ) ) (num-celdas 0))
	     (push lista pila)
	     (loop do 
		  (setq elemento (first pila))
		  (setq pila (remove elemento pila :test #'equal))
		  (loop for i in elemento
		       if (listp i)
		         do (push i pila)
			 (when (consp i) (setq num-celdas (+ 1 num-celdas)))
		       else
		         do (setq num-celdas (+ 1 num-celdas)))
		  until (null pila))
	     num-celdas))

;14.
;Caso de prueba: (Implica T T NIL T T)
;salida: T
(defun Implica (&rest args)
	   (let ((resultado T))
	     (if (= (length args) 0) T 
	     	(if (= (length args) 1) 
	     		(if (equal (first args) T) T NIL)		
		 (progn 
	            (when (and (equal (first args) T) (equal (second args) NIL)) (setq resultado NIL))
	            (loop for i from 2 to (- (length args) 1) do
		       (if (and (equal resultado T) (equal (nth i args) NIL)) (setq resultado NIL) (setq resultado T)))
		    resultado)))))

;15.
;Caso de prueba: (Mult '((12 7 3) (4 5 6) (7 8 9)) '((5 8 1 2) (6 7 3 0) (4 5 9 1)) )
;salida: ((114 160 60 27) (74 97 73 14) (119 157 112 23))
(defun Mult (matriz1 matriz2)
	   (let ((resultado '( ) ) (num-aux 0) (aux '( ) ) (tamaño1 (length matriz1))
		 (tamaño2 (length (first matriz2))) (tamaño3 (length matriz2)))
	     (if (not (= (length (first matriz1)) (length matriz2))) NIL 
		 (progn
		   (dotimes (i tamaño1 resultado)
		     (dotimes (j tamaño2)
		       (dotimes (k tamaño3)
			 (setq num-aux (+ num-aux (* (nth k (nth i matriz1)) (nth j (nth k matriz2))))))
		       (setq aux (append aux (list num-aux)))
		       (setq num-aux 0))
		     (setq resultado (append resultado (list aux)))
		     (setq aux '( ) ))))))