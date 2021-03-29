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
;Función recursiva para encontrar el último real positivo
(defun Ultimo-real-positivo-aux (lista)
	   (if (null lista) NIL 
	       (if (numberp (first (last lista))) 
		   (if (and (realp (first (last lista))) (>= (first (last lista)) 0)) 
		       (first (last lista)) 
		       (Ultimo-real-positivo-aux (subseq lista 0 (- (length lista) 1))))
		   (Ultimo-real-positivo-aux (subseq lista 0 (- (length lista) 1))))))
;Función recursiva para contar apariciones
(defun Contar-apariciones (lista elemento contador)
	   (if (null lista) contador
	       (if (numberp (first lista)) 
		   (if (= (first lista) elemento)
		       (Contar-apariciones (rest lista) elemento (+ 1 contador)) 
		       (Contar-apariciones (rest lista) elemento contador))
		   (Contar-apariciones (rest lista) elemento contador))))
;Función principal
(defun Ultimo-real-positivo (lista)
	   (let ((numero 0) (contador 0))
	     (setq numero (Ultimo-real-positivo-aux lista))
	     (setq contador (Contar-apariciones lista numero 0))
	     (list numero contador)))


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


;8. 
;Caso de prueba: (Diagonal '((1 a b c) (d 2 e f) (g h 3 i) (j k l 4)))
;salida: (1 2 3 4)
;Función auxiliar para implementar el index de la lista
(defun Diagonal (lista)
	   (Diagonal-aux lista 0))
;Función recursiva
(defun Diagonal-aux (lista index)
	   (if (null lista) lista
	       (cons (nth index (first lista)) (Diagonal-aux (rest lista) (+ index 1)))))


;9. 
;Caso de prueba: (Analiza-lista '(a (a b c) 1 (b.c) 2.4 NIL) )
;salida: (A L A L A N)
(defun Analiza-lista (lista)
	   (if (null lista) lista 
	       (if (null (first lista)) (cons 'N (Analiza-lista (rest lista))) 
		   (if (listp (first lista)) (cons 'L (Analiza-lista (rest lista))) 
		       (cons 'A (Analiza-lista (rest lista)))))))


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


;11.
;Caso de prueba: (Filtra-vocales '(a (1 2 b) (c (E 4) 3)) )
;salida: ((1 2 B) (C (4) 3))
;Predicado auxiliar para verificar que un átomo sea una vocal
(defun Es-vocal? (elemento)
	       (or (equal elemento 'A) (equal elemento 'E) (equal elemento 'I) (equal elemento 'O) (equal elemento 'U)
		   (equal elemento 'a) (equal elemento 'e) (equal elemento 'i) (equal elemento 'o) (equal elemento 'u)))
;Función recursiva
(defun Filtra-vocales (elementos)
	   (let ((aux NIL))
	     (if (listp elementos)
		 (dolist (i elementos) 
		   (let ((resultado (Filtra-vocales i)))
		     (if (not (null resultado)) 
			 (setq aux (append aux (list resultado))) )))
		 (if (Es-vocal? elementos) (setq aux NIL) (setq aux elementos)))
	     aux))


;12.
;Caso de prueba: (Filtra-múltiplos 2 '(2 1 3 5 6 10 20 7 3 11) )
;salida: (1 3 5 7 3 11)
(defun Filtra-múltiplos (numero lista)
	   (cond ((null lista) NIL)
		 ((zerop (mod (first lista) numero)) (Filtra-múltiplos numero (rest lista)))
		 (T (cons (first lista) (Filtra-múltiplos numero (rest lista))))))


;13.
;Caso de prueba: (Celdas '((a b) ((c)) (d e)) )
;salida: 9
;Función auxiliar para implementar el contador
(defun Celdas (lista)
	   (Celdas-aux lista 0))
;Función recursiva
(defun Celdas-aux (lista contador)
	   (cond ((null lista) contador)
		 ((consp (first lista)) (if (not (null (rest lista))) 
		 	(setq contador (+ (Celdas-aux (first lista) (+ 1 contador)) (Celdas-aux (rest lista) contador))) 
		 	(Celdas-aux (first lista) (+ 1 contador))))
		 (T (Celdas-aux (rest lista) (+ 1 contador)))))


;14.
;Caso de prueba: (Implica T T NIL T T)
;salida: T
;Función auxiliar para verificar la ariedad
(defun Implica (&rest args)
	   (if (< (length args) 2) "Se requieren al menos dos argumentos"  
	       (Implica-aux T args)))
;Función recursiva
(defun Implica-aux (implicacion args)
	   (if (< (length args) 2) implicacion 
	       (progn
		 (if (and (equal (first args) T) (equal (second args) NIL)) (setq implicacion NIL) (setq implicacion T))
	       (Implica-aux implicacion (cons implicacion (rest (rest args)))))))


;16.
;Caso de prueba: (Cambia '(a b c d a e f a g h a) 'a 33)
;salida: (33 B C D 33 E F 33 G H 33)
(defun Cambia (lista elem1 elem2)
	   (cond ((null lista) lista)
		 ((equal (first lista) elem1) (cons elem2 (Cambia (rest lista) elem1 elem2)))
		 (T (cons (first lista) (Cambia (rest lista) elem1 elem2)))))


;17. 
;Implementación 1. Naive recursive computation of the nth element of the Fibonacci sequence
;Seconds of real time: 670.297
;Processor cycles: 1,474,652,993,409 
;salida: 12586269025
(defun fib1 (n)
  (check-type n (integer 0 *))
  (if (< n 2) n
      (+ (fib1 (1- n)) (fib1 (- n 2)))))

;Implementación 2. Tail-recursive computation of the nth element of the Fibonacci sequence
;Seconds of real time: 0.000 
;Processor cycles: 8,202
(defun fib2 (n)
  (check-type n (integer 0 *))
  (labels ((fib-aux (n f1 f2)
                    (if (zerop n) f1
                      (fib-aux (1- n) f2 (+ f1 f2)))))
          (fib-aux n 0 1)))

;Implementación 3. loop-based iterative computation of the nth element of the Fibonacci sequence
;Seconds of real time: 0.000
;Processor cycles: 9,356
(defun fib3 (n)
  (check-type n (integer 0 *))
  (loop for f1 = 0 then f2
        and f2 = 1 then (+ f1 f2)
        repeat n finally (return f1)))

;Implementación 4. do-based iterative computation of the nth element of the Fibonacci sequence
;Seconds of real time: 0.000
;Processor cycles: 8,014
(defun fib4 (n)
  (check-type n (integer 0 *))
  (do ((i n (1- i))
       (f1 0 f2)
       (f2 1 (+ f1 f2)))
      ((= i 0) f1)))

;Implementación 5. CPS computation of the nth element of the Fibonacci sequence
;Seconds of real time: 0.000
;Processor cycles: 12,985
(defun fib5 (n)
  (check-type n (integer 0 *))
  (labels ((fib-aux (n k)
                    (if (zerop n)
                        (funcall k 0 1)
                      (fib-aux (1- n) (lambda (x y)
                                        (funcall k y (+ x y)))))))
          (fib-aux n #'(lambda (a b) a))))

;Implementación 6. 
;Seconds of real time: 0.000
;Processor cycles: 8,912
(defun fib6 (n)
   (labels ((fibaux (n)
                 (cond ((= n 0)
                        (values 1 0))
                       (t
                        (multiple-value-bind (val prev-val)
                                             (fibaux (- n 1))
                           (values (+ val prev-val)
                                   val))))))
      (nth-value 0 (fibaux n))))

;Implementación 7. Successive squaring method from SICP
;Seconds of real time: 0.000
;Processor cycles: 11,804
(defun fib7 (n)
  (check-type n (integer 0 *))
  (labels ((fib-aux (a b p q count)
                    (cond ((= count 0) b)
                          ((evenp count)
                           (fib-aux a
                                    b
                                    (+ (* p p) (* q q))
                                    (+ (* q q) (* 2 p q))
                                    (/ count 2)))
                          (t (fib-aux (+ (* b q) (* a q) (* a p))
                                      (+ (* b p) (* a q))
                                      p
                                      q
                                      (- count 1))))))
          (fib-aux 1 0 0 1 n)))

;Implementación 8. 
;Seconds of real time: 0.000
;Processor cycles: 14,222
(defun fib8 (n)
  (if (< n 2) n
    (if (oddp n) 
      (let ((k (/ (1+ n) 2)))
        (+ (expt (fib8 k) 2) (expt (fib8 (1- k)) 2)))
      (let* ((k (/ n 2)) (fk (fib8 k)))
        (* (+ (* 2 (fib8 (1- k))) fk) fk)))))

;Implementación 9. Taken from Winston's Lisp, 3rd edition, this is a tail-recursive version, w/o an auxiliary function
;Seconds of real time: 0.000
;Processor cycles: 8,839
(defun fib9 (n &optional (i 1) (previous-month 0) (this-month 1)) 
 (if (<= n i)
      this-month
    (fib9 n (+ 1 i) this-month (+ this-month previous-month))))

;Implementación 10.
;;;Original code by Arnold Schoenhage, 
;;;translated to Scheme by Bradley J. Lucier (2004), 
;;;and adapted to Common Lisp by Nicolas Neuss.
;Seconds of real time: 0.000
;Processor cycles: 11,294
(defun fast-fib-pair (n)
  "Returns f_n f_{n+1}."
  (case n
    ((0) (values 0 1))
    ((1) (values 1 1))
    (t (let ((m (floor n 2)))
         (multiple-value-bind (f_m f_m+1)
             (fast-fib-pair m)
           (let ((f_m^2   (* f_m f_m))
                 (f_m+1^2 (* f_m+1 f_m+1)))
             (if (evenp n)
                 (values (- (* 2 f_m+1^2)
                            (* 3 f_m^2)
                            (if (oddp m) -2 2))
                         (+ f_m^2 f_m+1^2))
                 (values (+ f_m^2 f_m+1^2)
                         (- (* 3 f_m+1^2)
                            (* 2 f_m^2)
                            (if (oddp m) -2 2))))))))))

;Implementación 11. Fibonacci - Binet's Formula
;Seconds of real time: 0.060
;Processor cycles: 131,308,265
(defun fib11 (n)
  (* (/ 1 (sqrt 5))
     (- (expt (/ (+ 1 (sqrt 5)) 2) n)
	(expt (/ (- 1 (sqrt 5)) 2) n))))

;Implementación 12. 
;Seconds of real time: 0.000
;Processor cycles: 15,324
(defun fib12 (n)
  (prog1 (round (/ (expt (/ (+ 1 (sqrt 5)) 2) n) (sqrt 5)))))


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


;21.
;Caso de prueba: (PegaYCambia '(a b c a b c) '(1 2 3 4 1 2 3 4) 'a 0)
;salida: (0 1 B 2 C 3 0 4 B 1 C 2 3 4)
;Función para concatenar listas
(defun concatenar (lista1 lista2)
	   (if (and (null lista1) (null lista2)) NIL
	       (if (null lista1) (cons (first lista2) (concatenar (rest lista1) (rest lista2))) 
		   (if (null lista2) (cons (first lista1) (concatenar (rest lista1) (rest lista2))) 
		       (cons (first lista1) (cons (first lista2) (concatenar (rest lista1) (rest lista2))))))))
;Función para realizar los reemplazos
(defun reemplazo (lista el1 el2)
	   (if (null lista) lista
	       (if (equal (first lista) el1) (cons el2 (reemplazo (rest lista) el1 el2)) 
		   (cons (first lista) (reemplazo (rest lista) el1 el2)))))
;Función principal PegaYCambia
(defun PegaYCambia (lista1 lista2 el1 el2)
	   (let ((lista-con (concatenar lista1 lista2)))
	     (reemplazo lista-con el1 el2)))


;22.
;Caso de prueba: (QSort '(100 4 5 1 2 T 7 "CADENA" 10 0 a))
;salida: (0 1 2 4 5 7 10 100)
;Se implementó quicksort tomando como pivote el primer elemento
;Función para partir lista
(defun mitad (lista pivote a)
    (if (endp lista)
        a
	(when (numberp (first lista)) 
	  (if (< (first lista) pivote) 
            (mitad (rest lista) pivote (list (cons (first lista) (first a) ) (second a) )) 
            (mitad (rest lista) pivote (list (first a) (cons (first lista) (second a) )) )))))
;Función para remover el elemento pivote de la lista
(defun quitar-pivote (lista pivote)
	   (when (numberp (first lista)) 
	     (if (= (first lista) pivote) 
		    (rest lista)
		    (cons (first lista) (quitar-pivote (rest lista) pivote)))))
;Función para mezclar listas
(defun mezclar (lista1 lista2)
    (if (endp lista1)
        lista2
        (if (endp lista2)
            lista1
	    (when (and (numberp (first lista1)) (numberp (first lista2)))
	      (if (< (first lista1) (first lista2))
                (cons (first lista1) (mezclar (rest lista1) lista2))
                (cons (first lista2) (mezclar lista1 (rest lista2))))))))
;Función para eliminar elementos no numéricos de las lista
(defun filtra-numeros (lista)
	   (cond ((null lista) NIL)
		 ((not (numberp (first lista))) (filtra-numeros (rest lista)))
		 (T (cons (first lista) (filtra-numeros (rest lista))))))
;Función principal QSort
(defun QSort (lista-arg)
	   (let ((lista (filtra-numeros lista-arg)))
	     (if (endp (rest lista)) lista
		 (let ((pivote (first lista)))
		   (let ((mitades (mitad (quitar-pivote lista pivote) pivote nil)))
		     (mezclar (QSort (first mitades)) (cons pivote (QSort(second mitades)))))))))

