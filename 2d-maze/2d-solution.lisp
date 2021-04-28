;;;======================================================================================
;;;  Primer examen parcial. Laberintos 2D
;;;
;;;  Representación de los estados: 
;;;         Arreglo de dos posiciones que indica la fila y la columna respectivamente.
;;;         #(0 0) = Fila 0, Columna 0
;;; 
;;;  Christopher Osornio Sánchez
;;;  Abril, 2021
;;;======================================================================================

(load "maze_lib.lisp")

(add-algorithm 'depth-first-search)
(add-algorithm 'best-first-search)
(add-algorithm 'A*)

(defparameter  *open* '())                                               
(defparameter  *memory* '()) 
(defparameter  *ops*  '( (:N      0)
                         (:NE     1)
                         (:E      2)
                         (:SE     3)     
                         (:S      4) 
                         (:SW     5) 
                         (:W      6)
                         (:NW     7) ) )
(defparameter  *id*  -1)  
(defparameter  *current-ancestor*  nil)  
(defparameter  *solucion*  nil)  
(defparameter  *costos* (make-hash-table :test 'equal)) ;tabla hash que permite guardar el 
                                                        ;costo de un nodo para no calcularlo dos veces

(defun  crear-nodo (estado op flag tipo-distancia)
"Crea un nodo de la forma (id estado id-ancestro op [aptitud | evaluación])
 flag puede tener los valores :con-aptitud, :con-evaluacion o NIL
 distancia puede ser :euclidiana o :manhattan"
      (incf  *id*)
      (if (equal flag :con-aptitud) 
        (list  *id*  estado  *current-ancestor*  (second op) (calcular-aptitud estado tipo-distancia))
        (if (equal flag :con-evaluacion)
            (list  *id*  estado  *current-ancestor*  (second op) (obtener-evaluacion tipo-distancia estado))
            (list  *id*  estado  *current-ancestor*  (second op)) ) ) )  

(defun calcular-aptitud (estado flag)
"Calcula la aptitud de un nodo utilizando como función heurística la distancia 
 Euclidiana o Manhattan. Depende del valor de flag"
    (let ((row (aref estado 0))
           (col (aref estado 1))
           (row-meta (aref *goal* 0))
           (col-meta (aref *goal* 1)) )
        (if (equal flag :manhattan)
            (+ (abs (- row-meta row)) (abs (- col-meta col)) )
            (if (equal flag :euclidiana)
                (sqrt (+ (expt (- row-meta row) 2) (expt (- col-meta col) 2) ))
                "ERROR") ) ) )

(defun obtener-evaluacion (tipo-distancia estado)
"Suma del costo y de la aptitud de un nodo"
    (+ (gethash *current-ancestor* *costos*) (calcular-aptitud estado tipo-distancia) )
)

(defun calcular-costo ()
"Calcula el nivel de un nodo en el árbol verificando si el costo ya se encuentra
 en la tabla hash *costos*"
    (labels ((locate-node  (id  lista)       
		(cond ((null  lista)  Nil)
		      ((eql  id  (first (first  lista))) (first  lista))
		      (T  (locate-node  id (rest  lista))))))
	    
        (let ((current  (locate-node  *current-ancestor*  *memory*))
              (route NIL)
              (nivel 0)
              (aux NIL)
              (incremento 1) )
            (loop  while  (not (null  current))  do   
                (setq aux (gethash (third current) *costos*))
                (cond ((numberp aux) (setq nivel (+ incremento aux)) (return nivel)) 
                      (T 
                        (push  current  route)
                        (setq incremento (+ incremento 1))
                        (setq nivel  (length route) ) ) )                      
                (setq  current  (locate-node  (third  current) *memory*)) )
            nivel) ) )

(defun insertar-en-open (estado  op tipo-distancia) 
"Crea un nodo y lo inserta en la frontera de búsqueda"
     (let ((nodo  (crear-nodo  estado  op NIL tipo-distancia)))
       (push  nodo  *open*) ) )

(defun insertar-en-open-BESTFS (estado  op tipo-distancia) 
"Crea un nodo y lo inserta en la frontera de búsqueda ordenando de menor a mayor aptitud"
    (labels ( (list-insert-at (lst index new-value)
              (let ((retval nil))
              (loop for i from 0 to (- (length lst) 1) do
                (when (= i index)
                    (push new-value retval))
                    (push (nth i lst) retval))
                (when (>= index (length lst))
                (push new-value retval))
                (nreverse retval))) ) 
    
        (let ((nodo  (crear-nodo  estado  op :con-aptitud tipo-distancia)) 
            (index (length *open*))
            (lon (length *open*)) 
            (contador 0) )

            (when (not (null *open*)) 
                (dolist (i (reverse *open*))
                (if (<= (fifth nodo) (fifth i)) (setq index (- lon contador 1)))
                (setq contador (+ contador 1))) )

            (setq *open* (list-insert-at *open* index nodo)) ) ) )

(defun insertar-en-open-A* (estado op tipo-distancia) 
"Crea un nodo y lo inserta en la frontera de búsqueda ordenando de menor a mayor evaluación"
    (labels ( (list-insert-at (lst index new-value)
              (let ((retval nil))
              (loop for i from 0 to (- (length lst) 1) do
                (when (= i index)
                    (push new-value retval))
                    (push (nth i lst) retval))
                (when (>= index (length lst))
                (push new-value retval))
                (nreverse retval))) ) 
    
        (let ((nodo  (crear-nodo  estado  op :con-evaluacion tipo-distancia)) 
            (index (length *open*))
            (mejor-evaluacion NIL)
            (existe NIL)
            (lon (length *open*)) 
            (contador 0) )

            (when (not (null *open*)) 
                (dolist (i (reverse *open*))
                    (when (<= (fifth nodo) (fifth i)) (setq index (- lon contador 1)))
                    (when (equalp (second nodo) (second i)) (setq existe T) (return))
                    (when (and (equalp (second nodo) (second i)) (< (fifth nodo) (fifth i)))  
                        (setq mejor-evaluacion (list i (- lon contador 1)))
                        (return))
                    (setq contador (+ contador 1))) )
            
            (if (null mejor-evaluacion)
                (when (not existe) (setq *open* (list-insert-at *open* index nodo))  )
                (progn
                    (setq *open* (list-insert-at *open* (second mejor-evaluacion) nodo))
                    (setq *open* (remove (first mejor-evaluacion) *open* :test #'equalp)) ) ) ) ) )

(defun sacar-de-open ()
"Permite obtener el primer elemento de la frontera de búsqueda"
      (pop  *open*) )

(defun N-valido? (row col)
"Predicado. Verifica si el movimiento hacia arriba es válido"
    (let ((arriba NIL))
        (if (>= (- row 1) 0) 
            (progn
            (setq arriba (get-cell-walls (- row 1) col))
            (if (not
                (or (and (>= arriba 4) (<= arriba 7))
                (and (>= arriba 12) (<= arriba 15))))
                T
                NIL))
            NIL) ) )

(defun NE-valido? (row col)
"Predicado. Verifica si el movimiento arriba-derecha es válido"
    (let ((arriba-der NIL) 
          (arriba NIL)
          (derecha NIL)  
          (total-rows (get-maze-rows)) 
          (total-cols (get-maze-cols)) )
        (if (and (>= (- row 1) 0) (<= (+ col 1) (- total-cols 1)) )
            (progn
                (setq arriba-der (get-cell-walls (- row 1) (+ col 1)))
                (setq arriba (get-cell-walls (- row 1) col)) 
                (setq derecha (get-cell-walls row (+ col 1))) 
                (if (= (logand arriba-der 12) 12)
                    NIL
                    (if (and (= (logand arriba-der 4) 4) (= (logand arriba 4) 4))
                        NIL
                        (if (and (= (logand arriba-der 8) 8) (= (logand derecha 8) 8))
                            NIL
                            (if (and (= (logand arriba 4) 4) (= (logand derecha 8) 8))
                                NIL
                                T) ) ) ) )
            NIL) ) )

(defun E-valido? (row col)
"Predicado. Verifica si el movimiento a la derecha es válido"
    (let ((derecha NIL) (total-cols (get-maze-cols)) )
        (if (<= (+ col 1) (- total-cols 1)) 
            (progn
            (setq derecha (get-cell-walls row (+ col 1)))
            (if (not (>= derecha 8)) T NIL) )
            NIL ) ) )

(defun SE-valido? (row col)
"Predicado. Verifica si el movimiento abajo-derecha es válido"
    (let ((abajo-der NIL) 
          (abajo NIL)
          (derecha NIL)  
          (total-rows (get-maze-rows)) 
          (total-cols (get-maze-cols)) )
        (if (and (<= (+ row 1) (- total-rows 1)) (<= (+ col 1) (- total-cols 1)) )
            (progn
                (setq abajo-der (get-cell-walls (+ row 1) (+ col 1)))
                (setq abajo (get-cell-walls (+ row 1) col)) 
                (setq derecha (get-cell-walls row (+ col 1))) 
                (if (= (logand abajo-der 9) 9)
                    NIL
                    (if (and (= (logand abajo-der 1) 1) (= (logand abajo 1) 1))
                        NIL
                        (if (and (= (logand abajo-der 8) 8) (= (logand derecha 8) 8))
                            NIL
                            (if (and (= (logand abajo 1) 1) (= (logand derecha 8) 8))
                                NIL
                                T) ) ) ) )
            NIL) ) )

(defun S-valido? (row col)
"Predicado. Verifica si el movimiento hacia abajo es válido"
    (let ((abajo NIL) (total-rows (get-maze-rows)) )
        (if (<= (+ row 1) (- total-rows 1))
            (progn
            (setq abajo (get-cell-walls (+ row 1) col))
            (if (= (mod abajo 2) 1)
                NIL
                T))
            NIL) ) )

(defun SW-valido? (row col)
"Predicado. Verifica si el movimiento abajo-izquierda es válido"
    (let ((abajo-izq NIL) 
          (abajo NIL)
          (izquierda NIL)  
          (total-rows (get-maze-rows)) 
          (total-cols (get-maze-cols)) )
        (if (and (<= (+ row 1) (- total-rows 1)) (>= (- col 1) 0) )
            (progn
                (setq abajo-izq (get-cell-walls (+ row 1) (- col 1)))
                (setq abajo (get-cell-walls (+ row 1) col)) 
                (setq izquierda (get-cell-walls row (- col 1))) 
                (if (= (logand abajo-izq 3) 3)
                    NIL
                    (if (and (= (logand abajo-izq 1) 1) (= (logand abajo 1) 1))
                        NIL
                        (if (and (= (logand abajo-izq 2) 2) (= (logand izquierda 2) 2))
                            NIL
                            (if (and (= (logand abajo 1) 1) (= (logand izquierda 2) 2))
                                NIL
                                T) ) ) ) )
            NIL) ) )

(defun W-valido? (row col)
"Predicado. Verifica si el movimiento a la izquierda es válido"
    (let ((izquierda NIL) )
        (if (>= (- col 1) 0) 
            (progn
            (setq izquierda (get-cell-walls row (- col 1)))
            (if (not (= (logand izquierda 2) 2)) T NIL) )
            NIL ) ) )

(defun NW-valido? (row col)
"Predicado. Verifica si el movimiento arriba-izquierda es válido"
    (let ((arriba-izq NIL) 
          (arriba NIL)
          (izquierda NIL)  
          (total-rows (get-maze-rows)) 
          (total-cols (get-maze-cols)) )
        (if (and (>= (- row 1) 0) (>= (- col 1) 0) )
            (progn
                (setq arriba-izq (get-cell-walls (- row 1) (- col 1)))
                (setq arriba (get-cell-walls (- row 1) col)) 
                (setq izquierda (get-cell-walls row (- col 1))) 
                (if (= (logand arriba-izq 6) 6)
                    NIL
                    (if (and (= (logand arriba-izq 4) 4) (= (logand arriba 4) 4))
                        NIL
                        (if (and (= (logand arriba-izq 2) 2) (= (logand izquierda 2) 2))
                            NIL
                            (if (and (= (logand arriba 4) 4) (= (logand izquierda 2) 2))
                                NIL
                                T) ) ) ) )
            NIL) ) )

(defun operador-valido? (op estado)
"Predicado. Verifica cual operador es aplicable sobre un estado"
    (let* ((operador (first op)) 
          (row (aref estado 0))
          (col (aref estado 1)) )
        (case operador 
	        (:N   (N-valido? row col)  )
            (:NE  (NE-valido? row col) )
            (:E   (E-valido? row col)  )
            (:SE  (SE-valido? row col) )   
            (:S   (S-valido? row col)  )    
            (:SW  (SW-valido? row col) )
            (:W   (W-valido? row col)  )
            (:NW  (NW-valido? row col)  )
	        (T NIL) ) ) )
    
(defun aplicar-operador (op estado)
"Aplica un operador sin restricciones"
    (let* ((operador (first op)) 
          (row (aref estado 0))
          (col (aref estado 1)) )
        (case operador 
	        (:N   (make-array '(2) :initial-contents (list (- row 1) col) )       )
            (:NE  (make-array '(2) :initial-contents (list (- row 1) (+ col 1)) ) )
            (:E   (make-array '(2) :initial-contents (list row (+ col 1)) )       )
            (:SE  (make-array '(2) :initial-contents (list (+ row 1) (+ col 1)) ) )
            (:S   (make-array '(2) :initial-contents (list (+ row 1) col) )       )  
            (:SW  (make-array '(2) :initial-contents (list (+ row 1) (- col 1)) ) )
            (:W   (make-array '(2) :initial-contents (list row (- col 1)) )       )
            (:NW  (make-array '(2) :initial-contents (list (- row 1) (- col 1)) ) )
	        (T "Error") ) ) )

(defun expandir (estado)
"Obtiene todos los estados resultado de aplicar los operadores sobre un estado"
    (let ((descendientes  nil)
	    (nuevo-estado  nil))
        (dolist  (op  *Ops*  descendientes)  
            (when (operador-valido? op estado)
                (setq  nuevo-estado  (aplicar-operador op estado))         
	            (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))))

(defun  estado-conocido?  (estado  lista-origen)
"Predicado. Verifica si un estado de encuentra en la lista-origen"
     (cond ((null  lista-origen)  NIL)
	        ((equalp  estado  (second (first  lista-origen)))  T) 
		(T  (estado-conocido?  estado  (rest  lista-origen))))  )

(defun  filtrar-conocidos (lista-estados-y-ops origen) 
"Elimina los estados que ya se encuentran 'origen' para evitar repeticiones"
     (cond ((null  lista-estados-y-ops)  NIL)
	       ((estado-conocido? (first (first  lista-estados-y-ops)) origen)
		       (filtrar-conocidos  (rest  lista-estados-y-ops) origen))
		(T  (cons  (first lista-estados-y-ops) (filtrar-conocidos  (rest  lista-estados-y-ops) origen)))) )

(defun obtener-solucion (nodo)
"Recopila todos los nodos que forman parte de la solución"
     (labels ((locate-node  (id  lista)       
		  (cond ((null  lista)  Nil)
		        ((eql  id  (first (first  lista))) (first  lista))
		        (T  (locate-node  id (rest  lista))))))
	  (let ((current  (locate-node  (first  nodo)  *memory*)))
	     (loop  while  (not (null  current))  do                        
		 (push  current  *solucion*)     
		 (setq  current  (locate-node  (third  current) *memory*))))  
	     *solucion*))

(defun reiniciar-variables-globales () 
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil)
     (setq *costos* (make-hash-table :test 'equal)) )

(defun  blind-search (edo-inicial  edo-meta)
"Función para invocar la búsqueda Depth-First"
    (reiniciar-variables-globales)
    (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil))

        (insertar-en-open  edo-inicial  NIL NIL)
        (loop until  (or  meta-encontrada (null *open*))  do
	        (setq nodo    (sacar-de-open)              
		          estado  (second  nodo)               
		          operador  (third  nodo))          
	        (push  nodo  *memory*)
	        (cond    ((equalp  edo-meta  estado)
                      (obtener-solucion  nodo) 
                      (setq  meta-encontrada  T))
		              (t (setq  *current-ancestor*  (first  nodo)) 
			             (setq  sucesores  (expandir estado))
			             (setq  sucesores  (filtrar-conocidos  sucesores *memory*))     
			             (loop for  elemento  in  sucesores  do
				            (insertar-en-open  (first elemento)  (second elemento) :euclidiana))))))  )

(defun informed-search-BEST (edo-inicial  edo-meta tipo-distancia)
"Función para invocar la búsqueda Best-First"
    (reiniciar-variables-globales)
    (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil))

        (insertar-en-open-BESTFS   edo-inicial  NIL tipo-distancia)
        (loop until  (or  meta-encontrada (null *open*))  do
	        (setq nodo    (sacar-de-open)              
		          estado  (second  nodo)               
		          operador  (third  nodo))          
	        (push  nodo  *memory*)
	        (cond    ((equalp  edo-meta  estado)
                        (obtener-solucion  nodo) 
                        (setq  meta-encontrada  T))
		              (t (setq  *current-ancestor*  (first  nodo)) 
			             (setq  sucesores  (expandir estado))
			             (setq  sucesores  (filtrar-conocidos  sucesores *memory*))
                         (setq  sucesores  (filtrar-conocidos  sucesores *open*))         
			             (loop for  elemento  in  sucesores  do
				            (insertar-en-open-BESTFS  (first elemento)  (second elemento) tipo-distancia))))))  )

(defun planeacion-A* (edo-inicial  edo-meta tipo-distancia)
"Función para invocar la planeación A*"
    (reiniciar-variables-globales)
    (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil))

        (insertar-en-open   edo-inicial  NIL tipo-distancia)
        (loop until  (or  meta-encontrada (null *open*))  do
	        (setq nodo    (sacar-de-open)              
		          estado  (second  nodo)               
		          operador  (third  nodo))          
	        (push  nodo  *memory*)
	        (cond    ((equalp  edo-meta  estado)
                        (obtener-solucion  nodo) 
                        (setq  meta-encontrada  T))
		              (t (setq  *current-ancestor*  (first  nodo))   
			             (setq  sucesores  (expandir estado))
			             (setq  sucesores  (filtrar-conocidos  sucesores *memory*))
                         (when (>(length sucesores) 0) 
                            (setf (gethash *current-ancestor* *costos*) (calcular-costo))     
			             (loop for  elemento  in  sucesores  do
				            (insertar-en-open-A* (first elemento)  (second elemento) tipo-distancia))))))  ) )

(defun depth-first-search ()
"Depth-First-Search"
    (blind-search *start* *goal*)
    (loop for i in *solucion* 
        if (numberp (fourth i)) do (setq *solution* (append *solution* (list (fourth i)))) ) )

(defun best-first-search ()
"Best-FS con distancia euclidiana como función heurística"
    (informed-search-BEST *start* *goal* :euclidiana)
    (loop for i in *solucion* 
        if (numberp (fourth i)) do (setq *solution* (append *solution* (list (fourth i)))) ) )

(defun A*()
"Planeación A* con distancia euclidiana como función heurística"
    (planeacion-A* *start* *goal* :euclidiana)
    (loop for i in *solucion* 
        if (numberp (fourth i)) do (setq *solution* (append *solution* (list (fourth i)))) ) )

(start-maze)
