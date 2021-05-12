;;;======================================================================================
;;;  Agente jugador de gato 4x4
;;;
;;;     Se implementa el algoritmo MiniMax con podas Alfa-Beta.
;;; 
;;;  Christopher Osornio Sánchez
;;;  Mayo, 2021
;;;======================================================================================

(defparameter  *ops*  '(6 7 10 11 1 4 13 16 2 3 5 9 8 12 14 15) ) 
(defparameter  *profundidad-max*  3) ;constante que permite ajustar dinámicamente la profundidad máxima
(defparameter  *simbolo* 'O)
(defparameter  *simbolo-rival* 'X)

;Funciones auxiliares
(defun obtener-tablero-hash (tablero)
"Función auxiliar para mapear el tablero a una tabla hash"
    (let ((counter 1) (tablero-hash (make-hash-table :test 'equal)))
        (loop for i in tablero do
            (loop for j in i do 
                (setf (gethash counter tablero-hash) j)
                (incf  counter) ) )
        tablero-hash ) )

(defun transpuesta (tablero)
"Función auxiliar para obtener la matriz transpuesta de la representación del tablero"
    (apply #'mapcar #'list tablero) )

;Funciones para la aplicación de operadores
(defun operador-valido? (tablero op)
"Predicado. Se verifica si la posición seleccionada está disponible"
    (let ((tablero-hash (obtener-tablero-hash tablero)))
        (not (or (equal (gethash op tablero-hash) *simbolo*) (equal (gethash op tablero-hash) *simbolo-rival*))) ) )

(defun aplicar-operador (estado op simbolo) 
"Se aplica el operador indicado con el símbolo elegido"
    (let ((new-tablero NIL) (new-row NIL) (counter 1)) 
        (loop for row in estado do
            (loop for e in row do
                (if (= counter op)
                    (setq new-row (append new-row (list simbolo)))
                    (setq new-row (append new-row (list e))) )
                (incf counter) )
            (setq new-tablero (append new-tablero (list new-row)))
            (setq new-row NIL) )
        new-tablero ) )

(defun fin-de-juego? (estado)
"Predicado. Se verifican las condiciones para ver si el tablero está lleno o alguien ganó"
    (let ((gana-agente (list *simbolo* *simbolo* *simbolo* *simbolo*))
          (gana-rival (list *simbolo-rival* *simbolo-rival* *simbolo-rival* *simbolo-rival*))
          (transpuesta (transpuesta estado))
          (aux-diagonal NIL)
          (terminado NIL)
          (lleno T) ) 
        (loop for row in estado do 
            (when (or (equal gana-agente row)(equal gana-rival row)) 
                (setq terminado T) ) )
        (loop for col in transpuesta do 
            (when (or (equal gana-agente col)(equal gana-rival col)) 
                (setq terminado T) ) )
        
        (setq aux-diagonal (list (first (first estado)) (second (second estado)) (third (third estado)) (fourth (fourth estado))))
        (when (or (equal gana-agente aux-diagonal)(equal gana-rival aux-diagonal)) (setq terminado T))

        (setq aux-diagonal (list (fourth (first estado)) (third (second estado)) (second (third estado)) (first (fourth estado))))
        (when (or (equal gana-agente aux-diagonal)(equal gana-rival aux-diagonal)) (setq terminado T))    

        (loop for row in estado do
            (loop for i in row do
                (when (equal i NIL) (setq lleno NIL)) ) )
        (when lleno (setq terminado T))

        terminado ) )

;Función de evaluación heurística del tablero. 
;   Se cuentan las posibilidades que tiene un jugador
;   de ganar con base en el número de sus símbolos sobre una línea.
;   Dicho número se utiliza como exponente para calcular un valor. Se eligió el número 5 porque permite
;   una diferenciación clara entre 'ganar' o 'evitar que el otro gane'
(defun evaluar-tablero(estado simbolo)
"Evaluación basada en el número de símbolos sobre una línea (vertical | horizontal | diagonal)
    *1 punto cero símbolos en la línea
    *5 puntos un símbolo en la línea
    *25 puntos dos símbolos en la línea
    *125 puntos tres símbolos en la línea
    *625 puntos cuatro símbolos en la línea"
    (let ((aux-count 0)
          (total 0)
          (transpuesta (transpuesta estado))
          (aux-diagonal 3) ) 

        ;filas
        (loop for row in estado do
            (loop for e in row do
                (when (equal e simbolo)
                    (incf aux-count) ) )
            (setq total (+ total (expt 5 aux-count)))
            (setq aux-count 0) )
        (setq aux-count 0)

        ;diagonales
        (loop for i from 0 to 3 do
            (when (equal (nth i (nth i estado)) simbolo) 
                (incf aux-count) ) )
        (setq total (+ total (expt 5 aux-count)))
        (setq aux-count 0)

        (loop for i from 0 to 3 do
            (when (equal (nth aux-diagonal (nth i estado)) simbolo) 
                (incf aux-count) )
            (setq aux-diagonal (- aux-diagonal 1)) )

        (setq total (+ total (expt 5 aux-count)))
        (setq aux-count 0)

        ;columnas
        (loop for col in transpuesta do
            (loop for e in col do
                (when (equal e simbolo)
                    (incf aux-count) ) )
            (setq total (+ total (expt 5 aux-count)))
            (setq aux-count 0) )

        total
    )
)

(defun evaluacion (estado)
"Evaluación a favor del tablero menos la evaluación en contra del tablero"
    (- (evaluar-tablero estado *simbolo*) (evaluar-tablero estado *simbolo-rival*)) )

(defun valor-minimax-podas (estado jugador profundidad alpha beta) 
"Función recursiva para ejecutar la planeación Minimax con podas alpha-beta"
    (let ((mejor-mov NIL) 
          (mejor-valor NIL) 
          (nuevo-estado NIL)
          (valor NIL) )
        (if (or (fin-de-juego? estado) (= profundidad *profundidad-max*))
            (return-from valor-minimax-podas (list (evaluacion estado) NIL))
            (progn 
            (if (equal jugador *simbolo* ) (setq mejor-valor most-negative-fixnum)(setq mejor-valor most-positive-fixnum))
            (dolist  (op  *ops*)  
                (when (operador-valido? estado op)
                    (if (equal jugador *simbolo*)
                    (progn 
                        (setq  nuevo-estado  (aplicar-operador estado op *simbolo*))
                        (setq valor (first (valor-minimax-podas nuevo-estado *simbolo-rival* (+ 1 profundidad) alpha beta)))
                        (when (> valor mejor-valor)
                            (setq mejor-valor valor)
                            (setq mejor-mov op) )
                        (setq alpha (max alpha valor))
                        (if (<= beta alpha) (return-from valor-minimax-podas (list mejor-valor mejor-mov))) )
                    (progn
                        (setq  nuevo-estado  (aplicar-operador estado op *simbolo-rival*))
                        (setq valor (first (valor-minimax-podas nuevo-estado *simbolo* (+ 1 profundidad) alpha beta)))
                        (when (< valor mejor-valor)
                            (setq mejor-valor valor)
                            (setq mejor-mov op) ) 
                        (setq beta (min beta valor))
                        (if (<= beta alpha) (return-from valor-minimax-podas (list mejor-valor mejor-mov))) )
                            
                    )      
                ) 
            )
            (return-from valor-minimax-podas (list mejor-valor mejor-mov)) ) ) ) )

(defun tictactoe (tablero)
"Función de juego para interactuar con la interfaz gráfica de la plataforma"
    (let ((mejor-valor NIL) 
          (resultado-minimax NIL) ) 
        (setq resultado-minimax (valor-minimax-podas tablero *simbolo* 0 most-negative-fixnum most-positive-fixnum))
        (setq mejor-valor (first resultado-minimax))
        (setq *output* (second resultado-minimax)) ) )
