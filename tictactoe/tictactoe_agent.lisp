;;;======================================================================================
;;;  Agente jugador de gato 4x4
;;;
;;;     Se implementa el algoritmo MinMax con podas Alfa-Beta.
;;; 
;;;  Christopher Osornio Sánchez
;;;  Mayo, 2021
;;;======================================================================================

(defparameter  *ops*  '(6 7 10 11 1 4 13 16 2 3 5 9 8 12 14 15) )
(defparameter  *profundidad-max*  4)
(defparameter  *simbolo* 'O)
(defparameter  *simbolo-rival* 'X)

(defun obtener-tablero-hash (tablero)
    (let ((counter 1) (tablero-hash (make-hash-table :test 'equal)))
        (loop for i in tablero do
            (loop for j in i do 
                (setf (gethash counter tablero-hash) j)
                (incf  counter) ) )
        tablero-hash ) )

(defun operador-valido? (tablero op)
    (let ((tablero-hash (obtener-tablero-hash tablero)))
        (not (or (equal (gethash op tablero-hash) 'O) (equal (gethash op tablero-hash) 'X))) 
    )
)

(defun aplicar-operador (estado op simbolo) 
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

(defun gano-alguien (estado)
    (let ((gana-agente (list *simbolo* *simbolo* *simbolo* *simbolo*))
          (gana-rival (list *simbolo-rival* *simbolo-rival* *simbolo-rival* *simbolo-rival*))
          (transpuesta (transpuesta estado))
          (aux-diagonal NIL)
          (ganador NIL)
          (lleno T)
        ) 
        (loop for row in estado do 
            (when (equal gana-agente row)
                (setq ganador *simbolo*)
            )
            (when (equal gana-rival row) 
                (setq ganador *simbolo-rival*)
            )
        )
        (loop for col in transpuesta do 
            (when (equal gana-agente col)
                (setq ganador *simbolo*)
            )
            (when (equal gana-rival col) 
                (setq ganador *simbolo-rival*)
            )
        )
        
        (setq aux-diagonal (list (first (first estado)) (second (second estado)) (third (third estado)) (fourth (fourth estado))))
        (when (equal gana-agente aux-diagonal) (setq ganador *simbolo*))
        (when (equal gana-rival aux-diagonal) (setq ganador *simbolo-rival*))

        (setq aux-diagonal (list (fourth (first estado)) (third (second estado)) (second (third estado)) (first (fourth estado))))
        (when (equal gana-agente aux-diagonal) (setq ganador *simbolo*))
        (when (equal gana-rival aux-diagonal) (setq ganador *simbolo-rival*))   

        ganador
    )
)

(defun fin-de-juego? (estado)
    (let ((gana-agente (list *simbolo* *simbolo* *simbolo* *simbolo*))
          (gana-rival (list *simbolo-rival* *simbolo-rival* *simbolo-rival* *simbolo-rival*))
          (transpuesta (transpuesta estado))
          (aux-diagonal NIL)
          (terminado NIL)
          (lleno T)
        ) 
        (loop for row in estado do 
            (when (or (equal gana-agente row)(equal gana-rival row)) 
                (setq terminado T)
            )
        )
        (loop for col in transpuesta do 
            (when (or (equal gana-agente col)(equal gana-rival col)) 
                (setq terminado T)
            )
        )
        
        (setq aux-diagonal (list (first (first estado)) (second (second estado)) (third (third estado)) (fourth (fourth estado))))
        (when (or (equal gana-agente aux-diagonal)(equal gana-rival aux-diagonal)) (setq terminado T))

        (setq aux-diagonal (list (fourth (first estado)) (third (second estado)) (second (third estado)) (first (fourth estado))))
        (when (or (equal gana-agente aux-diagonal)(equal gana-rival aux-diagonal)) (setq terminado T))    

        (loop for row in estado do
            (loop for i in row do
                (when (equal i NIL) (setq lleno NIL))
            )
        )
        (when lleno (setq terminado T))

        terminado
    )
)

(defun transpuesta (tablero)
    (apply #'mapcar #'list tablero))

(defun evaluar (estado)
    (let ((gana-agente 0) 
          (gana-rival 0) 
          (row-ganadora T)
          (col-ganadora T)
          (row-perdedora T)
          (col-perdedora T)
          (transpuesta (transpuesta estado))
        ) 
        ;filas
        (loop for row in estado do
            (loop for e in row do
                (when (equal e *simbolo-rival*)
                    (setq row-ganadora NIL)
                ) 
            )
            (when row-ganadora (incf gana-agente))
            (setq row-ganadora T)
        )
        (loop for row in estado do
            (loop for e in row do
                (when (equal e *simbolo*)
                    (setq row-perdedora NIL)
                ) 
            )
            (when row-perdedora (incf gana-rival))
            (setq row-perdedora T)
        )

        ;diagonales
        (if (or (equal (first (first estado)) *simbolo-rival*)
                (equal (second (second estado)) *simbolo-rival*)
                (equal (third (third estado)) *simbolo-rival*)
                (equal (fourth (fourth estado)) *simbolo-rival*)
            )
            (when (not (or (equal (first (first estado)) *simbolo*)
                        (equal (second (second estado)) *simbolo*)
                        (equal (third (third estado)) *simbolo*)
                        (equal (fourth (fourth estado)) *simbolo*)
                    ))
                (incf gana-rival)
            )
            (if (or (equal (first (first estado)) *simbolo*)
                    (equal (second (second estado)) *simbolo*)
                    (equal (third (third estado)) *simbolo*)
                    (equal (fourth (fourth estado)) *simbolo*)
                )
                (when (not (or (equal (first (first estado)) *simbolo-rival*)
                        (equal (second (second estado)) *simbolo-rival*)
                        (equal (third (third estado)) *simbolo-rival*)
                        (equal (fourth (fourth estado)) *simbolo-rival*)
                    ))
                    (incf gana-agente)
                )
                (progn (incf gana-rival) (incf gana-agente))
            )   
        )
        (if (or (equal (fourth (first estado)) *simbolo-rival*)
                (equal (third (second estado)) *simbolo-rival*)
                (equal (second (third estado)) *simbolo-rival*)
                (equal (first (fourth estado)) *simbolo-rival*)
            )
            (when (not (or (equal (fourth (first estado)) *simbolo*)
                    (equal (third (second estado)) *simbolo*)
                    (equal (second (third estado)) *simbolo*)
                    (equal (first (fourth estado)) *simbolo*)
                ))
                (incf gana-rival)
            )
            (if (or (equal (fourth (first estado)) *simbolo*)
                    (equal (third (second estado)) *simbolo*)
                    (equal (second (third estado)) *simbolo*)
                    (equal (first (fourth estado)) *simbolo*)
                )
                (when (not (or (equal (fourth (first estado)) *simbolo-rival*)
                    (equal (third (second estado)) *simbolo-rival*)
                    (equal (second (third estado)) *simbolo-rival*)
                    (equal (first (fourth estado)) *simbolo-rival*)
                    ))
                    (incf gana-agente)
                )
                (progn (incf gana-rival) (incf gana-agente))
            )
        )
    
        ;columnas
        (loop for col in transpuesta do
            (loop for e in col do
                (when (equal e *simbolo-rival*)
                    (setq col-ganadora NIL)
                ) 
            )
            (when col-ganadora (incf gana-agente))
            (setq col-ganadora T)
        )
        (loop for col in transpuesta do
            (loop for e in col do
                (when (equal e *simbolo*)
                    (setq col-perdedora NIL)
                ) 
            )
            (when col-perdedora (incf gana-rival))
            (setq col-perdedora T)
        )
        (if (equal (gano-alguien estado) *simbolo*) 10 
            (if (equal (gano-alguien estado) *simbolo-rival*) -10 
                (- gana-agente gana-rival) ) )
    )
)

(defun valor-minimax (estado jugador profundidad) ;agente será jugador = 0
    (let ((mejor-mov NIL) 
          (mejor-valor NIL) 
          (nuevo-estado NIL)
          (valor NIL) )
        (if (or (fin-de-juego? estado) (= profundidad *profundidad-max*))
            (list (evaluar estado) NIL)
            (progn 
                (if (= jugador 0 ) (setq mejor-valor most-negative-fixnum)(setq mejor-valor most-positive-fixnum))
                (dolist  (op  *ops*)  
                    (when (operador-valido? estado op)
                        (if (= jugador 0)
                            (progn 
                                (setq  nuevo-estado  (aplicar-operador estado op *simbolo*))
                                (setq valor (first (valor-minimax nuevo-estado 1 (+ 1 profundidad))))
                                (when (> valor mejor-valor)
                                    (setq mejor-valor valor)
                                    (setq mejor-mov op)
                                )  
                            )
                            (progn
                                (setq  nuevo-estado  (aplicar-operador estado op *simbolo-rival*))
                                (setq valor (first (valor-minimax nuevo-estado 0 (+ 1 profundidad))))
                                (when (< valor mejor-valor)
                                    (setq mejor-valor valor)
                                    (setq mejor-mov op)
                                )  
                            )
                            
                        )      
                    ) 
                )
                (list mejor-valor mejor-mov)
            )
        )
    )
)

(defun tictactoe (tablero)
    (let ((mejor-valor NIL) 
          (resultado-minimax NIL)
          ) 
        (setq resultado-minimax (valor-minimax tablero 0 0))
        (setq mejor-valor (first resultado-minimax))
        (setq *output* (second resultado-minimax))
    )
)

(defparameter board '(( NIL NIL NIL NIL) ( NIL X NIL NIL) (NIL NIL NIL NIL) (NIL NIL NIL NIL)) )
(tictactoe board)

