;;;======================================================================================
;;;  GLOL.lisp
;;;      Resuelve el problema de granjero, lobo, oveja y legumbre con búsqueda ciega, a lo profundo y a lo ancho.
;;;   
;;;      Representación de los estados: 
;;;         Lista con dos conjuntos internos que simbolizan ambas orillas.
;;;         Cada conjunto (sublista interna) contiene cadenas que simbolizan a cada elemento del problema:
;;;             - "G"  -> Granjero
;;;             - "LO" -> Lobo
;;;             - "O"  -> Oveja
;;;             - "LE" -> Legumbre
;;;
;;;                 Estado inicial:               Estado meta:
;;;               (("G" "LO" "O" "LE") ( ))       (( ) ("G" "LO" "O" "LE"))
;;;
;;;      Christopher Osornio Sánchez
;;;  Abril, 2021
;;;======================================================================================
(defparameter  *open* '())    ;; Frontera de busqueda...                                              
(defparameter  *memory* '())  ;; Memoria de intentos previos

(defparameter  *ops*  '( (:Moverse-Solo          ("G"))
                         (:Mover-Lobo            ("G" "LO"))
                         (:Mover-Oveja           ("G" "O"))
                         (:Mover-Legumbre        ("G" "LE"))  ))

(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria

;;;=======================================================================================
;;  crear-nodo (estado  op)  
;;      Permite crear un nodo de búsqueda con el [estado] actual y el operador [op]
;;      que lo llevó ahí
;;;=======================================================================================
(defun  crear-nodo (estado  op)
  "Construye y regresa un nuevo nodo de búsqueda con el un estado y el operador que lo llevó ahí"
      (incf  *id*)  
      (list  *id*  estado  *current-ancestor*  (first op) ) )  

;;;=======================================================================================
;;  insertar-en-open   y   sacar-de-open  
;;        
;;        insertar-en-open  
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breadth-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        
;;        sacar-de-open  el primer elemento de *open* siempre será el correcto para backtracking
;;                       gracias al método de inserción
;;;=======================================================================================
(defun insertar-en-open (estado  op  metodo) 
"Permite insertar nodos de la frontera de búsqueda *open* utilizando el parámetro metodo"
     (let ((nodo  (crear-nodo  estado  op)))
         (cond ((eql  metodo  :depth-first)
	                  (push  nodo  *open*))
	           ((eql  metodo  :breadth-first)
		          (setq  *open*  (append  *open*  (list nodo))))
	   	   (T  Nil)))  )

(defun sacar-de-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *open*))

;;;=======================================================================================
;;  tiene-granjero? (orilla)
;;        Predicado.  Indica si la orilla (una lista simple) contiene al granjero ("G")     
;;
;;;=======================================================================================
(defun tiene-granjero? (orilla) 
"Predicado.  Indica si la orilla (una lista simple) contiene al granjero (\"G\")"   
    (if (null orilla) NIL   
        (if (equal (first orilla) "G") T 
            (tiene-granjero? (rest orilla)) ) ) )

;;;=======================================================================================
;;  tiene-elemento? (orilla elemento)
;;        Predicado.  Indica si la [orilla] (una lista simple) contiene al [elemento]    
;;
;;;=======================================================================================
(defun tiene-elemento? (orilla elemento) 
"Predicado.  Indica si la orilla (una lista simple) contiene al [elemento]"   
    (if (null orilla) NIL   
        (if (equal (first orilla) elemento) T 
            (tiene-elemento? (rest orilla) elemento) ) ) )

;;;=======================================================================================
;;  operador-valido [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado],
;;                    es decir, se verifica que el granjero este en la misma orilla que el
;;                    elemento a mover (lobo, oveja o legumbre)
;;;=======================================================================================
(defun  operador-valido? (op  estado)
"Predicado.  Indica si es posible aplicar el operador [op] a [estado]
     el estado tiene estructura:  [(<entidades del problema>) (<entidades del problema>)],
     el operador tiene estructura : [<etiqueta-humana> <lista operador con (<entidades del problema>)>]"  
    (let*  ((orilla-izq  (first  estado))                         
	        (orilla-der  (second estado))
            (operador  (first op))
            (elementos  (second op))
        )
        (if (equal operador :Moverse-Solo) T 
            (if (or (equal operador :Mover-Lobo) (equal operador :Mover-Oveja) (equal operador :Mover-Legumbre)) 
                (if (tiene-granjero? orilla-izq)
                    (tiene-elemento? orilla-izq (second elementos))
                    (tiene-elemento? orilla-der (second elementos)))
                "error")) ) )

;;;=======================================================================================
;;  estado-valido? (estado)
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema.
;;        Es decir, en ninguna orilla se pueden quedar las siguientes combinaciones sin la
;;        supervisión del granjero: ("LO" "O"), ("O" "LE")        
;;
;;;=======================================================================================
(defun  estado-valido? (estado)
"Predicado. Valida  un estado según las restricciones generales del problema.
       el estado tiene estructura:  [(<entidades del problema>) (<entidades del problema>)]"
    (let ((orilla-izq (first estado)) 
          (orilla-der (second estado)) 
          (invalido-1 '("LO" "O"))
          (invalido-2 '("LE" "O")))

        (if (tiene-granjero? orilla-izq) 
            (not (or (equal orilla-der invalido-1) (equal orilla-der invalido-2) 
                     (equal orilla-der (reverse invalido-1)) (equal orilla-der (reverse invalido-2))))
            (not (or (equal orilla-izq invalido-1) (equal orilla-izq invalido-2) 
                     (equal orilla-izq (reverse invalido-1)) (equal orilla-izq (reverse invalido-2)))) ) ) )

;;;=======================================================================================
;;  aplicar-operador (op estado)
;;        Cambia el estado del sistema
;;;=======================================================================================
(defun aplicar-operador (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let ((orilla-izq (first estado))
          (orilla-der (second estado))
          (operador (first op)) 
          (elementos (second op))
    )
    (if (equal operador :Moverse-Solo) 
        (if (tiene-granjero? orilla-izq) 
            (list (remove (first elementos) orilla-izq :test #'equal) (union elementos orilla-der :test #'equal))
            (list (union elementos orilla-izq :test #'equal) (remove (first elementos) orilla-der :test #'equal)) )

            (if (or (equal operador :Mover-Lobo) (equal operador :Mover-Oveja) (equal operador :Mover-Legumbre))
                (if (tiene-granjero? orilla-izq) 
                    (list (remove (second elementos) (remove (first elementos) orilla-izq :test #'equal) :test #'equal) (union elementos orilla-der :test #'equal))
                    (list (union elementos orilla-izq :test #'equal) (remove (second elementos) (remove (first elementos) orilla-der :test #'equal) :test #'equal)) )
                "error")) ) )

;;;=======================================================================================
;;  expandir (estado)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;=======================================================================================
(defun expandir (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
    (let ((descendientes  nil)
	    (nuevo-estado  nil))
        
        (dolist  (op  *Ops*  descendientes)  
            (when (operador-valido? op estado)
                (setq  nuevo-estado  (aplicar-operador op estado))
                (when (estado-valido? nuevo-estado)
                    (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))) ) )

;;;=======================================================================================
;;  estado-conocido?  y  filtrar-conocidos
;;        Permiten administrar la memoria de intentos previos
;;     el estado tiene estructura:  [(<entidades del problema>) (<entidades del problema>)],
;;     el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador>]
;;;=======================================================================================
(defun  estado-conocido?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos
Nota: Las comparaciones entre estados se realizan como conjuntos, es decir, son iguales
      si contienen los mismos elementos en cada orilla" 
     (let* ( (estado-memoria (second (first  lista-memoria)))
             (orilla-izq-mem (sort (copy-seq (first estado-memoria)) #'string-lessp))
             (orilla-der-mem (sort (copy-seq (second estado-memoria)) #'string-lessp))
             (orilla-izq-est (sort (copy-seq (first estado)) #'string-lessp))
             (orilla-der-est (sort (copy-seq (second estado)) #'string-lessp)) ) 
     (cond ((null  lista-memoria)  Nil)
	        ( (and (equal orilla-izq-mem orilla-izq-est) (equal orilla-der-mem orilla-der-est))  T)  
		(T  (estado-conocido?  estado  (rest  lista-memoria))))  ) )


(defun  filtrar-conocidos (lista-estados-y-ops) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
     (cond ((null  lista-estados-y-ops)  Nil)
	       ((estado-conocido? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda el primer elemento de la lista, filtrarlo
		       (filtrar-conocidos  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filtrar-conocidos  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta   

;;;=======================================================================================
;;  obtener-solucion  y  desplegar-solucion
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       obtener-solucion   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial.
;;       desplegar-solucion  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema.
;;;=======================================================================================
(defun obtener-solucion (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
     (labels ((locate-node  (id  lista)       
		  (cond ((null  lista)  Nil)
		        ((eql  id  (first (first  lista))) (first  lista))
		        (T  (locate-node  id (rest  lista))))))
	  (let ((current  (locate-node  (first  nodo)  *memory*)))
	     (loop  while  (not (null  current))  do                        
		 (push  current  *solucion*)     
		 (setq  current  (locate-node  (third  current) *memory*))))  
	     *solucion*))

(defun  desplegar-solucion (lista-nodos)
"Despliega la solución en forma conveniente y numerando los pasos"
    (format  t  "Solución con ~A  pasos:~%~%" (1- (length  lista-nodos)))
    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
	      (if  (= i 0)
		   (format t "Inicio en: ~A~%" (second  nodo))  
		   (format t "\(~2A\)  aplicando ~16A se llega a ~2A~%"  i (fourth  nodo) (second  nodo)))))  ) 	   

;;;=======================================================================================
;;  es-meta?
;;
;;       Predicado que permite saber si un estado es meta, es decir, todos los elementos 
;;       del problema (granjero, lobo, oveja y legumbre) cruzaron a la orilla contraria.
;;;======================================================================================= 
(defun es-meta? (edo-meta estado)
"Verifica si un estado es el estado meta, comparando las orillas como conjuntos"
    (let ((orilla-izq-meta (first edo-meta))
          (orilla-der-meta (second edo-meta))
          (orilla-izq-estado (first estado))
          (orilla-der-estado (second estado)) )
        (and (equal NIL orilla-izq-meta) (equal NIL orilla-izq-estado)
             (= (length (intersection orilla-der-meta orilla-der-estado :test #'equal)) 4) ) ) )    

;;;=======================================================================================
;;  reiniciar-variables-globales  y  BLIND-SEARCH
;;
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;
;;       reiniciar-variables-globales   Reinicia todas las variables globales para una nueva ejecución
;;       blind-search  Función principal, realiza búsqueda desde un estado inicial a un estado meta
;;;=======================================================================================
(defun reiniciar-variables-globales () 
"Reinicia todas las variables globales para realizar una nueva búsqueda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil))

(defun  blind-search (edo-inicial  edo-meta  metodo)
"Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :depth-first - búsqueda en profundidad
                               :breadth-first - búsqueda en anchura"
  (reiniciar-variables-globales)
  (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil))

      (insertar-en-open   edo-inicial  nil  metodo)
      (loop until  (or  meta-encontrada
                        (null *open*))  do
	   (setq nodo    (sacar-de-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
		     estado  (second  nodo)               ;;Identificar el estado y operador que contiene
		     operador  (third  nodo))             
	   (push  nodo  *memory*)                     
	   (cond    ((es-meta?  edo-meta  estado)  
		                (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
		                (desplegar-solucion  (obtener-solucion  nodo))
		                (setq  meta-encontrada  T))
		         (t (setq  *current-ancestor*  (first  nodo)) 
			     (setq  sucesores  (expandir estado))
			     (setq  sucesores  (filtrar-conocidos  sucesores))     ;;Filtrar los estados ya revisados...
			      (loop for  element  in  sucesores  do
				    (insertar-en-open  (first element)  (second element)  metodo ))))))  )    

;;;=======================================================================================
;;;=======================================================================================
;(blind-search (list '("G" "LO" "LE" "O") '() ) (list '() '("G" "LO" "LE" "O") ) :breadth-first )
(blind-search (list '("G" "LO" "LE" "O") '() ) (list '() '("G" "LO" "LE" "O") ) :depth-first )