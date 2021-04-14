;;;======================================================================================
;;;  Ranas.lisp
;;;      Resuelve el problema de ranas que cruzan el estanque con búsqueda ciega, a lo profundo y a lo ancho.
;;;   
;;;      Representación de los estados: 
;;;         Lista con 3 elementos: un cojunto para indicar las posiciones de las ranas verdes,
;;;         un atomo numérico para representar la posición libre en el estanque y un segundo
;;;         conjunto para indicar las posiciones de las ranas cafés.
;;;         Se consideran las posiciones en el estanque iniciando desde 0.
;;;
;;;         _v_  _v_  _v_  ___  _c_  _c_  _c_ 
;;;          0    1    2    3    4    5    6  
;;;     
;;;         Estado inicial:            Ejemplo Estado meta:            
;;;         ((0 1 2) 3 (4 5 6))       ((4 5 6) 3 (0 1 2))
;;;
;;;         Notar que en el estado meta las ranas de cada color pueden encontrarse en cualquier posición,
;;;         y gracias a que están representadas las orillas como conjuntos, mientras estén del lado
;;;         opuesto a su inicio se considera meta, por lo cual ((6 4 5) 3 (1 0 2)) seguiría siendo meta.
;;;
;;;      Christopher Osornio Sánchez
;;;  Abril, 2021
;;;======================================================================================
(defparameter  *open* '())    ;; Frontera de busqueda                                             
(defparameter  *memory* '())  ;; Memoria de intentos previos

(defparameter  *ops*  '( (:Rana-Verde-Uno-Adelante         (0  1))
                         (:Rana-Verde-Dos-Adelante         (0  2))
                         (:Rana-Verde-Uno-Atrás            (0 -1))
                         (:Rana-Verde-Dos-Atrás            (0 -2))
                         (:Rana-Café-Uno-Adelante          (1 -1))
                         (:Rana-Café-Dos-Adelante          (1 -2))
                         (:Rana-Café-Uno-Atrás             (1  1))
                         (:Rana-Café-Dos-Atrás             (1  2))))

(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria

;;indicadores de desempeño
(defparameter *nodos-creados* 0)
(defparameter *nodos-expandidos* 0)
(defparameter *lon-max-open* 0)

;;;=======================================================================================
;;  crear-nodo (estado  op index)  
;;      Permite crear un nodo de búsqueda con el [estado] actual, el operador [op]
;;      que lo llevó ahí y el [index] de la rana sobre la cual se aplicó dicho operador
;;;=======================================================================================
(defun  crear-nodo (estado  op index)
  "Construye y regresa un nuevo nodo de búsqueda con el un estado y el operador que lo llevó ahí"
      (incf  *id*) 
      (incf  *nodos-creados*)
      (list  *id*  estado  *current-ancestor*  (first op) index)  )  

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
(defun insertar-en-open (estado  op  metodo index) 
"Permite insertar nodos de la frontera de búsqueda *open* utilizando el parámetro metodo"
     (let ((nodo  (crear-nodo  estado  op index)))
         (cond ((eql  metodo  :depth-first)
	                  (progn (push  nodo  *open*) 
                        (when (> (length *open*) *lon-max-open*) (setq *lon-max-open* (length *open*)))) )
	           ((eql  metodo  :breadth-first)
		          (progn (setq  *open* (append  *open*  (list nodo)))
                  (when (> (length *open*) *lon-max-open*) (setq *lon-max-open* (length *open*)) ))) 
	   	   (T  Nil)))  )


(defun sacar-de-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *open*))

;;;=======================================================================================
;;  rana-que-se-mueve(estado op)
;;        Regresa la posición de la rana que puede ejecutar el operador [op].
;;;=======================================================================================
(defun  rana-que-se-mueve (estado op)
"Regresa la posición de la rana que puede ejecutar el operador [op]"
    (let ((ranas NIL) 
          (index NIL)  
          (libre (second estado)) 
          (operador (first (second op))) 
          (saltos (second (second op))))
        (case operador
            (0 (progn
                (setq ranas (first estado))
                (loop for i from 0 to 2 
                    if (= (+ (nth i ranas) saltos) libre) do
                    (setq index i))
                index))
            (1 (progn
                (setq ranas (third estado))
                (loop for i downfrom 2 to 0 
                    if (= (+ (nth i ranas) saltos) libre) do
                    (setq index i))
                index))
            (T "Error"))))

;;;=======================================================================================
;;  aplicar-operador (op estado pos)
;;        Cambia el estado del sistema
;;;=======================================================================================
(defun  aplicar-operador (op estado pos) 
"Obtiene el descendiente de [estado] al aplicarle [op] a la rana cuyo indice es [pos] en su respectivo color"
    (let*  ((operador (first op))
            (saltos (second (second op)))
            (ranas-verdes (first estado))
            (ranas-cafés (third estado))
            (rana-verde-1 (first ranas-verdes))
            (rana-verde-2 (second ranas-verdes))
            (rana-verde-3 (third ranas-verdes))
            (rana-café-1 (first ranas-cafés))
            (rana-café-2 (second ranas-cafés))
            (rana-café-3 (third ranas-cafés))
            (espacio (second estado ))
            )
    (if (or (eql operador :Rana-Verde-Uno-Adelante)
            (eql operador :Rana-Verde-Dos-Adelante)
            (eql operador :Rana-Verde-Uno-Atrás)
            (eql operador :Rana-Verde-Dos-Atrás))
        (if (= pos 0) (list (list espacio rana-verde-2 rana-verde-3) rana-verde-1 ranas-cafés)
                (if (= pos 1) (list (list rana-verde-1 espacio rana-verde-3) rana-verde-2 ranas-cafés)
                    (list (list rana-verde-1 rana-verde-2 espacio) rana-verde-3 ranas-cafés)))
        
        (if (or (eql operador :Rana-Café-Uno-Adelante)
            (eql operador :Rana-Café-Dos-Adelante)
            (eql operador :Rana-Café-Uno-Atrás)
            (eql operador :Rana-Café-Dos-Atrás))
            
            (if (= pos 0) (list ranas-verdes rana-café-1 (list espacio rana-café-2 rana-café-3))
                (if (= pos 1) (list ranas-verdes rana-café-2 (list rana-café-1 espacio rana-café-3))
                    (list ranas-verdes rana-café-3 (list rana-café-1 rana-café-2 espacio))))))))

;;;=======================================================================================
;;  expandir (estado)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;=======================================================================================
(defun expandir (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
    (let ((descendientes  nil)
	    (nuevo-estado  nil)
        (index NIL))
        (incf  *nodos-expandidos*) 
        (dolist  (op  *Ops*  descendientes)  
            (setq index (rana-que-se-mueve estado op)) 
            (when (numberp index)
                (setq  nuevo-estado  (aplicar-operador op estado index))         
	            (setq  descendientes  (cons  (list nuevo-estado op index) descendientes))))))

;;;=======================================================================================
;;  estado-conocido?  y  filtrar-conocidos
;;        Permiten administrar la memoria de intentos previos
;;  el estado tiene estructura:  [(<rv1><rv2><rv3>) <libre> (<rc1><rc2><rc3>)],
;;  el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> <index>]
;;;=======================================================================================
(defun  estado-conocido?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos
Nota: Las comparaciones entre estados se realizan como conjuntos, es decir, son iguales
      si contienen los mismos elementos en cada orilla"
     (let* ((edo-mem (second (first  lista-memoria)))
           (ranas-verdes-mem (sort (copy-seq (first edo-mem)) #'>))
           (ranas-cafés-mem (sort (copy-seq (third edo-mem)) #'>))
           (libre-mem (second edo-mem))
           (ranas-verdes-estado (sort (copy-seq (first estado)) #'>))
           (ranas-cafés-estado (sort (copy-seq (third estado)) #'>))
           (libre-estado (second estado))  )
     (cond ((null  lista-memoria)  Nil)
	        ((and (equal ranas-verdes-mem ranas-verdes-estado) 
                  (equal ranas-cafés-mem ranas-cafés-estado) 
                  (equal libre-mem libre-estado))  T)  
		(T  (estado-conocido?  estado  (rest  lista-memoria)))) ) )


(defun  filtrar-conocidos (lista-estados-y-ops) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
     la lista de estados y operadores tiene estructura: [(<estado> <op> <index>) (<estado> <op> <index>) ... ]"
     (cond ((null  lista-estados-y-ops)  Nil)
	       ((estado-conocido? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda el primer elemento de la lista, filtrarlo...
		       (filtrar-conocidos  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filtrar-conocidos  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta

;;;=======================================================================================
;;  obtener-solucion  y  desplegar-solucion
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       obtener-solucion   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       desplegar-solucion  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema...
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
		   (format t "\(~2A\)  aplicando ~24A sobre rana ~A se llega a ~2A~%"  i (fourth  nodo) (fifth nodo) (second  nodo)))))  ) 

;;;=======================================================================================
;;  es-meta?
;;
;;       Predicado que permite saber si un estado es meta, es decir, las tres ranas verdes ocupan
;;       las posiciones 4, 5 y 6 en cualquier orden; la posición libre es la 3 y las ranas cafés
;;       ocupan las posiciones 0, 1 y 2 en cualquier orden.
;;
;;;======================================================================================= 
(defun es-meta? (edo-meta estado)
"Verifica si un estado es el estado meta, comparando las posiciones de las ranas como conjuntos"
    (let ((ranas-verdes-meta (first edo-meta))
          (ranas-cafés-meta (third edo-meta))
          (libre-meta (second edo-meta))
          (ranas-verdes-estado (first estado))
          (ranas-cafés-estado (third estado))
          (libre-estado (second estado))
        )
        (and (= (length (intersection ranas-verdes-meta ranas-verdes-estado)) 3)
             (= (length (intersection ranas-cafés-meta ranas-cafés-estado)) 3)
             (= libre-meta libre-estado))))

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
     (setq  *solucion*  nil)
     (setq *nodos-creados* 0)
     (setq *nodos-expandidos* 0)
     (setq *lon-max-open* 0) )

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

      (insertar-en-open   edo-inicial  nil  metodo nil)
      (loop until  (or  meta-encontrada
                        (null *open*))  do
	   (setq nodo    (sacar-de-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
		     estado  (second  nodo)               ;;Identificar el estado y operador que contiene
		     operador  (third  nodo))             
	   (push  nodo  *memory*)                     ;;Recordarlo antes de que algo pueda pasar...
	   (cond    ((es-meta?  edo-meta  estado)  
		                (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
		                (desplegar-solucion  (obtener-solucion  nodo))
		                (setq  meta-encontrada  T))
		         (t (setq  *current-ancestor*  (first  nodo)) 
			     (setq  sucesores  (expandir estado))
			     (setq  sucesores  (filtrar-conocidos  sucesores))     ;;Filtrar los estados ya revisados...
			      (loop for  element  in  sucesores  do
				    (insertar-en-open  (first element)  (second element)  metodo (third element) ))))))  )
			     
(defun busqueda-ciega-con-indicadores (edo-inicial edo-meta metodo)
    (let ((tiempo1 0) (tiempo2 0) (tiempo-total 0))
        (setq tiempo1 (get-internal-run-time ) )
        (blind-search edo-inicial edo-meta metodo)
        (setq tiempo2 (get-internal-run-time ) )
        (setq tiempo-total (/ (- tiempo2 tiempo1) internal-time-units-per-second ))
        (format  t  "~%Indicadores de desempeño~%")
        (format  t  "Nodos creados: ~A~%" *nodos-creados*)
        (format  t  "Nodos expandidos: ~A~%" *nodos-expandidos*)
        (format  t  "Longitud máxima de la frontera de búsqueda: ~A~%" *lon-max-open*)
        (format  t  "Longitud de la solución: ~A operadores~%" (length *solucion*))
        (format  t  "Tiempo para encontrar la solución: ~,2f segundos~%" tiempo-total) ) )
;;;=======================================================================================
;;;=======================================================================================
;(busqueda-ciega-con-indicadores '((0 1 2) 3 (4 5 6)) '((4 5 6) 3 (0 1 2)) :breadth-first )
;(busqueda-ciega-con-indicadores '((0 1 2) 3 (4 5 6)) '((6 5 4) 3 (2 1 0)) :depth-first )