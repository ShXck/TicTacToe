#lang racket/gui

;NOMENGLATURA
;0 -> Casilla vacía.
;1 -> Casilla con X.
;2 -> Casilla con O.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;GREEDY ALGORITHM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Función principal donde se verifica ganador y si no hay, ejecuta el algoritmo.
(define (run-main board)
  (cond ((equal? (solution-func board) 2) (display "Machine Wins!"))
        ((equal? (solution-func board) 1) (display "Player Wins!"))
        (else (run-algorithm board))))  

;Corre el algoritmo codicioso.
(define (run-algorithm board)
  (make-system-move board (selection-func (objective-func (feasibility-func (candidates-func board) board) board)) ))

;Hace la jugada usando el resultado del algoritmo.
(define (make-system-move board coords)
  (cons coords (change-symbol board (car coords) (cadr coords) 2)))
  
;Calcula los candidatos para el siguiente movimiento.
(define (candidates-func board)
  (cond ((null? board) '())
        (else (get-candidate-cells (append (get-symbol-pos board 1 0) (get-symbol-pos board 2 0)  ))))) ;TODO: cuando no haya ningun circulo elegir una casilla aleatoria.

;Evalua los candidatos y verifica que sean movimientos válidos.
(define (feasibility-func candidates board)
  (cond ((null? candidates) '())
        (else (cond ((and (exists? (caar candidates) (car (cdr (car candidates))) (get-width board) (get-height board))
                          (equal? (get-symbol (caar candidates) (car(cdr(car candidates))) board) 0)) (cons (car candidates) (feasibility-func (cdr candidates) board)))
                    (else (feasibility-func (cdr candidates) board) ) ))))

;Asigna una calificación a cada posición candidata.
(define (objective-func newcandidates board)
  (cond ((null? newcandidates) '())
        (else (cons (cons (+ (count-element-row (get-row (caar newcandidates) board) 2) (count-element-row (get-row (caar newcandidates) board) 1) (count-element-col (car(cdr(car newcandidates))) 2 board) (count-element-col (car(cdr(car newcandidates))) 1 board)) (car newcandidates) ) (objective-func (cdr newcandidates) board))))) 

;Función de selección: elige a uno de los mejores candidatos para servir como solución parcial.
(define (selection-func ratedcandidates)
  (list-ref (remove-duplicates (selection-aux ratedcandidates)) (random (round(/ (listlen(remove-duplicates (selection-aux ratedcandidates))) 2))) )) 


;Obtiene los mejores candidatos de la selección.
(define (selection-aux ratedcandidates)
  (cond ((null? ratedcandidates) '())
        (else (cond ((equal? (caar ratedcandidates) (get-highest-rate ratedcandidates 0)) (cons (cdr(car ratedcandidates)) (selection-aux (cdr ratedcandidates))))
                    (else (selection-aux (cdr ratedcandidates)))))))

;Determina si se ha hallado una solución.
(define (solution-func board)
  (cond ((equal? (winner-row? 2 0 board) (get-height board)) 2)
        ((equal? (winner-col? 2 0 0 board) (get-width board)) 2)
        ((equal? (winner-row? 1 0 board) (get-height board)) 1)
        ((equal? (winner-col? 1 0 0 board) (get-width board)) 1)
        (else 0)))

              
;Determina si hay ganador al llenar una fila completa.
(define (winner-row? symbol acc board)
  (cond ((null? board) acc)
        (else (cond ((> (count-element-row (car board) symbol) acc) (winner-row? symbol (count-element-row (car board) symbol) (cdr board)))
                    (else (winner-row? symbol acc (cdr board)))))))

;Determina si hay ganador al llenar una columna completa.
(define (winner-col? symbol colnumber acc board)
  (cond ((equal? colnumber (get-width board)) acc)
        (else (cond ((> (count-element-col colnumber symbol board) acc) (winner-col? symbol (+ colnumber 1) (count-element-col colnumber symbol board) board))
                    (else (winner-col? symbol (+ colnumber 1) acc board))))))

;Obtiene la mayor calificación de los candidatos.
(define (get-highest-rate ratedcandidates highest)
  (cond ((null? ratedcandidates) highest)
        (else (cond ((> (caar ratedcandidates) highest) (get-highest-rate (cdr ratedcandidates) (caar ratedcandidates)))
                    (else (get-highest-rate (cdr ratedcandidates) highest)))))) 
  
;Cuenta las apariciones de un elemento en una fila.
(define (count-element-row rowlst ele)
  (cond ((null? rowlst) 0)
        (else (cond ((equal? (car rowlst) ele) (+ (count-element-row (cdr rowlst) ele) 1))
                    (else (count-element-row (cdr rowlst) ele))))))

;Cuenta las apariciones de un elemento en una columna
(define (count-element-col colnumber ele board)
  (cond ((null? board) 0)
         (else (cond ((equal? (list-ref (car board) colnumber) ele) (+ 1 (count-element-col colnumber ele (cdr board))))
                     (else (count-element-col colnumber ele (cdr board)))))))

;Obtiene los candidatos a partir de donde haya puesto circulos el sistema.
(define (get-candidate-cells circles)
  (cond ((null? circles) '())
         (else (append (cal-candidate-cells (cdr (car circles)) (caar circles)) (get-candidate-cells (cdr circles)) ) )))

;Calcula las coordenadas de las celdas adyacentes.
(define (cal-candidate-cells lst row)
  (cond ((null? lst) '())
        (else (append (get-adjacents row (car lst)) (cal-candidate-cells (cdr lst) row)))))

;Función auxiliar que obtiene una lista de coordenadas de celdas adyacentes a un círculo.
(define (get-adjacents row col)
  (append (list(list row (- col 1)))
          (list(list row (+ col 1)))
          (list(list (+ row 1) col))
          (list(list (- row 1) col))
          (list(list (- row 1) (- col 1)))
          (list(list (- row 1) (+ col 1)))
          (list(list (+ row 1) (- col 1)))
          (list(list (+ row 1) (+ col 1)))
          ))

;Obtiene las posiciones de un caracter en específico.
(define (get-symbol-pos board symbol ys)
  (cond ((null? board) '())
        (else (cons (cons ys (get-symbol-pos-aux (car board) symbol 0)) (get-symbol-pos (cdr board) symbol (+ ys 1))))))

;Función auxiliar para obtener la posición de un caracter.
(define (get-symbol-pos-aux lst symbol xs)
    (cond ((null? lst) '())
        (else (cond ((equal? (car lst) symbol) (cons xs (get-symbol-pos-aux (cdr lst) symbol (+ xs 1))))
                    (else (get-symbol-pos-aux (cdr lst) symbol (+ xs 1)))))))
          
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;GAME LOGIC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Función principal del juego.
(define (TTT width height)
  (cond ((valid? width height) (build-board width height))
        (else (display "INVALID PARAMETERS"))
  )
)

;Obtiene el simbolo en una posición del tablero.
(define (get-symbol row col board)
  (cond ((null? board) -1)
        ((exists? row col (get-width board) (get-height board)) (list-ref (list-ref board row) col)) 
        (else -1)))

;Cambia el valor de un item dentro del tablero dadas sus coordenadas.
(define (change-symbol board row col newval)
  (cond
    ((empty? board) '())
    ((= row 0) (cons (change-item-val (car board) col newval) (cdr board)))
    (else (cons (car board) (change-symbol (cdr board) (- row 1) col newval)))))

;Cambia el valor de un item dentro de una lista.
(define (change-item-val lst index newval)
  (cond
    ((empty? lst) '())
    ((= index 0) (cons newval (cdr lst)))
    (else (cons (car lst) (change-item-val (cdr lst) (- index 1) newval)))))
  
;Función que crea el tablero de MxN.
(define (build-board width height)
  (cond ((equal? height 0) '())
        (else (cons (build-board-aux width) (build-board width (- height 1))))
  )
)

;Función auxiliar que crea las filas del tablero.
(define (build-board-aux width)
  (cond ((equal? width 1) (list 0))
        (else (cons 0  (build-board-aux (- width 1))))))
  
    
;Determina si las dimensiones del tablero son válidas.
(define (valid? width height)
  (cond ((and (>= width 3)
              (>= height 3)
              (<= width 10)
              (<= height 10)) #t)
        (else #f)
  )
)

;Determina si las coordenadas existen dentro de la matriz.
(define (exists? row col width height)
  (cond ((and (>= row 0)
              (>= col 0)
              (< row height)
              (< col width)) #t)
         (else #f)))

;Obtiene las dimensiones de la matriz.
(define (get-board-dimensions board)
  (cond ((null? board) (list 0 0))
        (else (list (get-width board) (get-height board)))))

;Obtiene el numero de filas de la matriz.
(define (get-height board)
  (cond ((null? board) 0)
        (else (+ 1 (get-height (cdr board))))))

;Obtiene el número de columnas de la matriz.
(define (get-width board)
  (cond ((null? board) 0)
        (else (size (car board)))))

;Obtiene el número de items en una lista.
(define (size lst)
  (cond ((null? lst) 0)
        (else (+ 1 (size (cdr lst))))))

;Obtiene una fila específica de una matriz.
(define (get-row rownumber board)
  (cond ((null? board) '())
        ((equal? rownumber 0) (car board)) 
        (else (get-row (- rownumber 1) (cdr board)))))

;Elimina los duplicados de una lista.
(define (remove-duplicates lst)
  (cond ((null? lst) '())
        ((member? (car lst) (cdr lst)) (remove-duplicates (cdr lst)))
        (else (cons (car lst) (remove-duplicates (cdr lst))))))

;Verifica si un elemento pertenece a una lista.
(define (member? item lst)
  (cond ((null? lst) #f)
        ((equal? (car lst) item) #t)
        (else (member? item (cdr lst)))))

;Obtiene el largo de una lista.
(define (listlen lst)
  (cond ((null? lst) 0)
        (else (+ 1 (listlen (cdr lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Graphical User Interface;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Variables 
(define total -6)
(define numero_filas 0)
(define numero_columnas 0)
(define board '())
(define machine_play '())
(define buttons '(2 5))

;Contruye un marco/ventana (Ventana inicial )
(define frame (new frame% (label "TicTacToe") (width 300) (height 100)))

(define f (new font%) )

;Hacer un mensaje de texto estático en el marco.
(define msg (new message% (parent frame)(label "Bienvenido a TicTacToe")(font f)  ))

;Hacer un panel para agregar hay botones.
(define panelFilas_Columnas (new horizontal-panel% (parent frame)))

;Hacer un slider para seleccionar la cantidad de filas en el tablero
(define slider1(new slider% (parent panelFilas_Columnas)
                    (label "Filas: ") (min-value 3)  (max-value 10) (font f) 
                    (callback (lambda (slider event)
                                (set! total (- (+ (send slider1 get-value) (send slider2 get-value)) 5) )
                                (send msg2 set-value total) ))
                    ))

;Hacer un slider para seleccionar la cantidad de columnas en el tablero
(define slider2 (new slider% (parent panelFilas_Columnas)
                     (label "Columnas: ") (min-value 3) (max-value 10) (font f)
                     (callback (lambda (slider event)
                                 (set! total (- (+ (send slider1 get-value) (send slider2 get-value)) 5) )
                                 (send msg2 set-value total) ))
                     ))
;Hacer un panel para agregar hay botones.
(define panel (new horizontal-panel% (parent frame)))

;Función para crear el boton de play.
(define aa (new button% (parent panel) (label "Play") (font f)
                (stretchable-width 3)
                (stretchable-height 2)
                (callback (lambda (button event)
                            (set! numero_filas (send slider1 get-value))
                            (set! numero_columnas (send slider2 get-value))
                            (set! board (TTT numero_columnas numero_filas))
                            (c_t numero_filas numero_columnas )))
                )
  )


(define panel2 (new horizontal-panel% (parent frame)))

(define msg2 (new gauge%  (parent panel2) (label "Dificultad : ")(range 15) ))
(send msg2 set-value 1)


(define (c_t filas col)
  (define frame2 (new frame% (label  (~a(~a "" filas) "x" col ) ) ))
  ;(define msg (new message% (parent frame2)(label "__Tu turno__")))
  (printf "> Matriz ~ax~a\n" filas col)
  (crear_tablero filas col frame2 )
  )

(define (crear_tablero f c frame2)
  (cond    
    ((equal? f 0) (send frame2 show #t))
    (else
     (crear_filas f c frame2)
     (crear_tablero (- f 1) c frame2 )    
    )
))
(define(crear_filas num_fil num_col frame2)
  (cond
    ((equal? num_fil 0) 0)
    (else
     (define yesFilas (new horizontal-panel% (parent frame2)))
     (crear_columnas num_fil num_col yesFilas)
  )))

(define(crear_columnas num_fil num_col panel)
  (cond
    ((equal? num_col 0) 0)
    (else
     (define yes (new button% (parent panel)
             (label "  ")
             (vert-margin  0)
             (horiz-margin 0)
             (font f)
             (callback (lambda (button event)                         
                         (printf "Fila ~a \n"(/ (send panel get-y) 22))
                         (printf "Columna ~a \n" (/ (send yes get-x) 66))
                         (set! board (change-symbol board (/ (send panel get-y) 22) (/ (send yes get-x) 66) 1))
                         (send yes set-label "X")
                         (set! machine_play (run-main board))
                         (set! board (cdr machine_play))
                         (display board)
                         ))))
       (crear_columnas num_fil (- num_col 1) panel)
       )))

(send frame show #t)