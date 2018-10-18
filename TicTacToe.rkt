#lang racket/gui


;NOMENGLATURA
;0 -> Casilla vacía.
;1 -> Casilla con X.
;2 -> Casilla con O.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;GREEDY ALGORITHM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Función principal donde se verifica ganador y si no hay, ejecuta el algoritmo.
(define (run-main board)
  (cond ((equal? (solution-func board) 2) (list  "Machine"))
        ((equal? (solution-func board) 1) (list  "Player"))
        ((no-winner? board) (list "No one"))
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
  (cond ((equal? (listlen (remove-duplicates (selection-aux ratedcandidates))) 1) (car (remove-duplicates (selection-aux ratedcandidates))))
        ((equal? (listlen (remove-duplicates (selection-aux ratedcandidates))) 0) '(-1 -1))
        (else 
         (list-ref (remove-duplicates (selection-aux ratedcandidates)) (random (+ (round(/ (listlen(remove-duplicates (selection-aux ratedcandidates))) 2)) 1)) )))) 
  

;Obtiene los mejores candidatos de la selección.
(define (selection-aux ratedcandidates)
  (cond ((null? ratedcandidates) '())
        (else (cond ((equal? (caar ratedcandidates) (get-highest-rate ratedcandidates 0)) (cons (cdr(car ratedcandidates)) (selection-aux (cdr ratedcandidates))))
                    (else (selection-aux (cdr ratedcandidates)))))))

;Determina si se ha hallado una solución.
(define (solution-func board)
  (cond ((or (winner-row? 1 board)
             (winner-col? 1 0 0 board board)) 1)
        ((or (winner-row? 2 board)
             (winner-col? 2 0 0 board board)) 2)
        (else 0)))

;Determina si se ha ganado al llenar una fila cualquiera.
(define (winner-row? symbol board)
  (cond ((null? board) #f)
        (else (cond ((equal? (count-element-row (car board) symbol) (get-width board)) #t)
                    (else (winner-row? symbol (cdr board)))))))

;Determina si se ha ganado al llenar una columna cualquiera.
(define (winner-col? symbol acc indx board boardaux)
  (cond ((equal? indx (get-width board)) #f)
        ((equal? acc (get-height board)) #t)
        ((null? boardaux) (winner-col? symbol 0 (+ indx 1) board board))
        (else (cond ((equal? symbol (list-ref (car boardaux) indx)) (winner-col? symbol (+ acc 1) indx board (cdr boardaux)))
                    (else (winner-col? symbol acc indx board (cdr boardaux)))))))

;Determina si no hubo un ganador en la partida.
(define (no-winner? board)
  (cond ((zero? (count-element 0 board)) #t)
        (else #f)))

                                  
;Obtiene la mayor calificación de los candidatos.
(define (get-highest-rate ratedcandidates highest)
  (cond ((null? ratedcandidates) highest)
        (else (cond ((> (caar ratedcandidates) highest) (get-highest-rate (cdr ratedcandidates) (caar ratedcandidates)))
                    (else (get-highest-rate (cdr ratedcandidates) highest)))))) 

;Cuenta la cantidad de aparaciones de un elemento en la matriz.
(define (count-element symbol board)
  (cond ((null? board) 0)
        (else (+ (count-element-row (car board) symbol) (count-element symbol (cdr board))))))

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
(define difficulty -6)
(define row_number 0)
(define col_number 0)
(define board '())
(define machine_play '())

;Contruye un marco/ventana (Ventana inicial )
(define main_window (new frame% (label "TicTacToe") (width 300) (height 100)))

;Fuente utilizada en los botones.
(define f (new font%) )

;Hacer un mensaje de texto estático en el marco.
(define welcome_message (new message% (parent main_window)(label "Bienvenido a TicTacToe")(font f)  ))

;Hacer un panel para agregar hay botones.
(define button_panel (new horizontal-panel% (parent main_window)))

;Hacer un slider para seleccionar la cantidad de filas en el tablero
(define slider1(new slider% (parent button_panel)
                    (label "Filas: ") (min-value 3)  (max-value 10) (font f) 
                    (callback (lambda (slider event)
                                (set! difficulty (- (+ (send slider1 get-value) (send slider2 get-value)) 5) )
                                (send difficulty_gauge set-value difficulty)))
                    ))

;Hacer un slider para seleccionar la cantidad de columnas en el tablero
(define slider2 (new slider% (parent button_panel)
                     (label "Columnas: ") (min-value 3) (max-value 10) (font f)
                     (callback (lambda (slider event)
                                 (set! difficulty (- (+ (send slider1 get-value) (send slider2 get-value)) 5) )
                                 (send difficulty_gauge set-value difficulty) ))
                     ))
;Hacer un panel para agregar hay botones.
(define panel_play (new horizontal-panel% (parent main_window)))

;Función para crear el boton de play.
(define play_button (new button% (parent panel_play) (label "Play") (font f)
                (stretchable-width 3)
                (stretchable-height 2)
                (callback (lambda (button event)
                            (set! row_number (send slider1 get-value))
                            (set! col_number (send slider2 get-value))
                            (set! board (TTT col_number row_number))
                            (build-board-ui row_number col_number)
                            (send main_window show #f)))
                
                )
  )


(define difficulty_panel (new horizontal-panel% (parent main_window)))

(define difficulty_gauge (new gauge%  (parent difficulty_panel) (label "Dificultad : ")(range 15) ))
(send difficulty_gauge set-value 1)


(define game_window (new frame% (label  "TicTacToe: The Game") (stretchable-width #f) (stretchable-height #f)))

;Función principal de contrución de tablero.
(define (build-board-ui filas col)
  ;(define msg (new message% (parent frame2)(label "__Tu turno__")))
  ;(printf "> Matriz ~ax~a\n" filas col)
  (build-ui-aux 0 0 game_window )
  )

;Función auxiliar que construye el tablero.
(define (build-ui-aux f c frame2)
  (cond    
    ((equal? f row_number) (send frame2 show #t))
    (else
     (create_rows f c frame2)
     (build-ui-aux (+ f 1) c frame2 )    
    )
))

;Crea las filas del tablero.
(define (create_rows row col frame2)
     (define rows (new horizontal-panel% (parent frame2) (vert-margin  0) (horiz-margin 0) ))
     (create_cols row col rows frame2)
  )

;Crea las columnas del tablero.
(define(create_cols row col panel frame2)
  (define pos_x col)
  (define pos_y row)
  (cond
    ((equal? col col_number) 0)
    (else
     (define cell (new button% (parent panel)
             (label (get-pos-label row col))
             (vert-margin  0)
             (horiz-margin 0)
             (stretchable-height #f)
             (stretchable-width #f)
             (font f)
             
             (callback (lambda (button event)                                                
                         (cond ((is-valid-move? pos_y pos_x)
                                (send cell set-label "X")
                                (set! board (change-symbol board  pos_y pos_x 1))                              
                                (set! machine_play (run-main board))
                                (set! board (cdr machine_play))                             
                                (update-board frame2)
                                (cond ((equal? "Player" (car machine_play)) (show-msg "Player"))
                                      ((equal? 2 (solution-func board)) (show-msg "Machine"))
                                      ((no-winner? board) (show-msg "No One")))
                                )) 
                         ))))
       (create_cols row (+ col 1) panel frame2)
       )))


;Verifica que el jugador este haciendo una jugada válida.
(define (is-valid-move? row col)
  (cond ((or (equal? (get-symbol row col board) 1)
             (equal? (get-symbol row col board) 2)) #f)
        (else #t)))

;Obtiene el symbolo de una posición de acuerdo a la matriz.
(define (get-pos-label row col)
  (cond ((equal? (get-symbol row col board) 0) " ")
        ((equal? (get-symbol row col board) 1) "X")
        ((equal? (get-symbol row col board) 2) "O")
        (else "GAME")))

;Actualiza el tablero cuando se hace una jugada.
(define (update-board frame2)
  (clear-board frame2 (send frame2 get-children))
  (build-ui-aux 0 0 frame2))

;Limpia el tablero.
(define (clear-board frame2 children)
  (cond ((null? children) 0)
        (else (send frame2 delete-child (car children))
              (clear-board frame2 (cdr children)))))

;Imprime la matriz.
(define (print-matrix matr)
  (cond ((null? matr) "")
        (else (displayln (car matr))
              (print-matrix (cdr matr)))))

;Muestra un mensaje cuando el juego se ha acabado.
(define (show-msg winner)
  ;(send frame2 show #f)
  (define pop-dialog (new dialog% (label "TicTacToe")))
  (define end-panel (new horizontal-panel% (parent pop-dialog)))
  (define rest-button (new button%
     (label "Restart")
     (parent end-panel)
     (callback
      (lambda (b e)
        (send pop-dialog show #f)
        (send game_window show #f)
        (set! board '())
        (set! machine_play '())
        (clear-board game_window (send game_window get-children))
        (set! board (TTT col_number row_number))
        (build-board-ui row_number col_number)))))

  (define go-main-button (new button%
     (label "Go Main")
     (parent end-panel)
     (callback
      (lambda (b e)
        (set! difficulty -6)
        (set! row_number 0)
        (set! col_number 0)
        (set! board '())
        (set! machine_play '())
        (clear-board game_window (send game_window get-children))
        (send pop-dialog show #f)
        (send main_window show #t)))))

  (define exit-button (new button%
     (label "Exit")
     (parent end-panel)
     (callback
      (lambda (b e)
        (exit)))))
  (define msg (new message% (parent pop-dialog)(label (string-append winner " Wins!!"))))
  (send pop-dialog show #t))

(send main_window show #t)
