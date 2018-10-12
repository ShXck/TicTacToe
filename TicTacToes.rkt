;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TicTacToes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
  (change-symbol board (car coords) (cadr coords) 2))
  
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
  (list-ref (remove-duplicates (selection-aux ratedcandidates)) (random (round(/ (listlen(remove-duplicates (selection-aux ratedcandidates))) 2))))) 


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

(define (listlen lst)
  (cond ((null? lst) 0)
        (else (+ 1 (listlen (cdr lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;