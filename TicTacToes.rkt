;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TicTacToes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;NOMENGLATURA
;0 -> Casilla vacía.
;1 -> Casilla con X.
;2 -> Casilla con O.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;GREEDY ALGORITHM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Calcula los candidatos para el siguiente movimiento.
(define (candidates-func board)
  (cond ((null? board) '())
        (else (get-candidate-cells (get-circles-position board 0))))) ;TODO: cuando no haya ningun circulo elegir una casilla aleatoria.

;Evalua los candidatos y verifica que sean movimientos válidos.
(define (feasibility-func candidates board)
  (cond ((null? candidates) '())
        (else (cond ((and (exists? (caar candidates) (car (cdr (car candidates))) (get-width board) (get-height board))
                          (equal? (get-symbol (caar candidates) (car(cdr(car candidates))) board) 0)) (cons (car candidates) (feasibility-func (cdr candidates) board)))
                    (else (feasibility-func (cdr candidates) board) ) ))))

;Asigna una calificación a cada posición candidata. ;TODO: Agregar calificacion tambien con numero de unos, significa que bloqueará al jugador.
(define (objective-func newcandidates board)
  (cond ((null? newcandidates) '())
        (else (cons (cons (+ (count-element-row (get-row (caar newcandidates) board) 2) (count-element-col (car(cdr(car newcandidates))) 2 board)) (car newcandidates) ) (objective-func (cdr newcandidates) board))))) 

;Función de selección: elige a uno de los mejores candidatos para servir como solución parcial.
(define (selection-func ratedcandidates)
  (list-ref (remove-duplicates (selection-aux ratedcandidates)) (random (round(/ (listlen(remove-duplicates (selection-aux ratedcandidates))) 2)))))


;Obtiene los mejores candidatos de la selección.
(define (selection-aux ratedcandidates)
  (cond ((null? ratedcandidates) '())
        (else (cond ((equal? (caar ratedcandidates) (get-highest-rate ratedcandidates 0)) (cons (cdr(car ratedcandidates)) (selection-aux (cdr ratedcandidates))))
                    (else (selection-aux (cdr ratedcandidates)))))))

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
          
                     
;Obtiene todos las posiciones donde hay círculos.
(define (get-circles-position board ys)
  (cond ((null? board) '())
        (else (cons (cons ys (get-circles-aux (car board) 0)) (get-circles-position (cdr board) (+ ys 1))))))
              
;Función auxiliar que revisa si hay círculos en cada fila.                   
(define (get-circles-aux lst xs)
  (cond ((null? lst) '())
        (else (cond ((equal? (car lst) 2) (cons xs (get-circles-aux (cdr lst) (+ xs 1))))
                    (else (get-circles-aux (cdr lst) (+ xs 1)))))))

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