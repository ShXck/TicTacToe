;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TicTacToes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;NOMENGLATURA
;0 -> Casilla vacía.
;1 -> Casilla con X.
;2 -> Casilla con O.


;Función principal del juego.
(define (TTT width height)
  (cond ((valid? width height) (build-board width height))
        (else (display "INVALID PARAMETERS"))
  )
)

;Obtiene el simbolo en una posición del tablero.
(define (get-symbol x y board)
  (cond ((null? board) -1)
        (else (list-ref (list-ref board y) x))))

;Cambia el valor de un item dentro del tablero dadas sus coordenadas.
(define (change-symbol board y x newval)
  (cond
    [(empty? board) '()]
    [(= y 0)      (cons (change-item-val (car board) x newval) (cdr board))]
    [else (cons (car board) (change-symbol (cdr board) (- y 1) x newval))]))

;Cambia el valor de un item dentro de una lista.
(define (change-item-val lst index newval)
  (cond
    ((empty? lst) '())
    ((= index 0) (cons newval (cdr lst)))
    [else (cons (car lst) (change-item-val (cdr lst) (- index 1) newval))]))
  
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