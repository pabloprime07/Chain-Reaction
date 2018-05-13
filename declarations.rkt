#lang racket/gui
;|||contains 'my-button%' 'cell' 'board' '2d-vec' 'make-board'

(provide (all-defined-out))
(struct cell (numberofballs color criticalmass rco cco) #:transparent)
(struct board (brd row cols) #:transparent)

(define (findc b r c)
  (cond [(null? b) '()]
        [(and (equal? (cell-rco (car b)) r) (equal? (cell-cco (car b)) c)) (car b)]
        [else (findc (cdr b) r c)]))
;image% c
(define image%
  (class object%
    (super-new)
    (init-field picture)
    (init-field noofballs)
    (init-field color)
    (define/public (get-c) color)
    (define/public (get-n) noofballs)))
(define blank (make-object image% (make-object bitmap% "blank.png" 'png #f #f 1.2) 0 "blank"))
;list of images
(define list-of-images
  (list
   (make-object image% (make-object bitmap% "1blue.png" 'png #f #f 1.2) 1 "blue")
   (make-object image% (make-object bitmap% "2blue.png" 'png #f #f 1.2) 2 "blue")
   (make-object image% (make-object bitmap% "3blue.png" 'png #f #f 1.2) 3 "blue")
   (make-object image% (make-object bitmap% "1red.png" 'png #f #f 1.2) 1 "red")
   (make-object image% (make-object bitmap% "2red.png" 'png #f #f 1.2) 2 "red")
   (make-object image% (make-object bitmap% "3red.png" 'png #f #f 1.2) 3 "red")
   (make-object image% (make-object bitmap% "1green.png" 'png #f #f 1.2) 1 "green")
   (make-object image% (make-object bitmap% "2green.png" 'png #f #f 1.2) 2 "green")
   (make-object image% (make-object bitmap% "3green.png" 'png #f #f 1.2) 3 "green")
   (make-object image% (make-object bitmap% "1grey.png" 'png #f #f 1.2) 1 "grey")
   (make-object image% (make-object bitmap% "2grey.png" 'png #f #f 1.2) 2 "grey")
   (make-object image% (make-object bitmap% "3grey.png" 'png #f #f 1.2) 3 "grey")
   (make-object image% (make-object bitmap% "1purple.png" 'png #f #f 1.2) 1 "purple")
   (make-object image% (make-object bitmap% "2purple.png" 'png #f #f 1.2) 2 "purple")
   (make-object image% (make-object bitmap% "3purple.png" 'png #f #f 1.2) 3 "purple")
   (make-object image% (make-object bitmap% "1yellow.png" 'png #f #f 1.2) 1 "yellow")
   (make-object image% (make-object bitmap% "2yellow.png" 'png #f #f 1.2) 2 "yellow")
   (make-object image% (make-object bitmap% "3yellow.png" 'png #f #f 1.2) 3 "yellow")
   (make-object image% (make-object bitmap% "blank.png" 'png #f #f 1.2) 0 "blank") ))
;improved button% class
;my-button% has functions such as 'get-r','get-c','get-clr','set-cell!','get-balls'
(define my-button%
  (class button%
    (super-new)
    (init-field cell)
    (define (r) (cell-rco cell))
    (define (c) (cell-cco cell))
    (define (clr) (cell-color cell))
    (define/public (set-cell! cel) (set! cell cel))
    (define/public (get-balls) (cell-numberofballs cell))
    (public [r get-r] [c get-c] [clr get-clr])))
;2d vector
(define (make-vec r c initial)
  (build-vector r (lambda (x) (make-vector c initial))))
(define (vec-ref vec r c)
  (vector-ref (vector-ref vec r) c))
(define (vec-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val))))
;to make board
(define (make-board row colums)
  (define vec (make-vec row colums #f))
  (define (make-board-h c)
    (cond [(= c (* row colums)) '()]
          [(or (= c 0) (= c (- colums 1)) (= c (* (- row 1) colums)) (= c (- (* row colums) 1)))
           (begin (vec-set! vec (quotient c colums) (remainder c colums) (cell 0 "#" 2 (quotient c colums) (remainder c colums))) (make-board-h (+ c 1)))]
          [(or (= (remainder c colums) 0) (= (remainder (+ c 1) colums) 0)) 
           (begin (vec-set! vec (quotient c colums) (remainder c colums) (cell 0 "#" 3 (quotient c colums) (remainder c colums))) (make-board-h (+ c 1)))]
          [(or (and (> c 0) (< c (- colums 1))) (and (> c (* colums (- row 1))) (< c (- (* row colums) 1))))
           (begin (vec-set! vec (quotient c colums) (remainder c colums) (cell 0 "#" 3 (quotient c colums) (remainder c colums))) (make-board-h (+ c 1)))]
          [else (begin (vec-set! vec (quotient c colums) (remainder c colums) (cell 0 "#" 4 (quotient c colums) (remainder c colums))) (make-board-h (+ c 1)))]))
  (begin (make-board-h 0) vec))