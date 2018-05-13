#lang racket/gui
;final version
;hgfhfghgfdhgfhgfdhgfdhgfdhfgdhfgdh

(require "declarations.rkt");2d-vector,make-board and my-button%
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;\NOTE/---button% ke x or y coordinate ke liye get-x hota hai.
(require 2htdp/image)
(provide (all-defined-out))
(define plist (list "red" "green" "blue" "yellow" "purple" "grey"))

;;;;;KING
(define globalboard (board (make-board 6 9) 6 9))
(define whose-turn "red") (define cnt 0) (define z 0) (define prev-turn "#") (define playerlist '())

(define (valid-move? x y clr) ;also changes turn in case move was valid
  (let* [(cell (vec-ref (board-brd globalboard) x y))]
    (if (or (equal? (cell-color cell) clr)
            (equal? (cell-color cell) "#")) (begin (if (= z 1) (checkboard)  (display ""))
                                                   (if (equal? "red" clr) (set! cnt (+ cnt 1)) (display "")) 
                                                   (if (= cnt 2) (set! z 1) (display ""))
                                                   (set! prev-turn whose-turn) (set! whose-turn (next playerlist clr)) #t) #f)))

(define (checkboard)
  (define (helper l)
    (cond [(null? l) (display "")]
          [(checkcolor (car l) 0) (set! playerlist  (remove (car l) playerlist)) (helper (cdr l))]
          [(helper (cdr l))]))
  (helper plist))

(define (checkcolor cl r)
  (define (w-h c)
    (cond [(= c 9) (checkcolor cl (+ r 1))]
          [else (if (or (not (eq? (cell-color (vec-ref (board-brd globalboard) r c)) cl)) (eq? (cell-color (vec-ref (board-brd globalboard) r c)) "#")) (w-h (+ c 1)) #f)]))
  (cond [(= r 6) #t]
        [else (w-h 0)]))

(define (next l clr)
  (cond
    [(null? (cdr l)) "red"]
    [(equal? (car l) clr) (cadr l)]
    [(next (cdr l) clr)]))
(define (make-plist n plist)
  (if (= n 0) '() (cons (car plist) (make-plist (- n 1) (cdr plist)))))
;;;;;;;;; 
(define WIN #f)
;;;;;;;;;;;
(define (playgame row cols)
  (define st (make-board row cols))
  (define moves (cons st '()))
  (define tlist '())
  
  (define (displayin n l);used for creating playerlist
    (cond [(= n 0) (displayln " ")]
          [else (begin (set! tlist (cons (car l) tlist)) (display "player") (display (length tlist)) (displayln (car l)) (displayin (- n 1) (cdr l)))]))
  (set! playerlist tlist)
  
  (define (play r c col b)
    (define count 0)
    (define (play-c r c col b)
      (define cel (vec-ref b r c))
      ;change in next line display removed
      (if (and (> count 0) (win b col 0 row cols)) (vec-set! b 0 0 (cell 1 col 2 0 0))
          (cond [(= (cell-criticalmass cel) 4) (cond [(= (cell-numberofballs cel) (- (cell-criticalmass cel) 1))
                                                      (begin (vec-set! b r c (cell 0 "#" (cell-criticalmass cel) (cell-rco cel) (cell-cco cel))) (set! count 1) (play-c (+ r 1) c col b) (play-c r (+ c 1) col b) (play-c (- r 1) c col b) (play-c r (- c 1) col b))]
                                                     [else (vec-set! b r c (cell (+ (cell-numberofballs cel) 1) col (cell-criticalmass cel) (cell-rco cel) (cell-cco cel)))])]
                [(= (cell-criticalmass cel) 3) (cond [(= (cell-numberofballs cel) (- (cell-criticalmass cel) 1))
                                                      (cond [(= r 0) (begin (vec-set! b r c (cell 0 "#" (cell-criticalmass cel) (cell-rco cel) (cell-cco cel))) (set! count 1) (play-c (+ r 1) c col b) (play-c r (+ c 1) col b) (play-c r (- c 1) col b))]
                                                            [(= r (- row 1)) (begin (vec-set! b r c (cell 0 "#" (cell-criticalmass cel) (cell-rco cel) (cell-cco cel))) (set! count 1) (play-c r (+ c 1) col b) (play-c (- r 1) c col b) (play-c r (- c 1) col b))]
                                                            [(= c 0) (begin (vec-set! b r c (cell 0 "#" (cell-criticalmass cel) (cell-rco cel) (cell-cco cel))) (set! count 1) (play-c r (+ c 1) col b) (play-c (- r 1) c col b) (play-c (+ r 1) c col b))]
                                                            [(= c (- cols 1)) (begin (vec-set! b r c (cell 0 "#" (cell-criticalmass cel) (cell-rco cel) (cell-cco cel))) (set! count 1) (play-c r (- c 1) col b) (play-c (- r 1) c col b) (play-c (+ r 1) c col b))])]
                                                     [else (vec-set! b r c (cell (+ (cell-numberofballs cel) 1) col (cell-criticalmass cel) (cell-rco cel) (cell-cco cel)))])]
                [(= (cell-criticalmass cel) 2) (cond [(= (cell-numberofballs cel) (- (cell-criticalmass cel) 1))
                                                      (cond [(and (= r 0) (= c 0)) (begin (vec-set! b r c (cell 0 "#" (cell-criticalmass cel) (cell-rco cel) (cell-cco cel))) (set! count 1) (play-c (+ r 1) c col b) (play-c r (+ c 1) col b))]
                                                            [(and (= r 0) (= c (- cols 1))) (begin (vec-set! b r c (cell 0 "#" (cell-criticalmass cel) (cell-rco cel) (cell-cco cel))) (set! count 1) (play-c (+ r 1) c col b) (play-c r (- c 1) col b))]
                                                            [(and (= r (- row 1)) (= c 0)) (begin (vec-set! b r c (cell 0 "#" (cell-criticalmass cel) (cell-rco cel) (cell-cco cel))) (set! count 1) (play-c r (+ c 1) col b) (play-c (- r 1) c col b))]
                                                            [(and (= r (- row 1)) (= c (- cols 1))) (begin (vec-set! b r c (cell 0 "#" (cell-criticalmass cel) (cell-rco cel) (cell-cco cel))) (set! count 1) (play-c r (- c 1) col b) (play-c (- r 1) c col b))])]
                                                     [else (vec-set! b r c (cell (+ (cell-numberofballs cel) 1) col (cell-criticalmass cel) (cell-rco cel) (cell-cco cel)))])])))
    (begin (play-c r c col b) (cond [(and (> count 0) (win b col 0 row cols)) (set! WIN #t)])))
  (define (pcplay Gboard cpucol playercol count)
    (define coords '())
    (define (cunt b cl ro co)
      (define ct 0)
      (define (c-h r)
        (define (w-h c)
          (cond [(= c co) (c-h (+ r 1))]
                [else (if (eq? (cell-color (vec-ref b r c)) cl) (begin (set! ct (+ ct (cell-numberofballs (vec-ref b r c)))) (w-h (+ c 1))) (w-h (+ c 1)))]))
        (cond [(= r ro) ct]
              [else (w-h 0)]))
      (c-h 0))
    (define (consen b ro co col)
      (define l '())
      (define (c-h r)
        (define (w-h c)
          (cond [(= c co) (c-h (+ r 1))]
                [else (let* ((cel (vec-ref b r c)))
                        (if (equal? col (cell-color cel)) 
                            (cond [(= (cell-criticalmass cel) 4) (begin (set! l (append l (list (cons cel (let* ((adcel (list (vec-ref b (+ r 1) c) (vec-ref b r (+ c 1)) (vec-ref b (- r 1) c) (vec-ref b r (- c 1)))))
                                                                                                            (append* (map (lambda (x) (if (or (equal? (cell-color cel) (cell-color x))
                                                                                                                                              (equal? (cell-color x) "#")) '()
                                                                                                                                                                           (list x))) adcel))))))) (w-h (+ c 1)))]
                                  [(= (cell-criticalmass cel) 3) (begin (set! l (append l (list (cons cel
                                                                                                      (let* ((adcel (cond [(= r 0) (list (vec-ref b (+ r 1) c) (vec-ref b r (+ c 1)) (vec-ref b r (- c 1)))]
                                                                                                                          [(= r (- ro 1)) (list (vec-ref b r (+ c 1)) (vec-ref b r (- c 1)) (vec-ref b (- r 1) c))]
                                                                                                                          [(= c 0) (list (vec-ref b (+ r 1) c) (vec-ref b r (+ c 1)) (vec-ref b (- r 1) c))]
                                                                                                                          [(= c (- co 1)) (list (vec-ref b (+ r 1) c) (vec-ref b (- r 1) c) (vec-ref b r (- c 1)))])))
                                                                                                        (append* (map (lambda (x) (if (or (equal? (cell-color cel) (cell-color x))
                                                                                                                                          (equal? (cell-color x) "#")) '()
                                                                                                                                                                       (list x))) adcel))))))) (w-h (+ c 1)))]
                                  [(= (cell-criticalmass cel) 2) (begin (set! l (append l (list (cons cel
                                                                                                      (let* ((adcel (cond [(and (= c 0) (= r 0)) (list (vec-ref b (+ r 1) c) (vec-ref b r (+ c 1)))]
                                                                                                                          [(and (= c 0) (= r (- ro 1))) (list (vec-ref b r (+ c 1)) (vec-ref b (- r 1) c))]
                                                                                                                          [(and (= c (- co 1)) (= r (- ro 1))) (list (vec-ref b r (- c 1)) (vec-ref b (- r 1) c))]
                                                                                                                          [(and (= c (- co 1)) (= r 0)) (list (vec-ref b (+ r 1) c) (vec-ref b r (- c 1)))])))
                                                                                                        (append* (map (lambda (x) (if (or (equal? (cell-color cel) (cell-color x))
                                                                                                                                          (equal? (cell-color x) "#")) '()
                                                                                                                                                                       (list x))) adcel))))))) (w-h (+ c 1)))])
                            (w-h (+ c 1))))]))
        (cond [(= r ro) l]
              [else (w-h 0)]))
      (c-h 0))
  
    (define (ch l)
      (cond [(null? l) '()]
            [else (append (list (map (lambda (x) (list (- (cell-criticalmass x) (cell-numberofballs x)) (cell-rco x) (cell-cco x))) (car l)))
                          (ch (cdr l)))]))
    (define (ch2 l)
      (cond [(null? l) '()]
            [else (append (if (null? (append* (map (lambda (x) (if (>= (car x) (caaar l)) '() (list x))) (cdar l)))) (list (caar l)) '()) (ch2 (cdr l)))])) 

    (define (f-1 l) 
      (cond [(null? l) '()]
            [else (append (if (equal? (caar l) 1) (list (car l)) '()) (f-1 (cdr l)))]))
    (define (givsts brd l)    
      (define (sttr x)
        (define (c-h r)
          (define (w-h c)
            (cond [(= c (board-cols Gboard)) (c-h (+ r 1))]
                  [else (begin (vec-set! x r c (vec-ref brd r c)) (w-h (+ c 1)))]))
          (cond [(= r (board-row Gboard)) x]
                [else (w-h 0)]))
        (c-h 0))
      (define x (sttr (make-board (board-row Gboard) (board-cols Gboard))))
      (define (h c)
        (cond [(null? c) '()]
              [else (append (list (begin (play (cadar c) (caddar c) cpucol x) (list x (cadar c) (caddar c)))) (givsts brd (cdr c)))]))
      (h l))

    (define (2pla l s)
      (cond [(null? l) s]
            [else (let* ((b0  (caar l))
                         (b0b (cunt b0 cpucol (board-row Gboard) (board-cols Gboard))))                  
                    (if (win b0 cpucol 0 (board-row Gboard) (board-cols Gboard))
                        (list (list b0 (cadar l) (caddar l) b0b))
                        (let* ((b1 (pcplay (board b0 (board-row Gboard) (board-cols Gboard)) playercol cpucol 1)))
                          (if (win b1 playercol 0 (board-row Gboard) (board-cols Gboard))
                              (2pla (cdr l) s)
                              (2pla (cdr l) (cons (list (pcplay (board b1 (board-row Gboard) (board-cols Gboard)) cpucol playercol 1) (cadar l) (caddar l) b0b) s))))))]))
    (define (mx l)
      (define x (car l))
      (define (h c)
        (cond [(null? c) (cdr x)]
              [else (if (> (car x) (caar c)) (h (cdr c)) (begin (set! x (car c)) (h (cdr c))))]))
      (h l))
    (define (mx2 l)
      (define x (car l))
      (define (h c)
        (cond [(null? c) (if (>= (car x) (cadddr x)) (cdr x) '())]
              [else (if (> (car x) (caar c)) (h (cdr c)) (begin (set! x (car c)) (h (cdr c))))]))
      (h l))
    (define (mn l)
      (define x (car l))
      (define (h c)
        (cond [(null? c) (cdr x)]
              [else (if (< (car x) (caar c)) (h (cdr c)) (begin (set! x (car c)) (h (cdr c))))]))   (h l))

    (define (edgf b r c)
      (define a (list (vec-ref b 0 0) (vec-ref b (- r 1) 0) (vec-ref b (- r 1) (- c 1)) (vec-ref b 0 (- c 1))))
      (append* (map (lambda (x) (let* ((ca (- (cell-criticalmass x) (cell-numberofballs x)))
                                       (ro (cell-rco x))
                                       (co (cell-cco x)))                                  
                                  (cond [(and (equal? 0 (cell-rco x)) (equal? (cell-cco x) 0))
                                         (if (and (equal? (cell-color x) "#") (if (equal? (cell-color x) cpucol) #t (>= (- (cell-criticalmass (vec-ref b (+ ro 1) co)) (cell-numberofballs (vec-ref b (+ ro 1) co))) ca))
                                                  (if (equal? (cell-color x) cpucol) #t (>= (- (cell-criticalmass (vec-ref b ro (+ 1 co))) (cell-numberofballs (vec-ref b ro (+ co 1)))) ca))) (list x) '())]
                                        [(and (equal? 0 (cell-rco x)) (equal? (cell-cco x) (- c 1)))
                                         (if (and (equal? (cell-color x) "#") (if (equal? (cell-color x) cpucol) #t (>= (- (cell-criticalmass (vec-ref b (+ ro 1) co)) (cell-numberofballs (vec-ref b (+ ro 1) co))) ca))
                                                  (if (equal? (cell-color x) cpucol) #t (>= (- (cell-criticalmass (vec-ref b ro (- co 1))) (cell-numberofballs (vec-ref b ro (- co 1)))) ca))) (list x) '())]
                                        [(and (equal? (- r 1) (cell-rco x)) (equal? (cell-cco x) 0))
                                         (if (and (equal? (cell-color x) "#") (if (equal? (cell-color x) cpucol) #t (>= (- (cell-criticalmass (vec-ref b (- ro 1) co)) (cell-numberofballs (vec-ref b (- ro 1) co))) ca))
                                                  (if (equal? (cell-color x) cpucol) #t (>= (- (cell-criticalmass (vec-ref b ro (+ co 1))) (cell-numberofballs (vec-ref b ro (+ co 1)))) ca))) (list x) '())]
                                        [(and (equal? (- r 1) (cell-rco x)) (equal? (cell-cco x) (- c 1)))
                                         (if (and (equal? (cell-color x) "#") (if (equal? (cell-color x) cpucol) #t (>= (- (cell-criticalmass (vec-ref b (- ro 1) co)) (cell-numberofballs (vec-ref b (- ro 1) co))) ca))
                                                  (if (equal? (cell-color x) cpucol) #t (>= (- (cell-criticalmass (vec-ref b ro (- co 1))) (cell-numberofballs (vec-ref b ro (- co 1)))) ca))) (list x) '())]))) a)))
    (define (remov l)
      (cond [(null? l) '()]
            [(equal? (length (car l)) 1) (remov (cdr l))]
            [else (cons (car l) (remov (cdr l)))]))
    (define (vertl b ro co)
      (define (c-h r)    
        (define (w-h c)                     
          (cond [(= c co) (c-h (+ r 1))]
                [else (let* ((cel (vec-ref b r c))
                             (ct (- (cell-criticalmass cel) (cell-numberofballs cel))))
                        (cond [(and (= r 0) (equal? (cell-criticalmass cel) 3)) (if (and (equal? (cell-color cel) "#") (equal? (cell-color (vec-ref b r (+ c 1))) "#")
                                                                                         (equal? (cell-color (vec-ref b r (- c 1))) "#")
                                                                                         (if (equal? (cell-color (vec-ref b (+ r 1) c)) playercol) (>= (- (cell-criticalmass (vec-ref b (+ r 1) c)) (cell-numberofballs (vec-ref b (+ r 1) c))) ct)
                                                                                             (if (equal? (cell-color (vec-ref b (+ r 1) c)) "#") #t #f)))
                                                                                    (cons (list r c) (w-h (+ c 1))) (w-h (+ c 1)))]
                              [(and (= r (- ro 1)) (equal? (cell-criticalmass cel) 3)) (if (and (equal? (cell-color cel) "#") (equal? (cell-color (vec-ref b r (+ c 1))) "#")
                                                                                                (equal? (cell-color (vec-ref b r (- c 1))) "#")
                                                                                                (if (equal? (cell-color (vec-ref b (- r 1) c)) playercol) (>= (- (cell-criticalmass (vec-ref b (- r 1) c)) (cell-numberofballs (vec-ref b (- r 1) c))) ct)
                                                                                                    (if (equal? (cell-color (vec-ref b (- r 1) c)) "#") #t #f)))
                                                                                           (cons (list r c) (w-h (+ c 1))) (w-h (+ c 1)))]
                              [(and (= c 0) (equal? (cell-criticalmass cel) 3)) (if (and (equal? (cell-color cel) "#")
                                                                                         (if (equal? (cell-color (vec-ref b r (+ c 1))) playercol) (>= (- (cell-criticalmass (vec-ref b r (+ c 1))) (cell-numberofballs (vec-ref b r (+ c 1)))) ct)
                                                                                             (if (equal? (cell-color (vec-ref b r (+ c 1))) "#") #t #f))
                                                                                         (equal? (cell-color (vec-ref b (+ r 1) c)) "#") (equal? (cell-color (vec-ref b (- r 1) c)) "#"))
                                                                                    (cons (list r c) (w-h (+ c 1))) (w-h (+ c 1)))]
                              [(and (= c (- co 1)) (equal? (cell-criticalmass cel) 3)) (if (and (equal? (cell-color cel) "#")
                                                                                                (if (equal? (cell-color (vec-ref b r (- c 1))) playercol) (>= (- (cell-criticalmass (vec-ref b r (- c 1))) (cell-numberofballs (vec-ref b r (- c 1)))) ct)
                                                                                                    (if (equal? (cell-color (vec-ref b r (- c 1))) "#") #t #f))
                                                                                                (equal? (cell-color (vec-ref b (+ r 1) c)) "#") (equal? (cell-color (vec-ref b (- r 1) c)) "#"))
                                                                                           (cons (list r c) (w-h (+ c 1))) (w-h (+ c 1)))]
                              [else (w-h (+ c 1))]))]))
        (cond [(= r ro) '()]
              [else (w-h 0)]))
      (c-h 0))

    (define (insdl b ro co)
      (define (c-h r)               
        (define (w-h c)
          (cond [(= c co) (c-h (+ r 1))]
                [else (let* ((cel (vec-ref b r c)))
                        (cond [(equal? (cell-criticalmass cel) 4) (if (and (equal? (cell-color cel) "#") (equal? (cell-color (vec-ref b r (+ c 1))) "#")
                                                                           (equal? (cell-color (vec-ref b r (- c 1))) "#") (equal? (cell-color (vec-ref b (+ r 1) c)) "#")
                                                                           (equal? (cell-color (vec-ref b (- r 1) c)) "#"))
                                                                      (cons (list r c) (w-h (+ c 1))) (w-h (+ c 1)))]
                              [else (w-h (+ c 1))]))]))
        (cond [(= r ro) '()]
              [else (w-h 0)]))
      (c-h 0))

    (define (findcl b cl rows cols)
      (define (c-h r)
        (define (w-h c)
          (cond [(= c cols) (c-h (+ r 1))]
                [(equal? (cell-color (vec-ref b r c)) cl) (cons (vec-ref b r c) (w-h (+ c 1)))]
                [else (w-h (+ c 1))]))
        (cond [(= r rows) '()]
              [else (w-h 0)]))
      (c-h 0))
    (define (filtcl li b ro co)
      (define (filt l)
        (cond [(null? l) '()]
              [else (let* ((cel (car l))
                           (r (cell-rco cel))
                           (c (cell-cco cel)))
                      (cond [(= (cell-criticalmass cel) 4) (if (and (not (equal? (cell-color (vec-ref b (+ r 1) c)) playercol)) (not (equal? (cell-color (vec-ref b r (- c 1))) playercol))
                                                                    (not (equal? (cell-color (vec-ref b (- r 1) c)) playercol)) (not (equal? (cell-color (vec-ref b r (+ c 1))) playercol)))
                                                               (cons cel (filt (cdr l))) (filt (cdr l)))]
                            [(= (cell-criticalmass cel) 3) (cond [(= r 0) (if (and (not (equal? (cell-color (vec-ref b (+ r 1) c)) playercol)) (not (equal? (cell-color (vec-ref b r (- c 1))) playercol))
                                                                                   (not (equal? (cell-color (vec-ref b r (+ c 1))) playercol)))
                                                                              (cons cel (filt (cdr l))) (filt (cdr l)))]
                                                                 [(= r (- ro 1)) (if (and (not (equal? (cell-color (vec-ref b r (- c 1))) playercol))
                                                                                          (not (equal? (cell-color (vec-ref b (- r 1) c)) playercol)) (not (equal? (cell-color (vec-ref b r (+ c 1))) playercol)))
                                                                                     (cons cel (filt (cdr l))) (filt (cdr l)))]
                                                                 [(= c 0) (if (and (not (equal? (cell-color (vec-ref b (+ r 1) c)) playercol)) 
                                                                                   (not (equal? (cell-color (vec-ref b (- r 1) c)) playercol)) (not (equal? (cell-color (vec-ref b r (+ c 1))) playercol)))
                                                                              (cons cel (filt (cdr l))) (filt (cdr l)))]
                                                                 [(= c (- co 1)) (if (and (not (equal? (cell-color (vec-ref b (+ r 1) c)) playercol)) (not (equal? (cell-color (vec-ref b r (- c 1))) playercol))
                                                                                          (not (equal? (cell-color (vec-ref b (- r 1) c)) playercol))) 
                                                                                     (cons cel (filt (cdr l))) (filt (cdr l)))])]
                            [(= (cell-criticalmass cel) 2) (cond [(and (= c 0) (= r 0)) (if (and (not (equal? (cell-color (vec-ref b (+ r 1) c)) playercol)) 
                                                                                                 (not (equal? (cell-color (vec-ref b r (+ c 1))) playercol)))
                                                                                            (cons cel (filt (cdr l))) (filt (cdr l)))]
                                                                 [(and (= c 0) (= r (- ro 1))) (if (and  
                                                                                                    (not (equal? (cell-color (vec-ref b (- r 1) c)) playercol)) (not (equal? (cell-color (vec-ref b r (+ c 1))) playercol)))
                                                                                                   (cons cel (filt (cdr l))) (filt (cdr l)))]
                                                                 [(and (= c (- co 1)) (= r (- ro 1))) (if (and (not (equal? (cell-color (vec-ref b r (- c 1))) playercol))
                                                                                                               (not (equal? (cell-color (vec-ref b (- r 1) c)) playercol)))
                                                                                                          (cons cel (filt (cdr l))) (filt (cdr l)))]
                                                                 [(and (= c (- co 1)) (= r 0)) (if (and (not (equal? (cell-color (vec-ref b (+ r 1) c)) playercol)) (not (equal? (cell-color (vec-ref b r (- c 1))) playercol))
                                                                                                        ) 
                                                                                                   (cons cel (filt (cdr l))) (filt (cdr l)))])]))]))
      (filt li))
    
    (define (ai v)
      (let* ((c1 (remov (consen (board-brd Gboard) (board-row Gboard) (board-cols Gboard) cpucol)))
             (e (edgf (board-brd Gboard) (board-row Gboard) (board-cols Gboard)))
             (f (vertl (board-brd Gboard) (board-row Gboard) (board-cols Gboard)))
             (g (insdl (board-brd Gboard) (board-row Gboard) (board-cols Gboard)))
             (h (filtcl (findcl (board-brd Gboard) "#" (board-row Gboard) (board-cols Gboard)) (board-brd Gboard) (board-row Gboard) (board-cols Gboard)))
             (i (findcl (board-brd Gboard) cpucol (board-row Gboard) (board-cols Gboard)))
             (j (ch (if (null? i) '() (list i))))
             (wst (if (null? j) '() (mx (car j))))
             (bbal (cunt (board-brd Gboard) cpucol (board-row Gboard) (board-cols Gboard)))
         
             (a1 (ch2 (ch c1)))
             (b (f-1 a1))
             (a (remove* b a1)))
        (cond [(and (not (null? a1)) (equal? v 0)) (let* ((bord (board-brd Gboard)))
                                                     (if (= count 1) (if (not (null? b))  (let* ((zx (map (lambda (x) (list (cunt (car x) cpucol (board-row Gboard) (board-cols Gboard)) (cadr x) (caddr x) bbal))
                                                                                                          (givsts bord b)))
                                                                                                 (var (mx2 zx)))
                                                                                            (begin
                                                                                              (displayln zx) (displayln bbal)
                                                                                              (play (car var) (cadr var) cpucol (board-brd Gboard)) (board-brd Gboard)))
                                                                         (let* ((var2 (mx2 (map (lambda (x) (list (cunt (car x) cpucol (board-row Gboard) (board-cols Gboard)) (cadr x) (caddr x) bbal))
                                                                                                (givsts bord a1)))))
                                                                           (begin (play (car var2) (cadr var2) cpucol (board-brd Gboard)) (board-brd Gboard))))
                                                         (if (if (not (null? (2pla (givsts bord b) '())))
                                                                 (not (null? (mx2 (map (lambda (x) (list (cunt (car x) cpucol (board-row Gboard) (board-cols Gboard)) (cadr x) (caddr x) (cadddr x)))
                                                                                       (2pla (givsts bord b) '()))))) #f)
                                                             (let* ((zz (map (lambda (x) (list (cunt (car x) cpucol (board-row Gboard) (board-cols Gboard)) (cadr x) (caddr x) (cadddr x)))
                                                                             (2pla (givsts bord b) '())))
                                                                    (var (mx2 zz)))
                                                               (begin
                                                                 (displayln zz)    
                                                                 (play (car var) (cadr var) cpucol (board-brd Gboard))
                                                                 (set! coords var) (displayln coords)
                                                                 (displayln (printboard (board-brd Gboard) 0 (board-row Gboard) (board-cols Gboard)))))                                                  
                                                             (if (if (not (null? (2pla (givsts bord a) '())))
                                                                     (not (null? (mx2 (map (lambda (x) (list (cunt (car x) cpucol (board-row Gboard) (board-cols Gboard)) (cadr x) (caddr x) (cadddr x)))
                                                                                           (2pla (givsts bord a) '()))))) #f)
                                                                 (let* ((var (mx2 (map (lambda (x) (list (cunt (car x) cpucol (board-row Gboard) (board-cols Gboard)) (cadr x) (caddr x) (cadddr x)))
                                                                                       (2pla (givsts bord a) '())))))
                                                                   (begin (play (car var) (cadr var) cpucol (board-brd Gboard)) (set! coords var) (displayln coords)
                                                                     (displayln (printboard (board-brd Gboard) 0 (board-row Gboard) (board-cols Gboard)))))
                                                                 (ai 1)))))]
                                                   
              [(not (null? e)) (begin (play (cell-rco (car e)) (cell-cco (car e)) cpucol (board-brd Gboard))   
                                      (if (= count 0) (begin (set! coords (list (cell-rco (car e)) (cell-cco (car e)))) (displayln coords) (displayln (printboard (board-brd Gboard) 0 (board-row Gboard) (board-cols Gboard)))) (board-brd Gboard)))]
              [(not (null? f)) (begin (play (caar f) (cadar f) cpucol (board-brd Gboard))
                                      (if (= count 0) (begin (set! coords (list (caar f) (cadar f))) (displayln coords) (displayln (printboard (board-brd Gboard) 0 (board-row Gboard) (board-cols Gboard)))) (board-brd Gboard)))]
              [(not (null? g)) (begin (play (caar g) (cadar g) cpucol (board-brd Gboard))
                                      (if (= count 0) (begin (set! coords (list (caar g) (cadar g))) (displayln coords) (displayln (printboard (board-brd Gboard) 0 (board-row Gboard) (board-cols Gboard)))) (board-brd Gboard)))]
              [(not (null? h)) (begin (play (cell-rco (car h)) (cell-cco (car h)) cpucol (board-brd Gboard)) 
                                      (if (= count 0) (begin (set! coords (list (cell-rco (car h)) (cell-cco (car h)))) (displayln coords) (displayln (printboard (board-brd Gboard) 0 (board-row Gboard) (board-cols Gboard)))) (board-brd Gboard)))]
              [(not (null? wst)) (begin (play (car wst) (cadr wst) cpucol (board-brd Gboard)) 
                                        (if (= count 0) (begin (set! coords (list (car wst) (cadr wst))) (displayln coords) (displayln (printboard (board-brd Gboard) 0 (board-row Gboard) (board-cols Gboard)))) (board-brd Gboard)))])))
    (ai 0))  
                                                                                                                   
  (define count 0)

  (define (update x y coll)
    (define (1-p)
      (define nb (make-board (board-row globalboard) (board-cols globalboard)))
      (define sb (copier nb))
      (if (equal? x 'undo) (begin (set! moves (cdr moves)) (setb (board-brd globalboard) (car moves)) (set! count 0))
          (begin (play x y coll (board-brd globalboard))
                 (if (and (> count 0) (win (board-brd globalboard) coll 0 (board-row globalboard) (board-cols globalboard)))
                     (begin  (sleep 1) (set! count 0) (send BOARD show #f) (send congrat set-label "Congrats!! red won.") (send congrat-frame show #t)) 
                     (begin   
                       (pcplay globalboard "green" coll 0)
                       (set! moves (cons (copier sb) moves))
                       (set! count 1)
                       (if (and (> count 0) (win (board-brd globalboard) "green" 0 (board-row globalboard) (board-cols globalboard)))
                           (begin  (sleep 1) (set! count 0) (send BOARD show #f) (send congrat set-label "Congrats!! cpu won.") (send congrat-frame show #t)) (display "")) )))))     
    (define (u-h)
      (define nb (make-board (board-row globalboard) (board-cols globalboard)))
      (define sb (copier nb))
      (if (equal? x 'undo) (begin (set! moves (cdr moves)) (setb (board-brd globalboard) (car moves)))
          (begin (play x y coll (board-brd globalboard)) (set! moves (cons (copier sb) moves)))))
    (begin  (if (= playerschosen 1) (1-p)
                (u-h))))
  (define (copier x)
    (define (c-h r)
      (define (w-h c)
        (cond [(= c (board-cols globalboard)) (c-h (+ r 1))]
              [else (begin (vec-set! x r c (vec-ref (board-brd globalboard) r c)) (w-h (+ c 1)))]))
      (cond [(= r (board-row globalboard)) x]
            [else (w-h 0)]))
    (c-h 0))

  (define (setb b x)
    (define (c-h r)
      (define (w-h c)
        (cond [(= c (board-cols globalboard)) (c-h (+ r 1))]
              [else (begin (vec-set! b r c (vec-ref x r c)) (w-h (+ c 1)))]))
      (cond [(< r (board-row globalboard)) (w-h 0)]))
    (c-h 0))
  
  (define (win b cl r rows cols)
    (define (w-h c)
      (cond [(= c cols) (win b cl (+ r 1) rows cols)]
            [else (if (or (eq? (cell-color (vec-ref b r c)) cl) (eq? (cell-color (vec-ref b r c)) "#")) (w-h (+ c 1)) #f)]))
    (cond [(= r rows) #t]
          [else (w-h 0)]))
  (define (printboard b r rows cols)
    (define (p-h c)
      (cond [(= c cols) (begin (displayln " ") (printboard b (+ r 1) rows cols))]
            [else (begin (display (text (list->string (list (integer->char (+ 48 (cell-numberofballs (vec-ref b r c)))))) 24
                                        (if (equal? (cell-color (vec-ref b r c)) "#") "black" (cell-color (vec-ref b r c))))) (p-h (+ c 1)))]))
    (cond [(= r rows) " "]
          [else (p-h 0)]))

  (displayin playerschosen plist) 
  update)
     
;--------------------------BOARD---------------------------------------------------------------------------------------------------------
(define (improve-buttons butnlist)
  (define (helper l)
    (cond [(null? (cdr l))
           (let* ([butn (car l)]
                  [r (send butn get-r)]
                  [c (send butn get-c)]
                  [cel (vec-ref (board-brd globalboard) r c)])
             (begin (send butn set-cell! cel) (label-button butn (cell-numberofballs cel) (cell-color cel))))]
          [else (let* ([butn (car l)]
                       [r (send butn get-r)]
                       [c (send butn get-c)]
                       [cel (vec-ref (board-brd globalboard) r c)])
                  (begin (send butn set-cell! cel) (label-button butn (cell-numberofballs cel) (cell-color cel)) (helper (cdr l))))]))
  (helper butnlist))
;for labelling a button
(define (label-button btn nob clr)
  (let* ([imagE (find-picture nob clr)] [pict (get-field picture imagE)]) (send btn set-label pict)))
(define (find-picture n c)
  (define (helper l)
    (cond  [(null? l) blank]          
           [else (let* ([imag (car l)]) (if (and (equal? c (send imag get-c)) (equal? n (send imag get-n))) imag (helper (cdr l))))]))
  (helper list-of-images))

(define playerschosen 0) 
(define startframe (new frame%  
                        [label "CHAIN REACTION"]       [width 500]             [height 300]))

(define msg (new message% [parent startframe]
                 [label "                                                                 "]));it is necessary :-)
;**/*/*/buttons for 'startframe'
(define startplaying (new button%
                          [label "Start Playing"]
                          [parent startframe]
                          [font (make-object font% 30 'roman)]
                          [min-width 400]
                          [min-height 50]
                          [callback (lambda (button event)
                                      (if (> playerschosen 0)
                                          (begin (send startframe show #f) (set! playerlist (make-plist playerschosen plist))
                                                 (send BOARD show #t))
                                          (send msg set-label "Choose number of players!!")))]))
(define playersettings (new button%
                            [label "Number of players"]
                            [parent startframe]
                            [font (make-object font% 30 'roman)]
                            [min-width 400]
                            [min-height 50]
                            [callback (lambda (button event)
                                        (send players show #t))]))

;/*/*/****after clicking on 'Number of players'**///*/**/
(define players (new frame% [label "Select no. of players"] [height 120] [width 250]))
;/*/*/*buttons for 'players'
(define (menul n l)
  (new button% [parent players] [label l]
       [font (make-object font% 15 'roman)]
       [min-width 200]
       [min-height 20]
       [callback (lambda (button event)
                   (begin (set! playerschosen n) (send players show #f) (send startframe show #t)))]))

(define p1 (menul 1 "1-player"))
(define p2 (menul 2 "2-player"))
(define p3 (menul 3 "3-player"))
(define p4 (menul 4 "4-player"))
(define p5 (menul 5 "5-player"))
(define p6 (menul 6 "6-player"))

(send startframe show #t)
(define blist '()) ;list-of-buttons
(define play-move (playgame 6 9))
(define congrat-frame (new frame% [label ""] [min-width 400] [min-height 80][stretchable-width #f] [stretchable-height #f]))
(define congrat (new message% [parent congrat-frame] [font (make-object font% 40 'roman)] [label "                                    "]))

(define but (new button% [parent congrat-frame] [label "New Game"] [font (make-object font% 30 'roman)]
                 [callback (lambda (b e) (begin (set! WIN #f) (set! whose-turn "red") (set! z 0) (set! cnt 0) (set! globalboard (board (make-board 6 9) 6 9)) (set! play-move (playgame 6 9)) (improve-buttons blist) (set! playerschosen 0) (send congrat-frame show #f)
                                                (send msg set-label "                                                                 ")
                                                (send startframe show #t)))])) 

(define BOARD (new frame%
                   [label "|->Chain Reaction<-|"]
                   [min-width 900]
                   [min-height 690]
                   [stretchable-width #f]
                   [stretchable-height #f]))

(define turn (new message% [parent BOARD] [label whose-turn]))

(define undo (new button% [parent BOARD] [min-width 50] [min-height 10] [label "Undo"]
                  [callback (lambda (b e) (begin (play-move 'undo 0 0) (improve-buttons blist) (send turn set-label prev-turn) (set! whose-turn prev-turn)))]))

(define (skip cl)
  (if (and (= z 1) (checkcolor whose-turn 0)) (if (not (= (length playerlist) 1))  (begin (set! whose-turn (next playerlist cl)) (skip whose-turn)) (display "")) (checkboard)))

;for help
(define hf (new frame% [label "Chain reaction help"] [min-width 520] [min-height 520]))
(define help (new button%   [label "HELP"]
                  [parent startframe]
                  [font (make-object font% 30 'roman)]
                  [min-width 100]
                  [min-height 50]
                  [callback (lambda (button event)
                              (send hf show #t))]))
(define help-image (make-object bitmap% "help.png" 'png))
(define help-inst (new button% [parent hf] [min-width 500] [min-height 500]
                       [font (make-object font% 10 'roman)]
                       [label help-image]
                       [callback (lambda (b e ) (display ""))]))

(define (buttoner n panel rn)
  (define cel (cell 0 "#" 0 0 0))
  (define (h i)
    (cond [(> i 8) (set! blist
                         (append blist
                                 (list (new my-button%
                                            [cell cel]
                                            [label (get-field picture blank)]
                                            [parent panel]
                                            [min-width 100]  [min-height 100]
                                            [callback (lambda (button event)
                                                        (let* ([r (send button get-r)] [c (send button get-c)] [cl whose-turn])
                                                          (if (valid-move? r c cl)
                                                              (begin (play-move r c cl) (improve-buttons blist) (if (= z 1) (skip cl) (display "")) 
                                                                     (if (and (eq? WIN #t) (> playerschosen 1)) (begin (sleep 1) (send BOARD show #f) (send congrat set-label (string-append "Congrats!! " prev-turn " won.")) (send congrat-frame show #t))
                                                                         (if (and (eq? WIN #t) (= playerschosen 1)) (sleep 0.001)  
                                                                             (send turn set-label whose-turn))))
                                                              (send turn set-label (string-append "Invalid move!. " whose-turn " retry.")))))]))))]
          [else (begin (set! blist (append blist (list (new my-button%
                                                            [cell cel]
                                                            [label  (get-field picture blank)]
                                                            [font (make-object font% 20 'roman)]
                                                            [parent panel]
                                                            [min-width 100]  [min-height 100]
                                                            [callback (lambda (button event)
                                                                        (let* ([r (send button get-r)] [c (send button get-c)] [cl whose-turn])
                                                                          (if (valid-move? r c cl)
                                                                              (begin (play-move r c cl) (improve-buttons blist) (if (= z 1) (skip cl) (display "")) 
                                                                                     (if (and (eq? WIN #t) (> playerschosen 1)) (begin  (sleep 1) (send BOARD show #f) (send congrat set-label (string-append "Congrats!! " prev-turn " won.")) (send congrat-frame show #t)) 
                                                                                         (if (and (eq? WIN #t) (= playerschosen 1)) (sleep 0.001)
                                                                                             (send turn set-label whose-turn)
                                                                                             )))
                                                                              (send turn set-label (string-append "Invalid move!. " whose-turn " retry.")))))]))))
                       (h (+ i 1)))]))
  (h 1))                 
(define row1 (new horizontal-panel% [parent BOARD])) (buttoner 9 row1 1)
(define row2 (new horizontal-panel% [parent BOARD])) (buttoner 9 row2 2)
(define row3 (new horizontal-panel% [parent BOARD])) (buttoner 9 row3 3)
(define row4 (new horizontal-panel% [parent BOARD])) (buttoner 9 row4 4)
(define row5 (new horizontal-panel% [parent BOARD])) (buttoner 9 row5 5)
(define row6 (new horizontal-panel% [parent BOARD])) (buttoner 9 row6 6)

(define (initialise btnlist)
  (define (helper i l)
    (cond [(null? l) (display "")]
          [else (let* ([rn (floor (/ i 9))]
                       [cn (- i (* rn 9))]
                       [critmass (cond [(and (or (= rn 0) (= rn 5)) (or (= cn 0) (= cn 8))) 2]
                                       [(and (not (or
                                                   (= rn 0) (= rn 5))) (or (= cn 0) (= cn 8))) 3]
                                       [else 4])])
                  (begin (send (car l) set-cell! (cell 0 "#" critmass rn cn)) (helper (+ i 1) (cdr l))))]))
  (helper 0 btnlist))
(initialise blist)