#lang racket
; 2018/05/18
; exercise 2.40 - 2.43

; 2.40
(define nil '())
(define (square x) (* x x))
(define (enumerate-interval x y)
  (if (> x y)
      nil
      (cons x
            (enumerate-interval (+ x 1)
                                y))))
(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime? num)
  (define (helper count)
    (cond ((> (square count) num) #t)
          ((= 0 (remainder num count)) #f)
          (else (helper (+ count 2)))))
  (if (= 0 (remainder num 2))
      #f
      (helper 3)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair)
                                  (cadr pair))))
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 2 n)))
(unique-pairs 6)
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
(prime-sum-pairs 6)

; 2.41
(define (left-element triple)
  (car triple))
(define (middle-element triple)
  (car (cdr triple)))
(define (right-element triple)
  (car (cdr (cdr triple))))
; test
(define tri1 (list 1 2 3))
(= (left-element tri1) 1)
(= (middle-element tri1) 2)
(= (right-element tri1) 3)

(define (distinct-triple? triple)
  (let ((a (left-element triple))
        (b (middle-element triple))
        (c (right-element triple)))
  (and (not (= a b))
       (not (= a c))
       (not (= b c)))))
(define (elements-less-than-or-equal-n triple n)
  (let ((a (left-element triple))
        (b (middle-element triple))
        (c (right-element triple)))
    (and (<= a n)
         (<= b n)
         (<= c n))))
(define (elements-positive triple)
  (let ((a (left-element triple))
        (b (middle-element triple))
        (c (right-element triple)))
    (and (> a 0)
         (> b 0)
         (> c 0))))
(define (triple-sum triple)
  (accumulate + 0 triple))
(define (ordered-triples-less-than-s n s)
  (filter (lambda (triple)
            (and (elements-positive triple)
                 (= (triple-sum triple)
                    s)
                 (distinct-triple? triple)
                 (elements-less-than-or-equal-n triple
                                                n)
                 ))
          (flatmap (lambda (first-element)
                     (map (lambda (second-element)
                            (list first-element
                                  second-element
                                  (- s first-element second-element)))
                          (enumerate-interval 1 n)))
                   (enumerate-interval 1 n))))
; test
(ordered-triples-less-than-s 7 12)

; 2.42
; 代码流程是清晰的，重要的是头脑清醒，在2x2, 3x3大小的棋盘上的皇后问题是空集
(define (kth-position positions k)
  (if (= k 1)
      (car positions)
      (kth-position (cdr positions) (- k 1))))
(define (safe? k positions)
  (define (same-row? a b)
    (= a b))
  (define (same-diagonal? a b)
    (= (- k 1)
       (abs (- a b))))
  (let ((first-queen (car positions))
        (kth-queen (kth-position positions k)))
    (if (= k 1) ; only one queen to place, must be true
        #t
        (if (or (same-row? first-queen kth-queen) 
                (same-diagonal? first-queen kth-queen))
            #f
            (safe? (- k 1)
                   (cdr positions))))))
(define empty-board '())
(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))
(define (queens board-size) ; return all solutions to the problem of placing n queens on an nxn chessboard
  (define (queen-cols k) ; return the sequence of all way to place queens in the first k columns of the board
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens) ; rest-of-queens is a way to place k-1 queens in the first k-1 columns
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens)) ; adjoins a new row-column postion to a set of positions
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
; test
(queens 1)
(queens 2)
(queens 3)
(queens 4)
(queens 8)

; 2.43
; 记运行adjoin-position所需时间为t
; 原答案所需时间为 k * board-size * t = T
; 修改后的答案为 board-size * k * board-size * t = board-size * T
