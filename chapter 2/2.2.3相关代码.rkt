#lang racket
(define (square x) (* x x))
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))
; test
(define tree1 (list 2 (list 3 (list 4 5))))
(sum-odd-squares tree1)

(define nil '())
(define (fib k) ; 1 1 2 3 5 8 
  (if (<= k 1)
      1
      (+ (fib (- k 1))
         (fib (- k 2)))))
(define (iter-fib k)
  (define (helper count a product)
    (cond ((<= k 1) 1)
          ((= count k) product)
          (else (helper (+ count 1)
                        product
                        (+ a product)))))
  (helper 1 1 1))
; test
(iter-fib 2) ; 2
(iter-fib 3) ; 3
(iter-fib 5) ; 8

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (iter-fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial seq) ;reduce
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))
(newline)
(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons nil (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (reform-sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (reform-even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map iter-fib
                           (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
              nil
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))
(list-fib-squares 10)

(define (product-of-squares-of-odd-elements seq)
  (accumulate *
              1
              (map square
                   (filter odd? seq))))
(product-of-squares-of-odd-elements (list 1 2 3 4 5))

; nested mappings
(define (prime? num)
  (define (helper count)
    (cond ((> (square count) num) #t)
          ((= 0 (remainder num count)) #f)
          (else (helper (+ count 2)))))
  (if (= 0 (remainder num 2))
      #f
      (helper 3)))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair)
                                  (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                  (enumerate-interval 1 n)))))
(prime-sum-pairs 6)

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item seq)
  (filter (lambda (x) (not (= x item)))
          seq))