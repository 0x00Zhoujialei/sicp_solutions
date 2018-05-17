#lang racket
; 2018/05/16
; exercise 2.33 - 2.39

; 2.33
(define nil '())
(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))
(define (map p seq)
  (accumulate (lambda (x y)
                (cons (p x)
                      y))
              nil
              seq))
; test
(map (lambda (x) (+ x 2)) ; '(3 4 5 6 7)
     (list 1 2 3 4 5))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
; test
(append (list 1 2 3) (list 4 5 6))

(define (length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))
; test
(length (list 1 2 3))
(length '())

; 2.34
; horner-rule描述是从最里面一层开始描述的
; 但是对于题设给出的acumulate应用
; 我们应该从最外层开始分解，这也是higher-terms这个变量名字所暗示的方法
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* x higher-terms)))
              0
              coefficient-sequence))
; test
(horner-eval 2 (list 1 3 0 5 0 1))

; 2.35
; 原定义
(define (count-leaves t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (+ (count-leaves (car t))
                 (count-leaves (cdr t))))))
; test
(count-leaves (list 1 2))
(count-leaves (list 1 (list 2 (list 3 (list 4 5)))))

; count-leaves对于给定树节点t做了两件事
; 数出t的子节点的叶子数, 对应于accu-count-leaves的map过程
; 算出子节点叶子数之和, 对应于accu-count-leaves的accumulate过程 
(define (accu-count-leaves t) 
  (accumulate +
              0
              (map count-leaves
                   t)))
; test
(accu-count-leaves (list 1 2))
(accu-count-leaves (list 1 (list 2 (list 3 (list 4 5)))))

; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
; test
(define test-s (list (list 1 2 3)
                     (list 4 5 6)
                     (list 7 8 9)
                     (list 10 11 12)))
(accumulate-n + 0 test-s)

; 2.37
(define (map-two op a b) ; 似乎map操作符本身不支持多个参数
  (if (null? a)
      nil
      (cons (op (car a)
                (car b))
            (map-two op (cdr a) (cdr b)))))
(define (dot-product v w)
  (accumulate + 0 (map-two * v w)))

;
(define (matrix-*-vector m v)
  (map (lambda (x)
         (accumulate +
                     0
                     (map-two * v x)))
       m))
; test
(define matr1 (list (list 1 2 3 4)
                    (list 4 5 6 6)
                    (list 6 7 8 9)))
(define vec1 (list 1 1 1 1))
(matrix-*-vector matr1 vec1)

;
(define (transpose mat)
  (accumulate-n cons nil mat))
; test
(define matr2 (list (list 1 2) (list 3 4)))
(transpose matr2)
(define matr3 (list (list 1 2 3) (list 4 5 7) (list 7 8 9)))
(transpose matr3)

;
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row-of-m)
           (map (lambda (cols-of-n)
                  (dot-product row-of-m
                               cols-of-n))
                cols))
           m)))

; test
(define A (list (list 1 2) (list 1 -1)))
(define B (list (list 1 2 -3) (list -1 1 2)))
(matrix-*-matrix A B)

; 2.38
(define (fold-left op initial seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result
                  (car rest))
              (cdr rest))))
  (iter initial seq))
(define fold-right accumulate)

; test
(fold-left / 1 (list 1 2 3))
(fold-right / 1 (list 1 2 3))
(fold-left list nil (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left + 2 (list 1 2 3))
(fold-right + 2 (list 1 2 3))
; 当(op x y)与(op y x)等价的时候
; fold-right和fold-left会产生相同的结果

; 2.39
(define (a-reverse seq)
  (fold-right (lambda (x y)
                (append y (list x)))
              nil
              seq))
; test
(define seq-a (list 1 2 3 4 5))
(a-reverse seq-a)
(define seq-b (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(a-reverse seq-b)

(define (b-reverse seq)
  (fold-left (lambda (x y)
               (cons y x))
             nil
             seq))
; test
(b-reverse seq-a)
(b-reverse seq-b)