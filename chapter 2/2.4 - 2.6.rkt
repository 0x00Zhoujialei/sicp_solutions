#lang racket
; exercise 2.4 - 2.6

; 2.4

; 由定义，这里的car函数接收一个符合下述特征的procedure
; i>. 它的参数是一个接收两个参数的procedure
; 并执行这个procedure
; cons函数接收两个函数，返回一个符合下述特征的procedure
; i>. 它的参数是一个接收两个参数的procedure
; (car (cons x y)) 相当于将car函数返回的(lambda (p q) p)
; 代入到(lambda (m) (m x y))执行
; 即向(lambda (p q) p)传入参数x, y并执行最后返回x
; 那么容易得到cdr的定义
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
   (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

; 2.5
(define (pow base exp)
  (if (= exp 0)
      1
      (* base (pow base (- exp 1)))))
(define (new-cons a b)
  (* (pow 2 a)
     (pow 3 b)))
(define (exp x base) ; 过程抽象，求a,b的过程本质就是已知一个数和一个因数求指数的问题
  (define (helper count x)
    (if (not (= 0 (remainder x base)))
        count
        (helper (+ count 1) (/ x base))))
  (helper 0 x))
      
(define (new-car x)
  (exp x 2))
(define (new-cdr x)
  (exp x 3))
; test
(define n1 (new-cons 3 2))
(new-car n1)
(new-cdr n1)

; 2.6
; church numerals，将numbers用procudure表示
; 在廖雪峰关于javascript的教程中关于函数一节也讲到了这个
; 2.6的求解重点在于理解substitution model
; 在借助 evaluate (add-1 zero) 得到
; (lambda (f) (lambda (x) (f x))) 之后
; 便得到了one的procedure定义，two的定义也就显而易见了
; 在add-1函数中，加一的动作是由((n f) x)之前的f决定的
; 在本题中的加自然是把这个f改成由参数决定的(a f)
; ((b f) x)得到的procedure作为参数代入(a f)中 自然得到了procedure相加的效果
(newline)
(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add a b) (lambda (f) (lambda (x) ((a f) ((b f) x)))))
((zero +) 1)
(((add one two) (lambda (x) (display "hello"))) 3) ; hellohellohello
