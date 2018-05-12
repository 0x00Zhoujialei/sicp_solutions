#lang racket
; 2018/05/12
; exercise 2.2 - 2.3

; 2.2
(define (make-point x y)
  (cons x y))
(define (x-point x)
  (car x))
(define (y-point x)
  (cdr x))

(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (x-average x y)
  (/ (+ (x-point x) (x-point y)) 2))
(define (y-average x y)
  (/ (+ (y-point x) (y-point y)) 2))
(define (x-distance x y)
  (abs (- (x-point x) (x-point y))))
(define (y-distance x y)
  (abs (- (y-point x) (y-point y))))

(define (midpoint-segment segment)
  (let ((start-point (start-segment segment))
        (end-point (end-segment segment)))
  (make-point (x-average start-point
                         end-point)
              (y-average start-point
                         end-point))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; test
(define p1 (make-point 3 3))
(define p2 (make-point 6 6))
(define l1 (make-segment p1 p2))
(define m1 (midpoint-segment l1))
(print-point m1) ; (9/2,9/2)

; 2.3
; 想要周长和面积函数在不同的矩形实现下成立，那么
; 两个函数的实现必定与矩形实现本身无关
; 这里通过由矩形函数实现本身来提供矩形长与宽的信息，隔离开了
; 计算矩形周长与面积的过程和求解矩形本身长与宽的过程

(define (rectangle l1 l2) ; 如果只是为了计算矩形的周长和面积，那么只给矩形的两条互相垂直的边是合适的
  (cons l1 l2))
(define (width-rectangle rectangle)
  (car rectangle))
(define (height-rectangle rectangle)
  (cdr rectangle))
(define (rec-perimeter rectangle)
  (+ (* 2 (width-rectangle rectangle))
     (* 2 (height-rectangle rectangle))))
(define (rec-area rectangle)
  (* (width-rectangle rectangle)
     (height-rectangle rectangle)))

(define (new-rectangle p1 p2 p3 p4)  ; 四个点分别是从任意点开始顺时针数这个矩形得到的四个点
  (cons (cons p1 p2)
        (cons p3 p4)))
(define (rec-diagonal rec) ; 对角线
  (make-segment (car (car rec))
                (car (cdr rec))))
(define (new-width-rectangle rectangle) 
  (let ((p1 (start-segment (rec-diagonal rectangle)))
        (p2 (end-segment (rec-diagonal rectangle))))
    (x-distance p1 p2)))
(define (new-height-rectangle rectangle)
  (let ((p1 (start-segment (rec-diagonal rectangle)))
        (p2 (end-segment (rec-diagonal rectangle))))
    (y-distance p1 p2)))
  (define (new-rec-perimeter rectangle)
  (+ (* 2 (new-width-rectangle rectangle))
     (* 2 (new-height-rectangle rectangle))))
(define (new-rec-area rectangle)
  (* (new-width-rectangle rectangle)
     (new-height-rectangle rectangle)))



; test
(newline)
(define r1 (rectangle 3 2))
(rec-perimeter r1)
(rec-area r1)

(define r2 (new-rectangle (make-point 0 0) (make-point 0 5) (make-point 7 5) (make-point 7 0)))
(new-rec-perimeter r2)
(new-rec-area r2)
  
  