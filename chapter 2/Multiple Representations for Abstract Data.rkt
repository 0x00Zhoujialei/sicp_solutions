#lang racket
; 2018/08/06
; sample codes from the book and exercises

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))
; tagged data object
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z) (eq? (type-tag z) 'polar))

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type: REAL-PART" z))))
(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type: IMAG-PART" z))))
(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type: MAGNITUDE" z))))
(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type: ANGLE" z))))

(define (make-from-real-imag real imag)
  (make-from-real-imag-rectangular real imag))
(define (make-from-mag-ang magnitude angle)
  (make-from-mag-ang-polar magnitude angle))
(define (square x) (* x x))

; 2.73
; a
; deriv函数事实上建立了以product, sum等为types轴，以deriv操作本身为operations轴的表
; 对于number和variable类型之外的表达式使用data-directed programming technique来处理
; number?是对于schema的基础类型数字类型的判断，并不需要我们封装相关逻辑来进行处理
; variable? 函数传入的参数类型为基础类型symbol，对于这一基础类型的逻辑处理也不需要我们封装相关逻辑来处理

; b
(define (=number? exp num)
    (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
(define (make-product p1 p2)
    (cond ((or (=number? p1 0) (=number? p2 0)) 0)
          ((=number? p1 1) p2)
          ((=number? p2 1) p1)
          ((and (number? p1) (number? p2)) (* p1 p2))
          (else (list '* p1 p2))))
(define (make-exponentiation base pwr)
  (cond ((=number? pwr 0) 1)
        ((=number? pwr 1) base)
        ((and (=number? base 0) (=number? pwr 0))
         (error "no meaning!"))
        ((=number? base 0) 0)
        ((and (number? base) (number? pwr))
         (expt base pwr))
        (else
         (list '** base pwr))))

(define (install-sum-package)
  (define (addend exp) (cadr exp))
  (define (augend exp) (caddr exp))
  (define (deriv exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  ;(put 'deriv '+ deriv)
  'done)

(define (install-product-package)
  (define (multiplicand exp) (cadr exp))
  (define (multiplier exp) (caddr exp))
  (define (deriv exp var)
    (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
  ;(put 'deriv '* deriv)
  'done)

; c

(define (install-exponent-package)
  (define (base expn) (cadr expn))
  (define (exponent expn) (caddr expn))
  (define (deriv exp var)
    (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp)
                                                         (make-sum (exponent exp)
                                                                   '-1))
                                     (deriv (base exp) var))))
  ;(put 'deriv '** deriv)
  'done)

; d
; 我的理解，derivative system不需要更改，只需要install一个包含对sum、product等进行
; deriv操作的package即可