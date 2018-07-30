; 2018/07/30
; exercise 2.56 - 2.58
; include code from book for convenience

#lang racket
; selector and constructor needs to be defined
; (variable? e)
; (same-variable? v1 v2)
; (sum? e)
; (addend e)
; (augend e)
; (make-sum a1 a2)
; (product? e)
; (multiplier e)
; (multiplicand e)
; (make-product m1 m2)
;;

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (deriv (multiplier exp)
                                                       var)
                                                (multiplicand exp))
                                  (make-product (deriv (multiplicand exp)
                                                       var)
                                                (multiplier exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp)
                                                         (make-sum (exponent exp)
                                                                   '-1))
                                     (deriv (base exp) var))))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num))) 

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s)
  (let ((rest-to-sum (cddr s)))
    (define (helper rts result)
      (if (null? (cdr rts))
          (make-sum (car rts)
                    result)
          (helper (cdr rts)
                  (make-sum (car rts)
                            result))))
    (helper rest-to-sum 0)))
                    
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (let ((rest-to-multi (cddr p)))
    (define (helper rtm result)
      (if (null? (cdr rtm))
          (make-product (car rtm)
                        result)
          (helper (cdr rtm)
                  (make-product (car rtm)
                                result))))
    (helper rest-to-multi 1)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

; test
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

; 2.56
(define (exponentiation? expn)
  (and (pair? expn) (eq? (car expn) '**)))
(define (base expn) (cadr expn))
(define (exponent expn) (caddr expn))
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

; test
(define ex1 (make-exponentiation 't 10))
(define ex2 (make-exponentiation 'n 9))
(base ex2)
(base ex1)
(exponent ex2)
(exponent ex1)
(deriv ex1 't)
(deriv ex2 'n)
(deriv ex1 'k)
(deriv ex2 't)

; 2.57 test
(define ex3 '(* x y (+ x 3)))
(deriv ex3 'x)

; 2.58
; a
(define (infix-addend s) (car s))
(define (infix-augend s) (caddr s))
(define (infix-sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (infix-make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (infix-product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (infix-multiplier p) (car p))
(define (infix-multiplicand p) (caddr p))
(define (infix-make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

(define (infix-exponentiation? expn)
  (and (pair? expn) (eq? (cadr expn) '**)))
(define (infix-base expn) (car expn))
(define (infix-exponent expn) (caddr expn))
(define (infix-make-exponentiation base pwr)
  (cond ((=number? pwr 0) 1)
        ((=number? pwr 1) base)
        ((and (=number? base 0) (=number? pwr 0))
         (error "no meaning!"))
        ((=number? base 0) 0)
        ((and (number? base) (number? pwr))
         (expt base pwr))
        (else
         (list base '** pwr))))

(define (infix-deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        ((infix-sum? exp) (infix-make-sum (infix-deriv (infix-addend exp) var)
                                          (infix-deriv (infix-augend exp) var)))
        ((infix-product? exp) (infix-make-sum (infix-make-product (infix-deriv (infix-multiplier exp)
                                                                               var)
                                                                  (infix-multiplicand exp))
                                              (infix-make-product (infix-deriv (infix-multiplicand exp)
                                                                               var)
                                                                  (infix-multiplier exp))))
        ((infix-exponentiation? exp)
         (infix-make-product (infix-exponent exp)
                             (infix-make-product (infix-make-exponentiation (infix-base exp)
                                                                            (infix-make-sum (infix-exponent exp)
                                                                                            '-1))
                                                 (deriv (infix-base exp) var))))
        (else
         (error "unknown expression type: DERIV" exp))))

; 2.58 a test
(define infix-add-1 (list 3 '+ (list 4 '* 'x)))
(define infix-mult-1 (list 3 '* (list 4 '* 'x)))
(define infix-expt-1 (list 'x '** 4))
(infix-deriv infix-add-1 'x)
(infix-deriv infix-mult-1 'x)
(infix-deriv infix-expt-1 'x)

;b
; this job cannot be done if we only
; design predicates, selectors and constructors
; and remain the implementation of deriv function to be untouched
; our origin impl of deriv lacks of logic about the order of calculation 