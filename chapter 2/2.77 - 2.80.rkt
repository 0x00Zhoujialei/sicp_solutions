#lang racket

; 2018/08/11
; exercise 2.77 - 2.80

; 2.77
; 直接 evaluate (magnitude z) 相当于在
; type-and-operation table中找对应于
; type为complex,operation为magnitude的item
; 而原来的代码中并没有注册这个item
; 在将题设的代码加入到complex package之后
; evaluate (magnitude z)的过程如下
; (magnitude z)
; (apply-generic 'magnitude (cons 'complex (cons 'rectangular (cons 3 5)))
; -> (apply-generic 'magnitude (cons 'rectangular (cons 3 5)))
; -> (sqrt (+ (square 3) (square 5)))
; -> 6
; 由上述过程易知apply-generic总共调用了两次

; 2.78
; 要保证修改过的版本既能考虑到scheme internal type system
; 又要保证原有代码的兼容性，我们需要对原有的打标签、取标签、取值操作
; 对number类型特殊处理
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))

; 2.79
; 事实上 equ 应该分发到scheme-number-package，rational-package
; 这些 sub-package 中，也就是应用 data-directed programming 在 equ 中

(define (equ? x y)
  (apply-generic 'equ x y))

; for scheme-number-package
(put 'equ '(scheme-number sheme-number)
     =)
; for rational-package
(put 'equ '(rational rational)
     (lambda (x y)
       (let ((difference (sub-rat (x y))))
         (= 0
            (numer difference)))))
; for complex-package
(put 'equ '(complex complex)
     (lambda (x y)
       (let ((difference (sub-complex (x y))))
         (and (real-part difference)
              (imag-part difference)))))

; 2.80
; 同上题，应用 data-directed programming 到 =zero? 中

(define (=zero? x)
  (apply-generic '=zero? x))

; for scheme-number-package
(put '=zero? 'scheme-number
     (lambda (x) (= x 0)))
; for rational-package
(put '=zero? 'rational
     (lambda (x) (= 0 (numer x))))
; for complex-package
(put '=zero? 'complex
     (lambda (x) (and (= 0 (real-part x))
                      (= 0 (imag-part x)))))

