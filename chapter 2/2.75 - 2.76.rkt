#lang racket

; 2018/08/10
; exercise 2.75 - 2.76

; 2.75

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* x (cos y)))
          ((eq? op 'imag-part)
           (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

; 2.76

; 对于 explicit dispatch, 增加新的类型我们需要在每个操作之下
; 增加对于新类型的处理的语句，不适合增加新的类型也不适合增加新的操作
; 增加新的操作需要我们在每个操作之下写出对于原有不同类型的处理的代码
; 对于 message-passing style
; 增加新的类型只需要我们在这个类型内给出原有操作对这个类型的处理的代码
; 增加新的操作则需要在每个原有类型中给出对于这个操作的对应的代码，不适合增加新的操作，适合增加新的类型
; 对于 data-directed style
; 增加新的类型和增加新的操作一样，通过install 新的package或者
; 修改原有的package就能达到目的
; 增加新的类型或者新的操作都适合
