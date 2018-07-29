; 2018/07/29
; exercise 2.53 - 2.55

#lang racket

; 2.53
; 略 输出什么内容其实在repl里打一打就知道了

; 2.54
(define (equal? le re)
  (cond ((and (symbol? le)
              (symbol? re))
         (eq? le re))
        ((and (null? le)  ; 按照题目的描述还不够 需要判断list是否都是空的
              (null? re))
         #t)
        ((and (list? le)
              (list? re)
              (not (null? le))
              (not (null? re)))
         (and (equal? (car le) (car re))
              (equal? (cdr le) (cdr re))))
        (else #f)))
; test
(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))

; 2.55
; 事实上 ' 就是一个special form -> quote
; 在repl上输入(list (quote a)) 等价于 (list 'a)
; 那么由题设(car ''abracadabra)等价于 (car (quote (quote abracadabra)))
; 借由之前得到的关于car的知识，容易得到它的值为quote