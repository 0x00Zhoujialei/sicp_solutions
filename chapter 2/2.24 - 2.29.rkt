#lang racket
; 2018/05/15
; exercise 2.24 - 2.29

; 2.24
(list 1 (list 2 (list 3 4)))
; box-and-pointer structure省略
; tree model也省略

; 2.25
(define list1 (list 1 3 (cons 5 7) 9))
(cdr (car (cdr (cdr list1)))) ; 7
(define list2 (list (list 7)))
(car (car list2))
(define pair3 (cons 1
                    (cons 2
                          (cons 3
                                (cons 4
                                      (cons 5
                                            (cons 6
                                                  7)))))))
(cdr (cdr (cdr (cdr (cdr (cdr pair3))))))

; 2.26
; 1. '(1 2 3 4 5 6)
; 2. '((1 2 3) 4 5 6)
; 3. '((1 2 3) (4 5 6))

; 2.27
(define nil '())
(define (reverse k)
  (define (list-ref items n)
    (if (not (list? items))
        items
        (if (= n 0)
            (car items)
            (list-ref (cdr items) (- n 1)))))
  (define (length items) ; 如果传入的items是list,返回list的length,否则返回-1
    (if (not (list? items))
        -1
        (if (null? items)
            0
            (+ 1 (length (cdr items))))))
  (define (helper count items)
    (cond 
      ((= 0 count) (cons (helper (- (length (list-ref items count))
                                    1)
                                 (list-ref items count))
                         nil))
      ((= -2 count) items) ;不是list的时候length返回-1,但还会被减1,所以-2的时候返回items本身
      (else (cons (helper (- (length (list-ref items count))
                             1)
                          (list-ref items count))
                  (helper (- count 1)
                          items)))
      ))
  (helper (- (length k)
             1)
          k))
; test
(define test-list (list (list 1 2) (list 3 4)))
(reverse test-list)
(define test-list1 (list (list 1 2) (list 3 4)))
(reverse test-list1)
(define test-list2 (list 1 2 3 4 5 6))
(reverse test-list2)
(define test-list3 (list (list 1 2) (list 3 4) (list 5 6)))
(reverse test-list3)

; 2.28
(define (fringe items)
  (cond ((not (list? items)) (cons items
                                   '()))
        ((and (= (length items) 1)
             (not (list? (car items)))) items) ; 非嵌套的表
        ((= (length items) 0) items) ; 空表
        (else (append (fringe (car items))
                      (fringe (cdr items))
              ))))
; test
(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))

; 2.29
; a
(define left-branch car)
(define (right-branch binary-mobile)
  (car (cdr binary-mobile)))
(define branch-length car)
(define (branch-structure branch)
  (car (cdr branch)))
; test
(newline)
(define t1 (list (list 3 1) (list 4 1)))
(left-branch t1)
(branch-length (left-branch t1))
(branch-structure (left-branch t1))
(right-branch t1)
(branch-length (right-branch t1))
(branch-structure (right-branch t1))

; b
(define (last-branch? branch)
    (not (list? (branch-structure branch))))
(define (total-weight-of-branch branch)
  (if (last-branch? branch)
      (branch-structure branch)
      (total-weight-of-branch (branch-structure branch)
                              )))
(define (total-weight mobile)
  (+ (total-weight-of-branch (left-branch mobile))
     (total-weight-of-branch (right-branch mobile))))
; test
(newline)
(define t2 (list (list 7 (list 5 2)) (list 6 (list 7 4))))
(total-weight t2)

; c
; 借助我们在b上写的total-weight-of-branch
(define (torque branch)
  (if (last-branch? branch)
      (* (branch-length branch)
         (branch-structure branch))
      (* (branch-length branch)
         (total-weight-of-branch (branch-structure branch)))))
(define (balanced? mobile)
  (= (torque (left-branch mobile))
     (torque (right-branch mobile))))
; test
(define t3 (list (list 4 (list 7 5)) (list 5 (list 7 4))))
(newline)
(balanced? t2)
(balanced? t3)

; d
; 只需要将last-branch?函数改成
; (define (last-branch? branch)
;    (not (pair? (branch-structure branch))))
; 即可