#lang racket

(define (key record)
  (car record))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

; 2018/08/04
; exercise 2.66

; 2.66
(define (b-tree this-entry left-branch right-branch)
  (list this-entry left-branch right-branch))

(define (this-entry b-tree)
  (car b-tree))

(define (left-branch b-tree)
  (cadr b-tree))

(define (right-branch b-tree)
  (caddr b-tree))

(define (b-lookup given-key b-tree-records)
  (if (null? b-tree-records)
      false
      (let ((current-entry (this-entry b-tree-records)))
        (cond
          ((= given-key current-entry)
           b-tree-records)
          ((< given-key current-entry)
           (b-lookup given-key (left-branch b-tree-records)))
          ((> given-key current-entry)
           (b-lookup given-key (right-branch b-tree-records)))))))

; test
(require (file "representing sets tree.rkt"))
(b-lookup 3 t1)
(b-lookup 8 t2)
