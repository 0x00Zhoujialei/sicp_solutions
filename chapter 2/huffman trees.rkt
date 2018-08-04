#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits)
                                          current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

; 2018/08/04
; exercise 2.67 - 2.72

; 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)
; output: '(A D A B B C A)

; 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (memq? item x)
  (cond ((null? x) false)
        ((eq? (car x) item) true)
        (else (memq? item (cdr x)))))
(define (encode-symbol message tree)
  (cond ((leaf? tree) '())
        ((or (null? tree)
             (not (memq? message
                         (symbols tree))))
         (error "bad symbol -- ENCODE-SYMBOL" message))
        ((memq? message
                (symbols (left-branch tree)))
         (cons '0
               (encode-symbol message (left-branch tree))))
        (else
         (cons 1
               (encode-symbol message (right-branch tree))))))
; test
(define test-message (decode sample-message sample-tree))
(encode test-message sample-tree)

; output: '(0 1 1 0 0 1 0 1 0 1 1 1 0)
; which is the same as the original sample message

; 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge ordered-weight-leafs)
  (if (or (null? ordered-weight-leafs) (= 1 (length ordered-weight-leafs)))
      ordered-weight-leafs
      (let ((new-generated-node (make-code-tree (car ordered-weight-leafs)
                                                (cadr ordered-weight-leafs))))
        (successive-merge (adjoin-set new-generated-node
                                      (cddr ordered-weight-leafs))))))
; test
(define test-ordered-leafs (list (make-leaf 'A 2)
                                 (make-leaf 'B 2)
                                 (make-leaf 'C 3)
                                 (make-leaf 'D 4)
                                 (make-leaf 'E 5)))
(successive-merge test-ordered-leafs)
  