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
        (if (null? (cddr ordered-weight-leafs))
            new-generated-node
            (successive-merge (adjoin-set new-generated-node
                                          (cddr ordered-weight-leafs)))))))
; test
(define test-ordered-leafs (list (list 'A 2)
                                 (list 'B 2)
                                 (list 'C 3)
                                 (list 'D 4)
                                 (list 'E 5)))
(display "test-ordered-leafs\n")
(generate-huffman-tree test-ordered-leafs)


; 2.70
(define rock-song-symbols
  (list (list 'A 2)
        (list 'NA 16)
        (list 'BOOM 1)
        (list 'SHA 3)
        (list 'GET 2)
        (list 'YIP 9)
        (list 'JOB 2)
        (list 'WAH 1)))
(display "2.70 generated-huffman-tree \n")
(generate-huffman-tree rock-song-symbols)
(display "2.70 encode output\n")
(encode
 (list 'GET 'A 'JOB
       'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA
       'GET 'A 'JOB
       'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA
       'WAH 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP
       'SHA 'BOOM)
 (generate-huffman-tree rock-song-symbols))
; 需要5个bit来encode，如果使用fixed-length code，需要3个bit来encode

; 2.71
(define (generate-2-of-pow-symbl-frequency symbl pow)
  (list symbl (expt 2 pow)))
(define (generate-symbols-frequency-list symbols)
  (define (helper items iter)
    (if (null? items)
        '()
        (cons (generate-2-of-pow-symbl-frequency (car items) iter)
              (helper (cdr items) (+ iter 1)))))
  (helper symbols 0))
; 事实上问题所要求的比特数即为huffman-tree的深度
(define (depth tree)
  (if (leaf? tree)
      0
      (let ((left-depth (depth (left-branch tree)))
            (right-depth (depth (right-branch tree))))
        (if (> left-depth right-depth)
            (+ 1 left-depth)
            (+ 1 right-depth)))))
; n = 5
(define s-5 (generate-symbols-frequency-list (list 'A 'B 'C 'D 'E)))
(display "huffman-tree when n = 5\n")
(define t5 (generate-huffman-tree s-5))
t5
; n = 10
(define s-10 (generate-symbols-frequency-list (list 'A 'B 'C 'D 'E 'F 'G 'H 'I 'J)))
(display "huffman-tree when n = 10\n")
(define t10 (generate-huffman-tree s-10))
t10
; 当 n = 5 时，huffman-tree深度为4，当 n = 10 时，huffman-tree深度为9
; 用数学归纳法，假设 n = k 时，huffman-tree深度为 k - 1，
; 只要证明当 n = k + 1时，huffman-tree深度为 k 即可
; 当 n = k + 1 时，the relative frequency of the most frequenct symbol 为 2^k
; 而前 n - 1 项的frequency之和是q=2的等比数列，数列和为2^k - 1，小于2^k
; 由huffman-tree算法可知，当 n = k + 1 时生成的huffman-tree 是
; 以最大频率树叶和前 n - 1 项生成的子树为左右branch的二叉树，其深度为由前 n - 1 项生成的子树的深度加上1，证毕
; 由上述证明易得，最大频率符号只需要1个bit就可以encode，最小频率符号则需要 n - 1 个bit来编码

; 2.72 TODO