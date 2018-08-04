#lang racket
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

; 2018/08/01
; exercise 2.63 - 2.65

; 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

; test
(define tree-1 (make-tree 3
                          (make-tree 2 '() '())
                          (make-tree 4 '() '())))
(define tree-2 (make-tree 6
                          (make-tree 3
                                     (make-tree 2 '() '())
                                     (make-tree 4 '() '()))
                          (make-tree 7
                                     (make-tree 5 '() '())
                                     (make-tree 8 '() '()))))
(define tree-3 (make-tree 1
                          '()
                          (make-tree 2
                                     '()
                                     (make-tree 3
                                                '()
                                                (make-tree 4
                                                           '()
                                                           (make-tree 5
                                                                      '()
                                                                      (make-tree 6
                                                                                 '()
                                                                                 (make-tree 7
                                                                                            '()
                                                                                            '()))))))))
(define tree-4 (make-tree 1
                          (make-tree 2
                                     (make-tree 3
                                                (make-tree 4
                                                           (make-tree 5
                                                                      (make-tree 6
                                                                                 (make-tree 7
                                                                                            '()
                                                                                            '())
                                                                                 '())
                                                                      '())
                                                           '())
                                                '())
                                     '())
                          '()))


(tree->list-1 tree-1)
(tree->list-1 tree-2)
(tree->list-1 tree-3)
(tree->list-1 tree-4)
(tree->list-2 tree-1)
(tree->list-2 tree-2)
(tree->list-2 tree-3)
(tree->list-2 tree-4)

; 对于 tree->list-1 和 tree->list-2
; 两者对于相同的树输出的list相同
; tree-list-1的时间复杂度为theta(n^2)
; tree-list-2的时间复杂度为theta(n)

; 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; test
(define elts-1 (list 1 2 3 4 5 6 7))
(list->tree elts-1)

; partial-tree做了这样一件事：
; 对于一个长度为n的有序列表，若其长度为奇数取中间节点为根节点，否则取第n/2为根节点
; 这样这个问题就可以分解成求取两个子列表的平衡树问题
; 对于这两个子列表，可以按照上述步骤求取根节点和左右子列表，这样递归的做下去
; 直到递归到长度为3或者2或者1的子列表为止，
; 对于长度为3的子列表，中间元素为根元素，第一个和第三个元素分别为左右叶子元素
; 对于长度为2的子列表，第一个元素为根元素，第二个元素为右叶子元素
; 对于长度为1的子列表，它本身就是一个叶子元素，这样从下向上地，我们就能得到一个有序列表对应的二叉平衡树了
; '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

; 对于长度为n的列表，partial-tree对列表中的每个元素执行一次make-tree
; 故而是时间复杂度为theta(n)

; 2.65
(define (union-list l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        (else
         (let ((x1 (car l1)) (x2 (car l2)))
           (cond ((= x1 x2) (union-list (cdr l1) l2))
                 ((< x1 x2) (cons x1
                                  (union-list (cdr l1) l2)))
                 ((< x2 x1) (cons x2
                                  (union-list l1 (cdr l2)))))))))
(define (intersection-list l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (let ((x1 (car l1)) (x2 (car l2)))
        (cond ((= x1 x2) (cons x1
                               (intersection-list (cdr l1) (cdr l2))))
              ((< x1 x2) (intersection-list (cdr l1)
                                            l2))
              ((< x2 x1) (intersection-list l1
                                            (cdr l2)))))))
(define (union-set s1 s2)
  (let ((l1 (tree->list-2 s1)) (l2 (tree->list-2 s2)))
    (let ((ul (union-list l1 l2)))
      (list->tree ul))))
(define (intersection-set s1 s2)
  (let ((l1 (tree->list-2 s1)) (l2 (tree->list-2 s2)))
    (let ((il (intersection-list l1 l2)))
      (list->tree il))))

; test
(provide t1 t2)
(define t1 (list->tree (list 1 3 5 7 9 11)))
(define t2 (list->tree (list 2 4 6 8 10 12 14 16)))
(intersection-set t1 t2)
(union-set t1 t2)

; 做一个简单的解释，对于上述union-set, intersection-set 两个函数
; tree->list-2 时间复杂度为theta(n)
; union-list 与 intersection-list 时间复杂度为 theta(n)
; list->tree 时间复杂度为 theta(n)
; 综上有union-set与intersection-set两个函数的时间复杂度为theta(n)
