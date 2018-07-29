#lang racket
; 2018/07/02
; exercise 2.44 - 2.52
; finished at 2018/07/29
; 为了连贯性的考虑，将书中的源码和课后习题答案写一块了
(require graphics/graphics)
(require (planet soegaard/sicp:2:1/sicp))

;(paint einstein) ;; OK, use einstein instead of wave
(define wave einstein)

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

;; exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

; test
(paint (up-split wave 2))
(paint (corner-split wave 4))

; Heigher-order operations
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (new-flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (new-square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

; exercise 2.45
(define (split divide-op conquer-op)
  (define (helper painter n)
    (if (= n 0)
        painter
        (let ((smaller (helper painter (- n 1))))  ;; 为了递归调用helper过程，必须将helper具名化
          (divide-op painter (conquer-op smaller smaller)))))
  (lambda (painter n)
    (helper painter n)))

; Frame
(define (frame-coord-map frame)
  (lambda (v)
    (vector-add
     (frame-origin frame)
     (vector-add (vector-scale (vector-xcor v)
                               (frame-edge1 frame))
                 (vector-scale (vector-ycor v)
                               (frame-edge2 frame))))))
; exercise 2.46
(define (make-vect x y)
  (cons x y))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cadr vect))
(define (add-vect lv rv)
  (make-vect (+ (xcor-vect lv) (xcor-vect rv))
             (+ (ycor-vect lv) (ycor-vect rv))))
(define (sub-vect lv rv)
  (make-vect (- (xcor-vect lv) (xcor-vect rv))
             (- (ycor-vect lv) (ycor-vect rv))))
(define (scale-vect scale v)
  (make-vect (* scale (xcor-vect v))
             (* scale (ycor-vect v))))

; exercise 2.47
(define (list-nth list n)
  (if (= n 0)
      (if (list? list)
          (car list)
          list)
      (list-nth (cdr list) (- n 1))))
(define (cons-origin-frame frame)
  (list-nth frame 0))
(define (cons-edge1-frame frame)
  (list-nth frame 1))
(define (cons-edge2-frame frame)
  (list-nth frame 2))

(define (list-origin-frame frame)
  (list-ref frame 0))
(define (list-edge1-frame frame)
  (list-ref frame 1))
(define (list-edge2-frame frame)
  (list-ref frame 2))

; Painters
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (segment-start segment))
        ((frame-coord-map frame) (segment-end segment))))
     segment-list)))

; exercise 2.48
(define (make-segment start-vect end-vect)
  (cons start-vect end-vect))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cadr segment))

; exercise 2.49
; a
(define (outline-of-frame frame)
  (segments->painter
   (list (make-segment (make-vect 0 0)
                       (make-vect 0 1))
         (make-segment (make-vect 0 1)
                       (make-vect 1 1))
         (make-segment (make-vect 1 1)
                       (make-vect 1 0))
         (make-segment (make-vect 1 0)
                       (make-vect 0 0)))))
; b
(define (X-frame frame)
  (segments->painter
   (list (make-segment (make-vect 0 0)
                       (make-vect 1 1))
         (make-segment (make-vect 0 1)
                       (make-vect 1 0)))))

; c
(define (diamond-of-frame frame)
  (segments->painter
   (list (make-segment (make-vect 0.5 0)
                       (make-vect 0 0.5))
         (make-segment (make-vect 0 0.5)
                       (make-vect 0.5 0.5))
         (make-segment (make-vect 0.5 0.5)
                       (make-vect 1 0.5))
         (make-segment (make-vect 1 0.5)
                       (make-vect 0.5 0)))))

; d
; 网上有通过估算得到的图形 这里就不画了

; its so-called origin argument really means origin point
; transform rule
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert-by-transform painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))
(define (rotate90-counter-clock painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.6 0.4)
                     (make-vect 0.4 0.6)))
(define (beside-by-transform painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

; exercise 2.50
(define (flip-horiz-by-transform painter)
  (transform-painter painter
                    (make-vect 1.0 0.0)
                    (make-vect 0.0 0.0)
                    (make-vect 1.0 1.0)))
(define (rotate180-counter-clock painter)
  (rotate90-counter-clock
   (rotate90-counter-clock painter)))
(define (rotate270-counter-clock painter)
  (rotate90-counter-clock
   (rotate90-counter-clock
    (rotate90-counter-clock painter))))

; exercise 2.51
(define (below-by-transform painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-below frame)
        (paint-top frame)))))
(define (below-by-beside painter1 painter2)
  (lambda (frame)
    ((flip-horiz
      (rotate90
       (beside
        (rotate270
         (flip-horiz painter1))
        (rotate270
         (flip-horiz painter2)))))
     frame)))

