(define (expt b n)
	(if (= n 0)
		1
		(* b (expt b (- n 1)))))

(define (expt-iter b n)
	(helper b n 1))
(define (helper b n count)
	(if (= n 0)
		count
		(helper b
			   (- n 1)
			   (* count b))))

(define (even? b)
	(= (remainder b 2) 0))
(define (square n) (* n n))
(define (fast-expt b n)
	(cond ((= n 0) 1)
		  ((even? n) (square (fast-expt b (/ n 2))))
		  (else (* b (fast-expt b (- n 1))))))