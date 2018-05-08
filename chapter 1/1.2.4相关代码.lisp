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

