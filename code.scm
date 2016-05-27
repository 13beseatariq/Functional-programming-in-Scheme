(define (TRUE a b) a)
(define (FALSE a b) b)
(define (ZERO s z) z)

(define (ISZERO n)
	(if (zero? n) TRUE FALSE)
)
	 

(define (OR M N)         
	(M TRUE N)
)

(define (AND M N)         
    (M N FALSE)
)

(define (NOT M)         
	(M FALSE TRUE)
)

(define (PRED n z)
	  (define (iter n z)
		(if (zero? z)
			z
			(if (zero? n)
				z
				(iter (- n 1) (- z 1)))))
	  (iter n z))
	 
(define SUB(lambda (a b)         
		(PRED b a) ))
	
(define (LT a b)       
		(AND (ISZERO (SUB a b)) (NOT ( ISZERO (SUB b a) )))
)


(define (IFTHENELSE predicate ifbranch elsebranch)
	  (predicate ifbranch elsebranch) 
)


(define RECADD
    (lambda (func-arg)
      (lambda (n)
        (if (zero? n)
            0
            (+ n (func-arg (- n 1)))))))
           
(define Y
    (lambda (X)
      ((lambda (procedure)
         (X (lambda (arg) ((procedure procedure) arg))))
       (lambda (procedure)
         (X (lambda (arg) ((procedure procedure) arg)))))))

(define SUM (Y RECADD))

(define (aCompletelyUselessFunctionCreatedOnlyToMakeYourLifeMiserable m n)
	(
		IFTHENELSE (OR (ISZERO m) (LT m n)) 
		 (SUM n)
		 (+ n m)
	)
)

(aCompletelyUselessFunctionCreatedOnlyToMakeYourLifeMiserable 2 1)
