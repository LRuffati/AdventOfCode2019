#lang racket

(define inp-v '())
(define (inp-instr) (let ([i (car inp-v)]) (set! inp-v (cdr inp-v)) i))

( define outs '() )

(define ( out-instr val)  (set! outs (cons val outs)))

(define ( lt val1 val2  )  (cond [(< val1 val2) 1] [else 0] ) )
(define ( eq val1 val2) (cond [(eqv? val1 val2) 1] [else 0] ) )
(define (jit p t j) (cond [(eq? 0 t) (+ p 3)]
			  [else j]) )
(define (jif p t j) (cond [(eq? 0 t) j]
			  [else (+ p 3)]) )
(define vec (vector 4 3 -1 4 0))

(define instructions #hash(
			    (1 . (+         . (2 . 1)))
			    (2 . (* 	    . (2 . 1)))
			    (3 . (inp-instr . (0 . 1)))
			    (4 . (out-instr . (1 . 0)))
			    (5 . (jit       . (2 . 0)))
			    (6 . (jif       . (2 . 0)))
			    (7 . (lt        . (2 . 1)))
			    (8 . (eq        . (2 . 1)))
			    ))

(define (gen-args code pos rim )  (let [[this (remainder code 10  )]
				 [next (quotient  code 10 )]] (cond 
			     [(zero? rim) '()]
			     [(zero? this) `((vector-ref v (vector-ref v ,pos)) 
						. 
					     ,(gen-args next `(+ ,pos 1) (- rim 1)))]
			     [else `((vector-ref v ,pos) 
				     . ,(gen-args next `(+ 1 ,pos) (- rim 1)))]
			     )))

(define-namespace-anchor ns)

( define (eval-instr v p-sym p expr) (eval `(let [[v ,v][,p-sym ,p]] (list . ,expr )) 
					   (namespace-anchor->namespace ns)) )

(define (interpr op-code) (let* (
				    [op-num (remainder op-code 100)]
				    [op (dict-ref instructions op-num)]
				    [fun (eval (car op) (namespace-anchor->namespace ns))]
				    [n-tot (+ (cadr op) (cddr op))]
				    [args (gen-args (quotient op-code 100) 'p (cadr op))]

				) 
			(cond 
			   [(and (< 4 op-num) (> 7 op-num)) 
				(lambda (v p) (apply fun (cons p (eval-instr v 'p (+ 1 p) args))))
			    ]
			   [(eq? (cddr op) 0)
			    (lambda (v p) ; pass the vector and the position to the result
			          	(apply fun (eval-instr v 'p (+ 1 p) args))
					(+ n-tot 1 p)
					)]

			   [else
			    (lambda (v p)
				(vector-set! v (vector-ref v (+ p n-tot)) 
					       (apply fun (eval-instr v 'p (+ 1 p) args)))
				(+ n-tot 1 p)
			    )]
			    )))
(define in ( open-input-file "input" #:mode 'text ) )

(define lis ( map string->number 
				 (string-split 
				   (stream-first 
				     (sequence->stream 
				       (in-lines in))) ",")))

(define (next-perm i) (let* ([l (range 5)] 
			     [v (map (lambda (x) (remainder (quotient i (expt 5 x)) 5)) l)]
			     [r (check-duplicates v)])
			    (cond
			      [(eq? r #f) i]
			      [else (next-perm (+ i 1))])
		) )

(define (run-machine)
	(let loop ([pos 0] [v (apply vector lis)])
		(cond 
			[(eq? ( vector-ref v pos) 99) (let ([o (car outs)]) (set! outs '()) o)] 

			[else (loop ((interpr (vector-ref v pos)) v pos) v)])
	      ))

(define max-o 0)

(let box-loop ([box 0] [it (next-perm 0)] [last-out 0])
  	(let ([cmd (remainder (quotient it (expt 5 box)) 5)])
		(set! inp-v (list cmd last-out)))
	(cond 
	   [(eq? box 5) (set! max-o (max max-o last-out))
			(print max-o) (print " -- ") (print it) (print " --- ")
			(cond 
			  [(> it (- (expt 5 5) 1)) max-o]
			  [ else (println last-out) (box-loop 0 (next-perm (+ 1 it)) 0)]
			  )]
	   [else (box-loop (+ 1 box) it (run-machine))]
	  )

)
