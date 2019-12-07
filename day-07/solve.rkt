#lang racket


(define (inp-instr) (thread-receive))

(define (out-instr o m)  (thread-send o m))

(define (lt val1 val2) (cond [(< val1 val2) 1] 
			     [else 0] ) )

(define (eq val1 val2) (cond [(eqv? val1 val2) 1] 
			     [else 0] ) )

(define (jit p t j)    (cond [(eq? 0 t) (+ p 3)]
			     [else      j]))

(define (jif p t j)    (cond [(eq? 0 t) j]
			     [else      (+ p 3)]))

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
				(lambda (o v p) 
				        (apply fun 
					       (cons p (eval-instr v 'p (+ 1 p) args))
					       ))]
			   [(eq? 4 op-num)
			    (lambda (o v p) ; pass the vector and the position to the result
			          	(apply fun (cons o (eval-instr v 'p (+ 1 p) args)))
					(+ n-tot 1 p)
					)]

			   [else
			    (lambda (o v p)
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

(define (run-machine o) ; o is the thread to which I should direct the output
	(let loop ([pos 0] [v (apply vector lis)])
		(cond 
			[(eq? (vector-ref v pos) 99) (thread-send o 'end)
						     ] 

			[else (loop ((interpr (vector-ref v pos)) o v pos) v)])
	      ))

(define (start-thread o conf)
  	(let ([t 
		(thread 
		  (lambda ()
		          (run-machine o)
			  (thread-receive)
			  ))])
	     (thread-send t conf)
	     t
	     ))

(define (thread-out call-thread) (thread (lambda () 
			             (let ([t (thread-receive)])
				          (let loop ([m (thread-receive)]
						     [l '()])
					            (cond
						      [(eq? m 'end) 
						       (thread-send call-thread (car l))]
						      [else (thread-send t m)
							    (loop (thread-receive) (cons m l))]
						      )
				     )))))

(define (start-experiment codes input) (let* ([ot (thread-out (current-thread))]
					      [ts (foldr 
						    (lambda 
						      (e prev) 
						      (sleep 0.04)
						      (cons (start-thread (car prev) (+ 5 e)) prev)
						      )
						    (list ot)
						    codes)]
					      )
					      (thread-send ot (car ts))
					      (thread-send (car ts) input)
					      (thread-receive)
					 ))

(let loop ([it (next-perm 0)] [max-o 0])
  	(let* ([l (range 4 -1 -1)]
	       [v (map (lambda (x) (remainder (quotient it (expt 5 x)) 5)) l)])
	      (println max-o)
	      (cond 
		[(> it (expt 5 5)) max-o]
		[else (loop (next-perm (+ 1 it)) (max max-o (start-experiment v 0)) )]
		)))

