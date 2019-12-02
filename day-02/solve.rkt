#lang racket

( define in ( open-input-file "input" #:mode 'text ) )

( define lis ( map string->number 
				 (string-split 
				   (stream-first 
				     (sequence->stream 
				       (in-lines in))) ",")))

( define ( apply-op op pos-op v) 
	 (vector-set! 
	   v 
	   (vector-ref v (+ pos-op 3))
	   (op (vector-ref v (vector-ref v (+ pos-op 1))) 
	       (vector-ref v (vector-ref v (+ pos-op 2))))
	   ))


( let loop ([pos 0] [v (apply vector lis)] [nv 0])
	( cond 
		[(eq? ( vector-ref v pos) 99) 
		 	(cond 
			  [(eq? ( vector-ref v 0) 19690720) ( print nv )]
 			  [else ( let-values ([(a b) (quotient/remainder (+ 1 nv) 100 )]) 
				( begin
				  ( define vec ( apply vector lis ) )
				  ( vector-set! vec  1 a )
				  ( vector-set! vec 2 b )
				  ( loop 0 vec ( + 1 nv ) )
				  )
					     )])
		]
		[(eq? ( vector-ref v pos) 1) ( begin 
						  (apply-op + pos v)
						  ( loop ( + pos 4 ) v nv )
						 )]
	  	[(eq? ( vector-ref v pos) 2) ( begin 
						  (apply-op * pos v)
						  ( loop ( + pos 4 ) v nv)
						 )])
      )

