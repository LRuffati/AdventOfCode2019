#lang racket

( define in ( open-input-file "input" #:mode 'text ) )

;; Every position now includes a counter beg = ( xy . pos  )
(define ( from/to beg mov) ( letrec [(t ( magnitude mov )) 
				      (s (/ mov t)) 
				      (xy (car beg))
				      (po (cdr beg))
				     ]
				     ( map 
				       (lambda (x) 
						(cons (+ xy (* x s)) (+ po x)))
				       (range 1 (+ t 1)))	
				     ))

( define ( convert str )
	 ( letrec [(ls ( string->list str ))
		   (k (car ls))
		   (n (string->number (list->string (cdr ls))))]
		  (* n (match k [#\R 1+0i] [#\U 0+i] [#\L -1+0i] [#\D 0-i])  )
		  )
	 )
( define lis (stream->list
	       (sequence->stream 
		 (sequence-map (lambda (x) (map convert (string-split x "," ) ) ) (in-lines in)))))

(define paths
        (map 
	  (lambda (x) (foldl 
			(lambda (t a) (foldl cons a (from/to (car a) t) )) 
	  		'((0 . 0))
			x)) 
	  lis))

( define ( manhattan a ) (+ (abs (imag-part a))(abs (real-part a))))

( define (comp a b)
	 ( let ([m1 (manhattan a)] [m2 (manhattan b)])
		(cond 
	      	      [(eq? m1 m2) (< (angle a) (angle b))]
		      [else (< m1 m2)]
	  	)
	 ))

( define sorted (map (lambda (x) (cdr (sort x (lambda (s1 s2) (comp (car s1) (car s2)))))) paths))

( define (first-match lis1 lis2) 
	 (let loop ([a lis1] [b lis2] [closest 0]) 
					(cond
					[(or (empty? a) (empty? b)) 
					 closest]
	   				[(eqv? (caar a) (caar b)) 
					 (begin (println (car a)) (println (car b))
						(println closest) (println "--")
					 (letrec ([sa (cdar a)] [sb (cdar b)])
					      (cond
						[(eqv? closest 0) 
						 (loop (cdr a) (cdr b) (+ sa sb))]
						[(< (+ sa sb) closest) 
						 (loop (cdr a) (cdr b) (+ sa sb))]
						[else (loop (cdr a) (cdr b) closest)])))
					 #|
					 (print (caar a))
					 |#
					]
	   				[(comp (caar a) (caar b)) 
					 (loop (cdr a) b       closest )]
	   				[(comp (caar b) (caar a))
					 (loop a       (cdr b) closest )]
					)))

(display (first-match (car sorted) (cadr sorted)))
