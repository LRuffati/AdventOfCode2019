#lang racket

( define in ( open-input-file "input" #:mode 'text ) )

( define ( fuel_sim v ) ( - ( quotient v 3 ) 2 ) )

( define ( fuel tot xs )
        ( cond
           [(equal? xs '()) tot ]
           [(< ( car xs ) 9) ( fuel ( + ( car xs) tot ) ( cdr xs ) )]
           [else ( fuel ( + tot (car xs)) (cons (fuel_sim (car xs)) (cdr xs)))]
))


( print (
	sequence-fold 
		(lambda (t a) (+ t ( fuel_sim (string->number a)))) 
		0 
		(in-lines in)))
#|
( print ( fuel 0 ( map fuel_sim 
		       ( sequence->list ( sequence-map string->number ( in-lines in ) ) ) 
		 )))
|#
