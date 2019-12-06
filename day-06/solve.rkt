#lang racket
(require racket/set)

(struct planet ( 
		 name
		 (parent #:mutable)
		 (moons  #:mutable)
		 ))

(define levels (make-hash))
(define ht (make-hash))

(define (insert-rel! star moon) ( 
				 let
				    [ [s (hash-ref ht star 'fail)]
				      [m (hash-ref ht moon 'fail)]
				    ]
				    (cond 
				      [(eq? s 'fail) (hash-set! ht star 
								(planet 
								   star
								   #f
								   (list moon)))]
				      [else (let [[o-moons (planet-moons s)]]
						 (set-planet-moons! s (cons moon o-moons)))]
				     )
				     (cond 
				       [(eq? m 'fail) (hash-set! ht moon 
								 (planet
								    moon
								    star
								    '()))]
				       [(not (not (planet-parent m)))
					  (raise `(two-stars ,moon ,star))]
				       [else (set-planet-parent! m star)]
				       )
				    ))

(define (increase! h k)
        (let ([v (hash-ref h k 0)])
	     (hash-set! h k (+ v 1))
	  ))

(define (set-lvl! h node l )
        (begin 
	  (increase! levels l)
	  (let [(p (hash-ref h node))]
	    (cond 
	    [(empty? (planet-moons p)) 'e]
	    [else (map (lambda (id) (set-lvl! h id (+ 1 l))) (planet-moons p))]
	)))
  )

(define in (open-input-file "input" #:mode 'text))

(define lis (sequence-map (lambda (s) (string-split s ")")) (in-lines in)))

(begin
  (stream->list (sequence->stream (sequence-map (lambda (x) (apply insert-rel! x)) lis)))
  (print ht)
  (set-lvl! ht "COM" 0)
  (println levels)
  (print (foldl + 0 (hash-map levels * ))  )
  )
