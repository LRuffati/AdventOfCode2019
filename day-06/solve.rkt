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

( define (ancestors h name l) 
	 (let [[par (planet-parent (hash-ref h name))]]
	   (begin
	     (println (cons name par) )
	 (cond 
	   [(eq? par #f) l]
	   [else (ancestors h par (cons par l))]
	   ))))

( define (len-diff l1 l2)
	 (begin (println l1) (println l2)
	 (cond
	   [(string=? (car l1) (car l2)) (len-diff (cdr l1) (cdr l2))]
	   [else (+ (length l1) (length l2))]
	   )))

(begin
  (stream->list (sequence->stream (sequence-map (lambda (x) (apply insert-rel! x)) lis)))
  (println ht)
  (hash-map ht (lambda (k v) (cons k (planet-parent v))  )) 
#|
  (set-lvl! ht "COM" 0)
  (println levels)
  (print (foldl + 0 (hash-map levels * ))  )
|#
  (len-diff (ancestors ht "YOU" '() ) (ancestors ht "SAN" '()))
  )
