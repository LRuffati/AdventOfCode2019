#lang racket


(define in ( open-input-file "input" #:mode 'text ) )

(define lis ( map (lambda (x) (string->number (string x)))
		  (string->list  (stream-first 
				     (sequence->stream 
				       (in-lines in))))))
(define (fun best f_r f_c l) (begin (println best)
		       (println f_r)
		       (println '" ")
        (cond 
	  [(eq? f_c 150) 
		       (cond 
			   [(< (car f_r) (car best)) (fun f_r (cons 0 (cons 0 0)) 0 l)]
			   [else (fun best (cons 0 (cons 0 0)) 0 l)])]
	  [(empty? l) (* (cadr best) (cddr best))] 
	  [(eq? 0 (car l)) (fun best (cons (+ 1 (car f_r)) (cdr f_r)) (+ f_c 1) (cdr l))]
	  [(eq? 1 (car l)) 
	   (fun best (cons (car f_r) 
			   (cons (+ 1 (cadr f_r)) 
				 (cddr f_r))) (+ 1 f_c) (cdr l))]
	  [(eq? 2 (car l)) 
	   (fun best (cons (car f_r) 
			   (cons (cadr f_r)
				 (+ 1 (cddr f_r)))) (+ 1 f_c) (cdr l))]
	  [else (fun best f_r ( + 1 f_c) (cdr l))]

	))
)

; fun2 vector pos lis
(define (fun2 vec p lis) 
        (cond
	  [(empty? lis) vec]
	  [(eq? p 150) (fun2 vec 0 lis)]
	  [(not (eq? (vector-ref vec p) 2)) (fun2 vec (+ 1 p) (cdr lis))]
	  [else (vector-set! vec p (car lis)) (fun2 vec (+ 1 p) (cdr lis))]
	  ))

(define (pp lis col)(cond 
		      [(empty? lis) (display "\n")]
		      [(eq? col 25) (display "\n") (pp lis 0)]
		      [(eq? 1 (car lis)) (display "█") (pp (cdr lis) (+ 1 col))]
		      [else (display " ") (pp (cdr lis) (+ 1 col))]
		      ))

(pp (vector->list (fun2 (apply vector (take lis 150)) 0 (drop lis 150))) 0)
