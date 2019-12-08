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

(print (fun '(1000 . (0 . 0)) '(0 . (0 . 0)) 0 lis))
