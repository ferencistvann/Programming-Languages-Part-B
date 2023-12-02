
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))


(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (list-ref xs (remainder n (length xs)))]))


(define (stream-for-n-steps s n)
  (if (< n 1)
      null
      (let ([pair (s)])
        (cons (car pair) (stream-for-n-steps (cdr pair) (- n 1))))))


(define (funny-number-stream)
  (define (helper x)
    (cons (if (zero? (modulo x 5))
              (- x)
              x)
          (lambda () (helper (+ x 1)))))
  (helper 1))


(define (dan-then-dog) (cons "dan.jpg" (lambda () (cons "dog.jpg" dan-then-dog))))

;(define (a-s) (cons "a" a-s))

(define (stream-add-zero s)
  (define (helper s)
    (let ([pair (s)])
      (cons (cons 0 (car pair))
            (stream-add-zero (cdr pair)))))
  (lambda () (helper s)))


;(define xs (list 1 2 3))
;(define ys (list "a" "b"))

(define (cycle-lists xs ys)
  (define (helper x y)
    (cons (cons (list-nth-mod xs x)
                (list-nth-mod ys y))
          (lambda () (helper (+ x 1) (+ y 1)))))
  (lambda () (helper (length xs) (length ys))))


(define (vector-assoc v vec)
  (define (helper pos)
    (cond [(= pos (vector-length vec)) #f]
          [(equal? (car (vector-ref vec pos)) v) (vector-ref vec pos)]
          [#t (helper (+ pos 1))]))
  (helper 0))

;(define vec (vector (cons 1 2) (cons 3 4) (cons 5 6)))


#;
(define ones (lambda () (cons 1 ones)))
#;
(define (nats) ; 1 2 3 4 5 ...
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (f 1)))
#;
(define (modify-stream s)
  (define (helper s)
    (let ([pr (s)])
      (cons (+ 1 (car pr))
            (modify-stream (cdr pr)))))
  (lambda () (helper s)))
