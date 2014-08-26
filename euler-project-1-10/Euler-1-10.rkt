#lang racket
;helper methods
(define (dec x) (- x 1))
(define (inc x) (+ x 1))
(define (square x) (* x x))


(define (factor? a b) ;returns true if a is a factor of b
  (if (= (remainder b a) 0)
      #t
      #f))

(define (factors x)
  (define (factors-aux n lfact)
    (if (> n (sqrt x)) lfact
        (if (factor? n x) (cons (/ x n) (factors-aux (inc n) (cons n lfact)))
            (factors-aux (inc n) lfact))))
  (factors-aux 2 '())
  )
       
(define (prime? x)
  (define (prime-aux n)
    (if (> n (sqrt x)) #t
        (if (factor? n x)
            #f
            (prime-aux (inc n)))))
  (prime-aux 2))


(define (palindrome? x)
  (cond [(number? x) (palindrome? (number->string x))]
        [(<= (string-length x) 1) #t]
        [(equal? (substring x 0 1) (substring x (dec (string-length x)))) (palindrome? (substring x 1 (dec (string-length x))))]
        [else #f]))


(define (andApply lBoo)
  (if (not (car lBoo)) #f
      (if (= 1(length lBoo)) (car lBoo)
          (andApply (cdr lBoo)))))

;solutions
(define (e1 n x) 
  (if (= 0 n) x 
      (if (or (= (modulo n 5) 0) (= (modulo n 3) 0)) (e1 (dec n) (cons n x))
          (e1 (dec n) x))))


(define (e2 a b n x)
  (if (> b n) x
   (if (= 0 (modulo b 2)) (e2 b (+ a b) n (cons b x))
       (e2 b (+ a b) n x))))

(define (e3 x) 
  (first (filter prime? (factors x))))

(define (e4 low high)
  (apply max 
         (filter palindrome? 
                 (flatten
                 (map (lambda (x) (map (lambda (y) (* y x)) (range low high))) (range low high))))))


(define (e5 n low high)
  (if  (andApply (map (lambda (x) (= 0 (remainder n x))) (range low (inc high)))) n
      (e5 (+ high n) low high)))

(define (e6 high)
  (- (square (apply + (range (inc high)))) (apply + (map square (range (inc high))))))

(define (e7 primes target current)
  (if (= primes target) (dec current)
      (if (prime? current) (e7 (inc primes) target (inc current))
          (e7 primes target (inc current)))))
;solved this one by hand
(define e8 (* 5 5 7 6 6 8 9 6 6 4 8 9 5))



