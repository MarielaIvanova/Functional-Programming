#lang racket
(define a(void))
(- a (- (+ (/ (+ 2 (/ 3 16)) (* 9 2.78)) (/ 5 2)) 6))

(define b(void))
(- b (/ (+ 15 21 (/ 3 15) (- 7 (* 2 2))) 16))

(define c(void))
(- c (/ (+ 5 (/ 1 4) (- 2 (- 3 (+ 6 (/ 1 5))))) (* 3 (- 6 2) (- 2 7))))

(define (square x)
  (* x x))

(define d (void))
(- d (/ (+ (pow 3 2) 5) (- (pow 3 3) 2)))

(define e (void))
(- e (+ (pow 16 4) (/ 95 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (squares-sum-bigger-two a b c)
  (cond
    ((and(a>b)(b>c)) (+ (* a a) (* b b)))
    ((and (a>b) (c>b))(+ (* a a) (* b b)))
    (else (+ (* b b) (* c c)))
   )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum-interval start end)
  (
   (if (start> end) (+ (sum-interval (+ start 1) end)))

   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count-digits number)
  (if (== number 0) 0)
  (if (> number 1)(+ 1 (count-digits number/10)))

 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reverse-digits number)
  (+ modulo(/ number 10) (reverse-digits (/ number 10)) )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (palindrome number)
  (if (== (reverse-digits number) number) ("Palindrome") ("No"))
  )