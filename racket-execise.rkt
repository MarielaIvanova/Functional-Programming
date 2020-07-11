#lang racket
(require rackunit rackunit/text-ui)

;1
(define (times n) (lambda (x) (* x n)))
(define (plus n) (lambda (x) (+ x n)))
(define (minus n) (lambda (x) (- x n)))
(define (div n) (lambda (x) (quotient x n)))

(define (one (op ""))
  (if (procedure? op) (op(one))
      1))
(define (two (op ""))
  (if (procedure? op) (op(two))
      2))
(define (three (op ""))
  (if (procedure? op) (op(three))
      3))
(define (five (op ""))
  (if (procedure? op) (op(five))
      5))
(define (four (op ""))
  (if (procedure? op) (op(four))
      4))
(define (six (op ""))
  (if (procedure? op) (op(six))
      6))
(define (seven (op ""))
  (if (procedure? op) (op(seven))
      7))
(define (eight (op ""))
  (if (procedure? op) (op(eight))
      8))
(define (nine (op ""))
  (if (procedure? op) (op(nine))
      9))

(define plus-tests
  (test-suite "Plus test"
              (test-case "1+1" (check-equal? (one(plus(one))) 2))
              (test-case "1+2+...+9" (check-equal? (one(plus(two(plus(three(plus(four(plus(five(plus(six(plus(seven(plus(eight(plus(nine))))))))))))))))) 45))))
(run-tests plus-tests 'verbose)

(define minus-tests
  (test-suite "Minus test"
              (test-case "1-1" (check-equal? (one(minus(one))) 0))
              (test-case "5-2-1" (check-equal? (five(minus(two(minus(one))))) 4))
              (test-case "4-5" (check-equal? (four(minus(five))) -1))))              
(run-tests minus-tests 'verbose)

(define times-tests
  (test-suite "Mult test"
              (test-case "1*1" (check-equal? (one(times(one))) 1))
              (test-case "5*2*1" (check-equal? (five(times(two(times(one))))) 10))
              (test-case "4*(5-2)" (check-equal? (four(times(five(minus(two))))) 12))))              
(run-tests times-tests 'verbose)

(define div-tests
  (test-suite "Div test"
              (test-case "1/1" (check-equal? (one(div(one))) 1))
              (test-case "8/2" (check-equal? (eight(div(two))) 4))
              (test-case "9/3/3" (check-equal? (nine(div(three(div(three))))) 9))))              
(run-tests div-tests 'verbose)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;2

(define (reverse xs)
  (if (null? xs) '()
      (append (reverse (cdr xs)) (list (car xs))))
  )

;;;
(define (prefixes xs)
  (define (helper xs)
    (if (null? xs)'()
        (cons xs (helper (reverse (cdr (reverse xs)))))))(cons '() (reverse (helper xs))))

;;;
(define reverse-tests
  (test-suite "Reverse funx test"
              (test-case "Empty" (check-equal? (reverse '()) '()))
              (test-case "Random" (check-equal? (reverse '(1 2 3)) '(3 2 1)))
              (test-case "Deep list" (check-equal? (reverse '(1 (2 3) (4))) '((4) (2 3) 1)))))
(run-tests reverse-tests 'verbose)


(define prefixes-tests
  (test-suite "Test func prefixes"
              (test-case "Empty" (check-equal? (prefixes '()) '(())))
              (test-case "Random" (check-equal? (prefixes '(2 5 6)) '(() (2) (2 5) (2 5 6))))
              (test-case "Deep list" (check-equal? (prefixes '(1 (2 3) (4) 5)) '(() (1) (1 (2 3)) (1 (2 3) (4)) (1 (2 3) (4) 5)))) ))
(run-tests prefixes-tests 'verbose)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;3

;1
(define (day xs) (car xs))

(define (month xs) (car (cdr xs)))

(define (year xs) (car(cdr(cdr xs))))

;2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (leapYear year)
 (if (or(and (= 0 (remainder year 4)) (not (= 0 (remainder year 100)))) (= 0 (remainder year 400))) #t
     #f
  ))

(define (date-is-legit xs)
  (if (< 2020 (year xs)) #t
      #f)
  (if (and (< 13 (month xs)) (> 0 (month xs))) #t
      #f)
  (if (> (day xs) 0)
      (cond
        ((and (or (= (month xs) 1) (= (month xs) 3)(= (month xs) 5) (= (month xs) 7)(= (month xs) 8) (= (month xs) 10)(= (month xs) 12))(< (day xs) 32)) #t)
        ((and (or (= (month xs) 4) (= (month xs) 6)(= (month xs) 9) (= (month xs) 11))(< (day xs) 31)) #t)
        ((and (=(month xs) 2) (not(leapYear (year xs))) (< (day xs) 29)) #t) 
        ((and (= (month xs) 2) (leapYear (year xs)) (< (day xs) 30)) #t)
        (else #f))
      #f))

(define date-is-legit-tests
  (test-suite "Legit date test"
              (test-case "Random date: 12.12.2019" (check-equal? (date-is-legit '(12 12 2019)) #t))
              (test-case "32.12.2019" (check-equal? (date-is-legit '(32 13 2019)) #f))
              (test-case "29.02.2016" (check-equal? (date-is-legit '(29 02 2016)) #t))
              (test-case "29.02.2019" (check-equal? (date-is-legit '(29 02 2019)) #f))
              (test-case "31.11.1975" (check-equal? (date-is-legit '(31 11 1975)) #f))
              (test-case "07.13.2019" (check-equal? (date-is-legit '(07 13 2019)) #f))))
(run-tests date-is-legit-tests 'verbose)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-date day month year)
  (list day month year))


;;;
(define make-date-tests
  (test-suite "Make-date test"
              (test-case "01.01.2011" (check-equal? (make-date 01 01 2011) '(01 01 2011)))
              (test-case "21.11.1975" (check-equal? (make-date 21 11 1975) '(21 11 1975)))))
(run-tests make-date-tests 'verbose)


;;;
(define (date? xs)
  (date-is-legit xs))

;;;3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (date->string xs)
 (string-append (number->string (day xs)) "."
                (number->string (month xs)) "."
                (number->string (year xs))))

(define date->string-tests
  (test-suite "Date->string test"
              (test-case "1.1.2011" (check-equal? (date->string '(01 01 2011)) "1.1.2011"))
              (test-case "21.11.1975" (check-equal? (date->string '(21 11 1975)) "21.11.1975"))))
(run-tests date->string-tests 'verbose)


;4;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (next-day xs)
  (cond ((and (or (= (month xs) 1) (= (month xs) 3)
         (= (month xs) 5) (= (month xs) 7)
         (= (month xs) 8) (= (month xs) 10)
         (= (month xs) 12)) (> 31 (day xs)))
                                            (make-date (+ 1 (day xs)) (month xs) (year xs)))
        ((and (or (= (month xs) 4) (= (month xs) 6)
         (= (month xs) 9) (= (month xs) 11)) (> 30 (day xs)))
                                                   (make-date (+ 1 (day xs)) (month xs) (year xs)))
        ((and(or (= (month xs) 1) (= (month xs) 3)
         (= (month xs) 5) (= (month xs) 7)
         (= (month xs) 8) (= (month xs) 10)) (= 31 (day xs)))
                                                  (make-date 1 (+ 1 (month xs)) (year xs)))
        ((and (or (= (month xs) 4) (= (month xs) 6)
         (= (month xs) 9) (= (month xs) 11)) (= (day xs) 30))
                                                  (make-date 1 (+ 1 (month xs)) (year xs)))  
        ((and (= (month xs) 12) (= (day xs) 31))  (make-date 1 1 (+ 1 (year xs))))
        ((and (= (month xs) 2) (leapYear (year xs)) (< (day xs) 29)) (make-date (+ 1 (day xs)) (month xs) (year xs)))
        ((and (= (month xs) 2) (leapYear(year xs)) (= (day xs) 29)) (make-date 1 (+ 1 (month xs)) (year xs)))
        ((and (=(month xs) 2) (not(leapYear (year xs))) (< (day xs) 28)) (make-date (+ 1 (day xs)) (month xs) (year xs)))
        ((and (=(month xs) 2) (not(leapYear (year xs))) (= (day xs) 28)) (make-date 1 (+ 1 (month xs)) (year xs)))  ) )

(define next-day-tests
  (test-suite "next-day test"
              (test-case "31.1.2011" (check-equal? (next-day '(31 1 2011)) '(1 2 2011) ))
              (test-case "31.12.1975" (check-equal? (next-day '(31 12 1975)) '(1 1 1976) ))
              (test-case "29.02.2016" (check-equal? (next-day '(29 2 2016)) '(1 3 2016) ))
              (test-case "28.02.1975" (check-equal? (next-day '(28 02 1975)) '(1 3 1975) ))
              (test-case "28.02.1976" (check-equal? (next-day '(28 2 1976)) '(29 2 1976) ))
              (test-case "30.11.1453" (check-equal? (next-day '(30 11 1453)) '(1 12 1453) ))
              (test-case "04.02.1111" (check-equal? (next-day '(3 2 1711)) '(4 2 1711) ))
              ))
(run-tests next-day-tests 'verbose)


;5;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (date< first second)
  (cond
    ((< (year first) (year second)) #true)
    ((> (year first) (year second)) #false)
    ((< (month first) (month second)) #true)
    ((> (month first) (month second)) #false)
    ((< (day first) (day second)) #true)
    ((> (day first) (day second)) #false)
    (else #false)))

(define date<-tests
  (test-suite "Date< test"
              (test-case "01.01.2011 and 02.01.2011" (check-equal? (date< '(01 01 2011) '(02 01 2011)) #t))
              (test-case "02.02.2011 and 01.03.2011" (check-equal? (date< '(02 02 2011) '(01 03 2011)) #t))
              (test-case "02.02.2011 and 01.01.2012" (check-equal? (date< '(02 02 2011) '(01 01 2012)) #t))
              (test-case "02.02.2012 and 01.03.2011" (check-equal? (date< '(02 02 2012) '(01 03 2011)) #f))
              (test-case "02.05.2011 and 01.03.2011" (check-equal? (date< '(02 05 2011) '(01 03 2011)) #f))
              (test-case "03.02.2011 and 02.02.2011" (check-equal? (date< '(03 02 2011) '(02 02 2011)) #f))
              ))
(run-tests date<-tests 'verbose)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;6
(define (K date) (remainder (year date) 100))

(define (formulaMonth m y)
  (cond ((or (= m 4) (= m 7) (and (= m 1) (leapYear y))) 0)
        ((or (and (= m 2) (leapYear y)) (= m 8)) 3)
        ((or (= m 1) (= m 10)) 1)
        ((or (= m 2) (= m 11) (= m 3)) 4)
        ((= m 5) 2)
        ((= m 6) 5)
        ((or (= m 9) (= m 12)) 6)))

(define (century year)
  (if (> year 1700)
      (cond ((= (remainder (quotient year 100) 4) 0) 6)
            ((= (remainder (quotient year 100) 4) 1) 4)
            ((= (remainder (quotient year 100) 4) 2) 2)
            (else 0))
      (remainder (- 18 (quotient year 100)) 7)))


(define (dayCode dayNum)
     (cond ((= dayNum 2) "Monday")
           ((= dayNum 3) "Tuesday")
           ((= dayNum 4) "Wednesday") 
           ((= dayNum 5) "Thursday") 
           ((= dayNum 6) "Friday")
           ((= dayNum 0) "Saturday")
           ((= dayNum 1) "Sunday")))

(define (weekday date)
  (define dayNum (remainder (+ (formulaMonth (month date) (year date)) (quotient (K date) 4) (K date) (day date) (century (year date))) 7))
  (dayCode dayNum))

(define weekday-tests
  (test-suite "Next-weekday test"
              (test-case "06.12.2019" (check-equal? (weekday '(06 3 2002)) "Wednesday"))
              (test-case "06.12.2019" (check-equal? (weekday '(29 02 1884)) "Friday"))
              (test-case "06.12.2019" (check-equal? (weekday '(9 12 2019)) "Monday"))
              (test-case "26.06.2019" (check-equal? (weekday '(26 6 1999)) "Saturday"))
              (test-case "26.06.1999" (check-equal? (weekday '(28 07 1999)) "Wednesday"))
              (test-case "31.01.1784" (check-equal? (weekday '(31 01 1784)) "Saturday"))
              (test-case "01.02.1784" (check-equal? (weekday '(1 2 1784)) "Sunday"))
              (test-case "31.12.2019" (check-equal? (weekday '(31 12 2019)) "Tuesday"))
              ))
(run-tests weekday-tests 'verbose)


;7;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (next-weekday wday d)
  (if (equal? (weekday (next-day d)) wday) (next-day d)
      (next-weekday wday (next-day d))))

(define next-weekday-tests
  (test-suite "Next-weekday test"
              (test-case "06.12.2019 FR" (check-equal? (next-weekday "Friday" '(06 12 2019)) '(13 12 2019)))
              (test-case "06.12.2019 MON" (check-equal? (next-weekday "Monday" '(06 12 2019)) '(9 12 2019)))
              (test-case "06.12.2019 Wedn" (check-equal? (next-weekday "Wednesday" '(06 12 2019)) '(11 12 2019)))
              (test-case "26.06.2019 Sun" (check-equal? (next-weekday "Sunday" '(26 6 1999)) '(27 6 1999)))
              (test-case "26.06.1999 Sun" (check-equal? (next-weekday "Sunday" '(26 06 1999)) '(27 06 1999)))
              (test-case "31.01.1784 Sat" (check-equal? (next-weekday "Saturday" '(31 01 1784)) '(7 2 1784)))
              (test-case "31.12.2019 Thu" (check-equal? (next-weekday "Thursday" '(31 12 2019)) '(2 1 2020)))
              ))
(run-tests next-weekday-tests 'verbose)


;8;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (listOfEvents)
  (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
        (cons (make-date 27 11 2019) "Спират водата в Младост")
        (cons (make-date 28 11 2019) "Спират водата в Лозенец")))


(define (events-for-day date listOfEvents)
  (cond ((null? listOfEvents) listOfEvents)
        ((equal? date (caar listOfEvents)) (cons (car listOfEvents) (events-for-day date (cdr listOfEvents))))
        (else (events-for-day date (cdr listOfEvents)))))

;9;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (calendar l)
  (define (helper newList l)
  (cond ((null? l) newList)
        ((equal? (caar l) (car newList)) (helper (cons (append (list(car newList)) (list(cdr(car l)))) (cdr newList)) (cdr l)))
        (else (helper (cons (car l) newList) (cdr l)) ) )) (helper (car(sortList l)) l ))

(define (sort< event1 event2)
  (date< (car event1) (car event2)))

(define (smallest l)
  (cond ((null? (cdr l)) (car l))
        ((sort< (car l) (smallest (cdr l))) (car l))
        (else (smallest (cdr l)))
  ))

(define (deleteEl el l)
  (cond ((null? l) '())
        ((equal? (car l) el) (deleteEl el (cdr l)))
        (else  (cons (car l) (deleteEl el (cdr l))))))

(define (sortList l)
  (cond ((null? l) '())
        (else (define smallhelper (smallest l))
              (cons (car smallhelper)
                    (sortList (deleteEl smallhelper l))))))































