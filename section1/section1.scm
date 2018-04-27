(declare (usual-integrations))

(define (print s a)
  (newline)(display s)(display a))

(define (square x) (* x x))

(square 21)
(square (+ 2 5))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)

(define (abs x)
        (cond ((< x 0) (- x))
              (else x)))

(define (abs x)
  (if (< x 0)
    (- x)
    x))

(abs 2)
(abs -9)

;; Exercise 1.3
(define (big-square a b c)
  (cond ((and (> a b) (> b c)) (sum-of-squares a b))
        ((and (< a b) (< a c)) (sum-of-squares b c))
        ((and (< b a) (> c b)) (sum-of-squares a c))))

(print "Big Square 2,3,4 = " (big-square 2 3 4))
(print "Big Square 4,1,6 = " (big-square 4 1 6))
(print "Big Square 5,3,0 = " (big-square 5 3 0))

(define (sqrt-iter guess previous x)
  (if (good-enough? guess previous)
    guess
    (sqrt-iter (improve guess x) guess
               x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))
;; Exercise 1.7
(define (good-enough? guess previous)
  (< (abs (-  guess previous)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 0 x))

(define (cube-iter guess previous x)
  (if (good-cube? guess previous)
    guess
    (cube-iter (improve-cube guess x) guess
               x)))

(define (good-cube? guess previous)
  (< (abs (- guess previous)) 0.001))

;; Exercise 1.8
(define (improve-cube guess x)
  (/ (+ (/ x
           (square guess))
        (* 2 guess))
     3))

(define (cbrt x)
  (cube-iter 1.0 0 x))

(print "Square root 9 = " (sqrt 9))
(print "Square root 16 = " (sqrt 16))
(print "Cube root of 8 = " (cbrt 8))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(print "Square root 25 = " (sqrt 25))

(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(print "4! = " (factorial 4))

(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product) (+ counter 1) max-count)))

(factorial 5)

;; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(newline)
(display(A 1 10))(newline)
(display(A 2 4))(newline)
(display(A 3 3))(newline)

(define (f n)
  (A 0 n))
(define (g n)
  (A 1 n))
(define (h n)
  (A 2 n))
(define (k n)
  (* 5 n n))

; Recursive Fibonacci
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(print "Fibonacci 10 = " (fib 10))

; Iterative Fibonacci
(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))

(print "Fibonacci 100 = " (fib 100))

;; Counting Change

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                      (- kinds-of-coins 1))
                  (cc (- amount
                         (first-denomination kinds-of-coins))
                      kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(print "Count change 100 = " (count-change 100))

;; Exercise 1.11

(define (f n)
  (cond ((< n 3) n)
        ((not (< n 3))
         (+
           (+
             (f (- n 1))
              (* 2 (f (- n 2))))
           (* 3 (f (- n 3)))))
        ))

(print "F(2) = " (f 2))
(print "F(3) = " (f 3))
(print "F(4) = " (f 4))
(print "F(5) = " (f 5))

(define (f-iter n count)
  (if (< n 3)
    n
    (+
      (+
        (f-iter (- count 1))
        (* 2 (f-iter (- count 2))))
      (* 3 (f-iter (- count 3))))))

(print "F(2) = " (f 2))
(print "F(3) = " (f 3))
(print "F(4) = " (f 4))
(print "F(5) = " (f 5))

;; Exercise 1.12

(define (pascal r c)
  (if (or (< r 3) (= c r) (= c 1)) 1
   (+ (pascal (- r 1) (- c 1))
      (pascal (- r 1) c))
   ))


(print "Pascal 1, 1 = " (pascal 1 1))
(print "Pascal 2, 2 = " (pascal 2 2))
(print "Pascal 3, 2 = " (pascal 3 2))
(print "Pascal 4, 3 = " (pascal 4 3))
(print "Pascal 5, 3 = " (pascal 5 3))
(print "Pascal 6, 3 = " (pascal 6 3))

(define (area shape r) (* shape r r))

(define circle 3.14159)

(print "Area of circle with radius 2 = " (area circle 2))

; Takes a function 'FN' as a parameter
(define (sum FN a b)
  (if (> a b)
    0
    (+ (FN a) (sum FN (+ a 1) b))))

(print "Sum of squares from 3 to 5 = " (sum square 3 5)) ;square is a function

;Exponentiation

;linear recursive
(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(print "3^3 = " (expt 3 3))

;linear iteration of expt
(define (expt-iter b counter product)
  (if (= counter 0)
    product
    (expt-iter b
               (- counter 1)
               (* b product))))

(define (expt b n)
  (expt-iter b n 1))

(print "30^30 = " (expt 30 30))

;fast expt
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b ( / n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(print "90^20 = " (fast-expt 90 20))

; Exercise 1.16
(define (expt-iterative b n a)
  (cond ((= n 0) a) ;answer given by a
        ((even? n) (expt-iterative (square b) (/ n 2) a))
        (else (expt-iterative b (- n 1) (* a b)))))

(define (expt b n)
  (expt-iterative b n 1))

(expt 90 900)
