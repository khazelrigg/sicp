(declare (usual-integrations))

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

(define (big-square a b c)
  (cond ((and (> a b) (> b c)) (sum-of-squares a b))
        ((and (< a b) (< a c)) (sum-of-squares b c))
        ((and (< b a) (> c b)) (sum-of-squares a c))))

(newline) (display "Big square of 2, 3, 4 = ") (display (big-square 2 3 4))l
(newline) (display "Big square of 4, 1, 6 = ") (display (big-square 4 1 6))
(newline) (display "Big square of 5, 3, 0 = ") (display (big-square 5 3 0))

(define (sqrt-iter guess previous x)
  (if (good-enough? guess previous)
    guess
    (sqrt-iter (improve guess x) guess
               x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

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

(define (improve-cube guess x)
  (/ (+ (/ x
           (square guess))
        (* 2 guess))
     3))

(define (cbrt x)
  (cube-iter 1.0 0 x))

(newline) (display "Square root of 9 = ") (display (sqrt 9))
(newline) (display "Square root of 16 = ") (display (sqrt 16))
(newline) (display "Cube root of 8 = ") (display (cbrt 8))

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

(newline) (display "Square root of 25 = ") (display (sqrt 25))

(define (factorial n)
  (if (= n 1) 
    1
    (* n (factorial (- n 1)))))

(newline) (display "4! = ") (display (factorial 4))

(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product) (+ counter 1) max-count)))

(factorial 5)

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

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(display (fib 20)) (newline)

(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))

(fib 1000)

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

(count-change 100)


