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

(newline)
(display (big-square 2 3 4))
(newline) (display (big-square 4 1 6))
(newline) (display (big-square 5 3 0))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(newline) (display (sqrt 9))
