#lang racket

(define (time)
  (/ (current-inexact-milliseconds) 1000.0))

(define (donut x y z [rad 0.4] [thick 0.3])
  (let* ([xy-d (- (sqrt (+ (sqr x) (sqr y))) rad)]
         [d (sqrt (+ (sqr xy-d) (sqr z)))])
    (- d (/ thick 2))))

(define (normal sdf x y z [eps 0.001])
  (let* ([n-x (- (sdf (+ x eps) y z) (sdf (- x eps) y z))]
         [n-y (- (sdf x (+ y eps) z) (sdf x (- y eps) z))]
         [n-z (- (sdf x y (+ z eps)) (sdf x y (- z eps)))]
         [norm (sqrt (+ (sqr n-x) (sqr n-y) (sqr n-z)))])
    (values (/ n-x norm) (/ n-y norm) (/ n-z norm))))
    
(define (sample x y)
  (define (go z i steps)
    (if (>= i steps) #\space
    (let*-values ([(theta) (* 2 (time))]
                  [(t-x) (- (* x (cos theta)) (* z (sin theta)))]
                  [(t-z)(+ (* x (sin theta)) (* z (cos theta)))]
                  [(d) (donut t-x y t-z)]
                  [(nt-x nt-y nt-z) (normal donut t-x y t-z)]
                  [(is-lit?) (< nt-y -0.15)]
                  [(is-frosted?) (< nt-z -0.5)])
      (cond 
        [(<= d 0.01) (cond 
                       [(and is-frosted? is-lit?) #\@]
                       [is-frosted? #\#]
                       [is-lit? #\=]
                       [else #\.])]
        [else (go (+ z d) (add1 i) steps)]))))
  (go -10 0 30))

(let loop ()
  (define frame
    (for*/list ([y 20]
                [x 81])
      (if (= x 80) #\newline
      (let ([x-remap (sub1 (* (/ x 80) 2))]
            [y-remap (* (sub1 (* (/ y 20) 2)) (* 2 (/ 20 80)))])
        (sample x-remap y-remap)))))
  
  (display "\033[2J")
  (display (list->string frame))
  ; (sleep (/ 1 120))
  (loop))