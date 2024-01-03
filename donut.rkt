#lang racket

(define (donut x y z [rad 0.4] [thick 0.3])
  (let* ([xy-d (- (sqrt (+ (sqr x) (sqr y))) rad)]
         [d (sqrt (+ (sqr xy-d) (sqr z)))])
    (- d (/ thick 2))))

(define (sample x y)
  (define (go z i steps)
    (if (>= i steps) #\space
    (let* ([theta (* 2 (/ (current-inexact-milliseconds) 1000.0))]
           [t-x (- (* x (cos theta)) (* z (sin theta)))]
           [t-z (+ (* x (sin theta)) (* z (cos theta)))]
           [d (donut t-x y t-z)])
      (cond [(<= d 0.01) #\#]
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
  (sleep (/ 1 60))
  (loop))