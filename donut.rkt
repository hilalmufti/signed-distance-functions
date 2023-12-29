#lang racket

(define (donut-2d x y [radius 0.4] [thickness 0.3])
  (- (abs (- (sqrt (+ (expt x 2) (expt y 2))) radius)) (/ thickness 2)))

(define (sphere x y z [radius 0.4])
  (- (sqrt (+ (expt x 2) (expt y 2) (expt z 2))) radius))

(define (circle x y [radius 0.4])
  (- (sqrt (+ (expt x 2) (expt y 2))) radius))

(define (sample x y [radius 0.4])
  (define (go z i steps)
    (if (>= i steps) #\space
    (let ([d (sphere x y z)])
      (cond [(<= d 0.01) #\#]
            [else (go (+ z d) (add1 i) steps)]))))
  (go -10 0 30))

(define (render-frame)
  (define frame-chars
    (flatten 
    (for/list ([y 20])
      (append
      (for/list ([x 80])
        (let ([x-remap (sub1 (* (/ x 80) 2))]
              [y-remap (* (sub1 (* (/ y 20) 2)) (* 2 (/ 20 80)))])
          (sample x-remap y-remap)))
      #\newline))))
    (display "\033[2J")
    (display (list->string frame-chars))
    (sleep (/ 1 30))
    (render-frame))

(render-frame)