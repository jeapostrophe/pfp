#lang scheme
(require 2htdp/universe
         htdp/image
         "pfp.ss")

(define ship-rad 1)
(define speed 5)
(define bullet-rad (/ ship-rad 4))
(define screen-scale 5)
(define screen-width (* screen-scale 16))
(define screen-height (* screen-scale 9))

(define scale 10)
(define ship (circle (* ship-rad scale) 'solid "red"))
(define bullet (circle (* bullet-rad scale) 'solid "black"))

(define initial-bodies
  (list (make-body 'ship (vector 5 5) ship-rad (vector 0 0) #f)))

(define bullet-density 4)
(define fresh-bullets
  (for/list ([i (in-range (/ screen-width bullet-density))])
    (make-body 'bullet (vector (* bullet-density i) screen-height) bullet-rad (vector 0 (* -1 speed)) #f)))

(define (collide b1 b2)
  #t)

(define tick-rate 1/30)

(define-struct screen (time bodies))
(define initial-screen (make-screen 0 initial-bodies))
(define es (empty-scene (* scale screen-width) (* scale screen-height)))

(big-bang initial-screen
          (on-tick
           (match-lambda
             [(struct screen (time (? list? bodies)))
              (define-values (new-bodies collisions) (simulate collide bodies tick-rate))
              (make-screen (+ time tick-rate)
                           (if (ormap (lambda (x) x) collisions)
                               #f
                               (if (= time (round time))
                                   (append new-bodies fresh-bullets)
                                   new-bodies)))]
             [x x])
           tick-rate)
          (on-key
           (lambda (s key)
             (printf "~S~n" key)
             (match s
               [(struct screen (time (? list? bodies)))
                (match bodies
                  [(list-rest ship-b other-bodies)
                   (define new-vel
                     (match key
                       ["up" (vector 0 speed)]
                       ["down" (vector 0 (* -1 speed))]
                       ["right" (vector speed 0)]
                       ["left" (vector (* -1 speed) 0)]
                       [_ (body-vel ship-b)]))
                   (make-screen time
                                (list* (struct-copy body ship-b
                                                    [vel new-vel])
                                       other-bodies))])]
               [x x])))
          (on-draw
           (match-lambda
             [(struct screen (time (? list? bodies)))
              (for/fold ([s es])
                ([b (in-list bodies)])
                (match b
                  [(struct body (layer (vector x y) radius vel data))
                   (place-image (case layer
                                  [(ship) ship]
                                  [(bullet) bullet])
                                (* scale x)
                                (* scale (- screen-height y))
                                s)]))]
             [x es]))
          (stop-when
           (match-lambda
             [(struct screen (time bodies))
              (not bodies)])))
