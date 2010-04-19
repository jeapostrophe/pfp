#lang scheme
(require (planet wmfarr/simple-matrix:1:1/matrix))

(define (posn-distance p1 p2)
  (sqrt (+ (sqr (- (vector-ref p1 0) (vector-ref p2 0)))
           (sqr (- (vector-ref p1 1) (vector-ref p2 1))))))

(define-struct body (layer posn radius vel data) #:transparent)

(define (body-step dx)
  (match-lambda
    [(and b (struct* body ([posn posn] [vel vel])))
     (struct-copy body b
                  [posn (vector-add posn (vector-scale vel dx))])]))

(define (bodies-overlap? b1 b2)
  (define p1 (body-posn b1))
  (define p2 (body-posn b2))
  (define d (posn-distance p1 p2))
  (or (< d (body-radius b1))
      (< d (body-radius b2))))

(define (n2-collisions collide bodies)
  (define layer-hash (make-hash))
  (define seen?-hash (make-hash))
  (for ([b (in-list bodies)])
    (hash-update! layer-hash (body-layer b)
                  (curry list* b)
                  empty))
  (for/fold ([l empty])
    ([(b1-layer fbodies) (in-hash layer-hash)])
    (hash-set! seen?-hash b1-layer #t)
    (for/fold ([l l])
      ([b1 (in-list fbodies)])
      (for*/fold ([l l])
        ([(b2-layer lbodies) (in-hash layer-hash)]
         #:when (not (hash-has-key? seen?-hash b2-layer))
         [b2 (in-list lbodies)]
         #:when (bodies-overlap? b1 b2))
        (list* (collide b1 b2) l)))))

(define (vector-max v1 v2)
  (vector (max (vector-ref v1 0) (vector-ref v2 0))
          (max (vector-ref v1 1) (vector-ref v2 1))))
(define (vector-min v1 v2)
  (vector (min (vector-ref v1 0) (vector-ref v2 0))
          (min (vector-ref v1 1) (vector-ref v2 1))))

(define (hash-collisions collide bodies)
  (define-values
    (how-many max-p min-p)
    (for/fold ([how-many 0]
               [max-p (vector -inf.0 -inf.0)]
               [min-p (vector +inf.0 +inf.0)])
      ([b (in-list bodies)])
      (define bp (body-posn b))
      (values (add1 how-many)
              (vector-max max-p bp)
              (vector-min min-p bp))))
  (if (zero? how-many)
      empty
      (local [(define size-p
                (vector-sub max-p min-p))]
        (printf "~S~n" size-p)
        (n2-collisions collide bodies))))

(define (simulate collide bodies dx)
  (define new-bodies 
    (map (body-step dx) bodies))
  (values new-bodies
          (n2-collisions collide new-bodies)))

(provide/contract
 [struct body ([layer symbol?]
               [posn (vector/c number? number?)]
               [radius number?]
               [vel (vector/c number? number?)]
               [data any/c])]
 [simulate ((body? body? . -> . any/c)
            (listof body?)
            number?
            . -> .
            (values (listof body?) (listof any/c)))])