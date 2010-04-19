#lang scheme
(require tests/eli-tester
         "pfp.ss")

(test
 (simulate (lambda (b1 b2) #f) empty 1) => (values empty empty)
 
 (simulate (lambda (b1 b2) #f) (list (make-body 'a (vector 0 0) 1 (vector 0 0) 1)) 1) 
 => (values (list (make-body 'a (vector 0 0) 1 (vector 0 0) 1)) empty)
 
 (simulate (lambda (b1 b2) #f) (list (make-body 'a (vector 0 0) 1 (vector 1 -2) 1)) 1)
 => (values (list (make-body 'a (vector 1 -2)
                             1 (vector 1 -2) 1)) empty)
 (simulate (lambda (b1 b2) #f) (list (make-body 'a (vector 0 0) 1 (vector 1 -2) 1)) 0.5)
 => (values (list (make-body 'a (vector 0.5 -1.0) 1 (vector 1 -2) 1)) empty)
 
 (simulate (lambda (b1 b2) #f) (list (make-body 'a (vector 0 0) 1 (vector 0 0) 1)
                                     (make-body 'b (vector 0.5 0.5) 1 (vector 0 0) 2)) 1)
 => 
 (values (list (make-body 'a (vector 0 0) 1 (vector 0 0) 1)
               (make-body 'b (vector 0.5 0.5) 1 (vector 0 0) 2))
         (list #f))
 
 (simulate (lambda (b1 b2) #f) (list (make-body 'a (vector 0 0) 1 (vector 0 0) 1)
                                     (make-body 'b (vector 2 2) 1 (vector 0 0) 2)) 1)
 => 
 (values (list (make-body 'a (vector 0 0) 1 (vector 0 0) 1)
               (make-body 'b (vector 2 2) 1 (vector 0 0) 2))
         (list))
 )