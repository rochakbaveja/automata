#lang racket

(define state-table
  (vector
   ;        H  E  L  O
   (vector  1 #f #f #f) ; state 0
   (vector #f  2 #f #f) ; state 1
   (vector #f #f  3 #f) ; state 2
   (vector #f #f  3  4) ; state 3
   (vector #f #f #f  4) ; state 4
   ))

(define chr->ndx
  (make-hash '[(#\H . 0) (#\E . 1) (#\L . 2) (#\O . 3)] ))

(define (next-state i chr)
  (if (hash-has-key? chr->ndx chr)
      (vector-ref (vector-ref state-table i)
                  (hash-ref chr->ndx chr) )
      #f))

(define (hello-dfa str)
  (let ([chrs (string->list str)])
    (let loop ([state 0] [chrs chrs])
      (if (equal? chrs '()) ; end of string
          (if (= state 4) 
              #t ;if 4, accepting
              #f ;not 4, not accepting
              )
          (let ([state (next-state state (car chrs))]
                [tail (cdr chrs)])
            (if (equal? state #f) #f ; invalid
                (loop state tail)))))))



