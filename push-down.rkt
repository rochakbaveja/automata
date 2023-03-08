#lang racket

(define stack '(ω)) ; ω is the bottom of stack marker

(define (pop)
  (let ([s (car stack)])
    (set! stack (cdr stack))
    s))

(define (push s)
  (set! stack (cons s stack)))

(define (peek)
  (car stack))

(define (make-reader input)
  (define (read)
    (if (equal? '() input) 'ϵ
        (let ([sym (car input)])
          (set! input (cdr input))
          sym)))
  read)

(define (run-pda input)
  (let ([read (make-reader input)])
    (set! stack '(ω)) ; initialze stack
    (define (pda)
      (let ([symbol (read)]
            [top (peek)])
        (match (cons symbol top)
          [(cons 'ϵ 'ω) #t] ; accept input
          [(cons  0 'ω) (begin (push 0) (pda))]
          [(cons  0  0) (begin (push 0) (pda))]
          [(cons  1 'ω) (begin (push 1) (pda))]
          [(cons  1  1) (begin (push 1) (pda))]
          [(cons  0  1) (begin (pop)    (pda))]
          [(cons  1  0) (begin (pop)    (pda))]
          [_ #f])))
    (pda))) 