#lang racket

;; Tic Tac Toe game solver
;; blend two templates:
;; 1) Generative recursion (trivial?)
;; 2) Arbitrary-arity tree (count--lob)
(define (count-boards bd)
  (local [(define (count--board bd)
            (if (trivial-solution? bd)
                (score-board bd)
                (count--lob (next-boards bd))))
          (define (count--lob lob) 
            (cond [(empty? lob) 0]
                  [else
                   (+ (count--board (first lob))
                      (count--lob (rest lob)))]))]
    (count--board bd)))
