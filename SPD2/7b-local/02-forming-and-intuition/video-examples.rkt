;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname video-examples) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(local [(define a 1)
        (define b 2)]
  (+ a b))
;(+ a b) ; are not defined

(define p "incendio ") ;won't be printed

(local [(define p "accio ")
        (define (fetch n) (string-append p n))]
  (fetch "portkey"))

(define x 1)
(define y 2)
(+ x
   (local [(define y 3)]
     (+ x y))
   y)

;;;;;;;;;;
