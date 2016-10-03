;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercises) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

empty

(define L1 (cons "Flames" empty)) ; a list if 1 element
(define LX (cons "Leafs" (cons "Flames" empty))) ; a list of 2 elements
(define LY (cons (string-append "C" "anuks") empty))
(define L2 (cons 10 (cons 9 (cons 10 empty)))) ; a list of 3 elements

(define L3 (cons (square 10 "solid" "blue")
      (cons (triangle 20 "solid" "green")
            empty)))

(first L1) ; first element of the list L1
(first L2)
(first L3)

(rest L1)
(rest L2)
(rest L3)

(second L2) ; second element of L2
(equal? (second L2) (first (rest L2)))

(first (rest (rest L2))) ; third element of L2

(empty? empty)
(empty? L2)
;(empty? 12) ; false as it's not a 

(define E1 (cons "Systematic" (cons "Program" (cons "Design" empty))))
(define E2 (cons 1 empty))
(first E1)
(rest E1)
(rest E2)
(empty? (rest E2))