;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname struct) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct pos (x y))
(define P1 (make-pos 3 6))
(define P2 (make-pos 1 2))

(pos-x P1) ; 3
(pos-y P2) ; 2

(pos? P1) ;true
(pos? "hello") ;false