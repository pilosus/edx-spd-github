;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname list-min) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define LOS1 empty)
(define LOS2 (list 5 8 2 1 9 3 4))

(check-expect (list-min LOS1) empty)
(check-expect (list-min LOS2) 1)

;(define (list-min los) empty) ;stub

(define (mmin a b)
  (cond [(empty? a) b]
        [(empty? b) a]
        [else (min a b)]))

(define (list-min los)
  (cond [(empty? los) empty]
        [else
         (mmin (first los)
              (list-min (rest los)))])) 
