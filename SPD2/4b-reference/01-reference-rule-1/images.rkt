;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname images) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

(define Y-SCALING 1/200)
(define TEXT-SIZE 24)
(define TEXT-COLOUR "black")
(define BAR-WIDTH 30)
(define BAR-COLOUR "lightblue")

(define T1 28000)
(define T2 24000)
(define T3 36000)

(beside/align "bottom" 
              (underlay
               (rectangle BAR-WIDTH (* T1 Y-SCALING) "solid" BAR-COLOUR)
               (rectangle BAR-WIDTH (* T1 Y-SCALING) "outline" TEXT-COLOUR)
               (rotate 90 (text "School 1" TEXT-SIZE TEXT-COLOUR)))
 (rectangle BAR-WIDTH (* T2 Y-SCALING) "solid" BAR-COLOUR)
 (rectangle BAR-WIDTH (* T3 Y-SCALING) "solid" BAR-COLOUR))
