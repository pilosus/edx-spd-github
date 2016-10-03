;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname austrian) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

(above
 (above (text "Guess who?" 34 "black")
        (underlay/xy 
         (underlay/xy (overlay/offset (beside (circle 10 "solid" "red") (circle 10 "solid" "red"))
                                      0 20
                                      (overlay/offset (triangle/sss 120 60 75 "solid" "black")
                                                      50 60
                                                      (circle 100 "solid" "gray"))     
                                      )
                      92 100
                      (rectangle 20 30 "solid" "black"))
         92 140
         (ellipse 50 30 "solid" "black")
         )
        )
 (text "Hint: bad austrian guy" 20 "black")
 )