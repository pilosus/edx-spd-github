;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname quiz1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; (listof String) -> (listof String)
;; append each string's position in the list to the front of the string to number the list
(check-expect (number-list empty) empty)
(check-expect (number-list (list "first" "second" "third")) 
              (list "1: first" "2: second" "3: third"))

;(define (number-list los) los)   ;stub

(define (number-list los0)
  ;; acc: Natural;  1-based position of (first lox) in lox0
  ;; (number-list (list "first" "second" "third") 1)
  ;; (number-list (list         "second" "third") 2)
  ;; (number-list (list                  "third") 3) 
  (local [(define (number-list los acc)
            (cond [(empty? los) empty]
                  [else
                   (cons 
                    (string-append 
                     (number->string acc) ": " (first los))
                    (number-list (rest los) (add1 acc)))]))]
    (number-list los0 1)))