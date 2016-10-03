;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname question) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct person (name gender children))
;; Person is (make-person String Gender ListOfPerson)
;; interp. a person with first name, gender and a list of their children

;; ListOfPerson is one of:
;; - empty
;; (cons Person ListOfPerson)
;; interp. a list of persons

;; Gender is one of"
;; - "M"
;; - "F"
;; interp. "M" means male, "F" means female

;; Template for Person
(define (fn-for-person p)
  (... (person-name p)
       (fn-for-gender (person-gender p))
       (fn-for-lop (person-children p))))

;; Template for ListOfPerson

(define (fn-for-lop lop)
  (cond [(empty? lop)(...)]
        [else
         (... (fn-for-person (first lop))
              (fn-for-lop (rest lop)))]))

