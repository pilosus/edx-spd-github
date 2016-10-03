#lang racket
(require rackunit)

(define L1 (list 1 2 3 (list 1 2) (list 1 3) (list 2 3)))
(define L2 (list 3 4))
(define L3 (list 2 3 4 (list 2 3) (list 2 4) (list 3 4)))
(define LS (list L1 L2 L3))

;; (listof (listof Any)) -> (listof (listof Any))
;; produce l

#;

(define (backtracking-fn x)
  (local [(define (fn-for-x x)
            (... (fn-for-lox (x-subs x))))
          
          (define (fn-for-lox lox)
            (cond [(empty? lox) false]
                  [else
                   (local [(define try (fn-for-x (first lox)))] ;try first child
                     (if (not (false? try))                     ;successful?
                         try                                    ;if so produce that
                         (fn-for-lox (rest lox))))]))]          ;or try rest of children
    
    (fn-for-x x)))

; . means any number of arguments
;(cartesian-product (range 1 3) (range 1 5) (range 1 4))
(define (cartesian-product . lsts)
  (foldr (lambda (lst acc)
           (for*/list ((x (in-list lst))
                       (y (in-list acc)))
             (cons x y)))
         '(())
         lsts))


;(list-perm L1 L2 L3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROBLEM 2
;; Design a search program that consumes a list of TAs and a list of Slots, and produces all the 
;; possible schedules where each Slot is assigned to a TA, and no TA is working more than their 
;; maximum shifts. If no such schedules exist, produce false.

;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap

;; (listof Slot)
(define S1 (list 1 2 3))
(define S2 (list 1 2 3 4))
(define S3 (list 1 2 3 4 5))

(define-struct ta (name max avail))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for
(define TA1 (make-ta "Harry"    1 (list 1   3)))
(define TA2 (make-ta "Ron"      1 (list   2 3)))
(define TA3 (make-ta "Hermione" 2 (list   2    4)))

#;
(define (fn-for-ta t)
  (...(ta-name t)     ; String
      (ta-max t)      ; Natural
      (ta-avail t))) ; (listof Slot)
;; Template rules used:
;;  - compound: 3 fields

(define TAS (list TA1 TA2 TA3))

(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

(define A1 (make-assignment TA1 1))
(define A2 (make-assignment TA2 3))
(define A3 (make-assignment TA3 2))
(define A4 (make-assignment TA3 4))

#;
(define (fn-for-assignment a)
  (...(assignment-ta a)        ; TA
      (assignment-slot a)))    ; Slot

;; Template rules used:
;;  - compound: 2 fields

;; Schedule is (listof Assignment)
(define SD1 (list A1 A2 A3))
(define SD2 (list A1 A2 A3 A4))

;; Schedule -> (listof Slot)
(define (schedule->slots sd)
  (cond [(empty? sd) empty]
        [else
         (cons (assignment-slot (first sd))
               (schedule->slots (rest sd)))]))

;; (listof (listof Any)) -> (listof (listof Any))
;; produce all possible permutations of the element of the given lists
(define (list-perm lsts)
  (foldr (lambda (lst acc)
           (for*/list ((x (in-list lst))
                       (y (in-list acc)))
             (cons x y)))
         '(())
         lsts))

;; (listof Number) Natural -> (listof Any)
;; produce list of all possible combinations by the given number of elemets

;(define (combinations n lst) lst) ;stub

(define (combinations n0 lst0)
  (local [;; produce list where each element of the given list become lists themselves
          (define (singletons lst)
            (cond [(empty? lst) empty]
                  [else
                   (cons (list (first lst))
                         (singletons (rest lst)))]))
          ;; produce list containing n copies of the given list
          (define (copy-list n lst)
            (map (lambda (x) lst) (build-list n identity)))
          
          ;; produce true if all elements of the list are the same
          (define (all-the-same? lst)
            (andmap (lambda (x) (equal? (first lst) x)) lst))
          
          ;; produce true if at leat two elements are the same
          (define (two-or-more-the-same? lst)
            (cond [(empty? lst) false]
                  [else
                   (if (member (first lst) (rest lst))
                       true
                       (two-or-more-the-same? (rest lst)))]))
          
          (define (combinations n lst)
            (cond [(equal? n 1) (singletons lst)]
                  [else
                   (filter (lambda (x) (not (two-or-more-the-same? x)))
                           (remove-duplicates 
                            (map 
                             (lambda (x) (sort x <)) 
                             (list-perm (copy-list n lst)))))]))]    
    (combinations n0 lst0)))

;; Natural (listof Any) -> (listof (listof Any))
;; produce list of all combinations by 1..n
(define (comb-upto n0 lst0)
  (local [(define iter (build-list n0 identity))
          (define (comb-upto n lst iter res)
            (cond [(empty? iter) res]
                  [else
                   (append (combinations n lst)
                           (comb-upto (sub1 n) lst (rest iter) res))]))]
    (comb-upto n0 lst0 iter empty)))

;; TA -> (listof Schedule)
;; produce list of schedules for the given TA
(define (ta->schedule t)
  (local [(define avail (ta-avail t))
          (define name (ta-name t))
          (define maximum (ta-max t))
          (define combinations (comb-upto maximum avail))
          ;; TA (listof Slot) -> Schedule
          (define (flatten-combs t lst)
            (cond [(empty? lst) empty]
                  [else
                   (cons (make-assignment t (first lst))
                         (flatten-combs t (rest lst)))]))
          ;; (listof (listof Slot)) -> Schedule
          (define (combs->schedule t combinations)
            (cond [(empty? combinations) empty]
                  [else
                   (cons (flatten-combs t (first combinations))
                         (combs->schedule t (rest combinations)))]))]
    (combs->schedule t combinations)))

;; (listof Schedule) (listof Slot) -> (listof Schedule)
;; filter out invalid schedules
(define (keep-only-valid-schedules losd los)
  (local [(define (flatten-losd losd)
            (cond [(empty? losd) empty]
                  [else
                   (cons (flatten (first losd)) (flatten-losd (rest losd)))]))
          (define flat-losd (flatten-losd losd))]
    (filter 
     (lambda (x) (subset? (list->set los) (list->set (schedule->slots x)))) 
     flat-losd)))
#;
; works only for exact match. 
; If list of slots we need to fill is shorter, than we can generate using all TAs
; then it fails
(define (keep-only-valid-schedules losd los)
  (local [(define (flatten-losd losd)
            (cond [(empty? losd) empty]
                  [else
                   (cons (flatten (first losd)) (flatten-losd (rest losd)))]))
          (define flat-losd (flatten-losd losd))
          (define sorted-los (sort los <))]
    (filter 
     (lambda (x) (equal? (sort (schedule->slots x) <) sorted-los)) 
     flat-losd)))

;; (listof TA) (listof Slot) -> (listof Schedule) | false
;; produce a list of possible schedules, where each Slot assigned to a TA
;; produce false if not such schedules exist
;; Assume: no TA is working more than their maximum shifts

(define (assign lota los)
  (local [(define schedules (map (lambda (x) (ta->schedule x)) lota))
          (define losd (list-perm schedules))
          (define valid (keep-only-valid-schedules losd los))
          ;; (listof Number) -> String
          ;; produce formatted string with the elements of the given list
          (define (list->string lst0)
            (local [(define (list->string lst result)
                      (cond [(empty? lst) result]
                            [else
                             (list->string (rest lst) 
                                           (string-append 
                                            result
                                            " "
                                            (number->string (first lst))))]))]
              (list->string lst0 "")))
          ;; Assignment -> String
          ;; profuce formatted string for the given Assignment
          (define (assignment->string a)
            (string-append (ta-name (assignment-ta a))
                           " "
                           (number->string (assignment-slot a))))
          
          ;; Schedule -> String
          ;; produce formatted string for the given list of assignemnts (schedule)
          (define (schedule->string sd0)
            (local [(define (schedule->string sd result)
                      (cond [(empty? sd) result]
                            [else
                             (schedule->string (rest sd) 
                                               (if (zero? (string-length result))
                                                   (string-append (assignment->string (first sd)))
                                                   (string-append result ", " (assignment->string (first sd)))))]))]
              (schedule->string sd0 "")))
          
          ;; (listof Schedule) -> String
          (define (format-losd losd0)
            (cond [(empty? losd0) empty]
                  [else
                   (cons (schedule->string (first losd0))
                         (format-losd (rest losd0)))]))]
    
    (if (empty? valid) 
        false
        (format-losd valid))))

; (assign TAS (list 1 2 3 4))

;; Problem 2 from the Final Quiz
(define T1 (make-ta "Erika" 1 (list 1 3 7 9)))
(define T2 (make-ta "Ryan" 1 (list 1 8 10)))
(define T3 (make-ta "Reece" 1 (list 5 6)))
(define T4 (make-ta "Gordon" 2 (list 2 3 9)))
(define T5 (make-ta "David" 2 (list 2 8 9)))
(define T6 (make-ta "Katie" 1 (list 4 6)))
(define T7 (make-ta "Aashish" 2 (list 1 10)))
(define T8 (make-ta "Grant" 2 (list 1 11)))
(define T9 (make-ta "Raeanne" 2 (list 1 11 12)))

(define T10 (make-ta "Alex" 1 (list 7)))
(define T11 (make-ta "Erin" 1 (list 4)))

(define STAFF (list T1 T2 T3 T4 T5 T6 T7 T8 T9))

(define SLOTS (build-list 12 add1))

;; Question 3
;(assign STAFF SLOTS)

;; Question 4
;(assign (cons T10 STAFF) SLOTS)

;; Question 5
;(assign (cons T11 (cons T10 STAFF)) SLOTS)
