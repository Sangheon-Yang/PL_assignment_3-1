#lang racket
(provide (all-defined-out))

;sol4.rkt - 2015004693-Yang Sangheon

;problem no.01
(define (my_append x y)
  (cond [(null? x) y]
        [(list? x) (cons (car x) (append (cdr x) y))]
        [#t y]))

(define (make_bst_list btree)
  (if (null? btree)
      '()
      (let ([child (cdr btree)])
        (if (null? child)
            '()
            (my_append (make_bst_list (car child)) (cons (car btree) (make_bst_list (car (cdr child)))))))))

(define (isSorted x)
  (cond [(null? x) #t]
        [(null? (cdr x)) #t]
        [#t (let ([tail (cdr x)])
              (if (< (car x) (car tail))
                  (isSorted tail)
                  #f))]))

(define (check_bst btree)
  (isSorted (make_bst_list btree)))

;(define ok1 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
;(define no1 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (13 (14 () ()) ()))))
;(define no2 '(8 (12 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
;(define no3 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (9 () ()) ()))))
;(define ok2 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (13 () (14 () ())))))
;(define ok3 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (13 (12 () ()) (14 () ())))))
;(define c check_bst)

;problem no.02
(define (apply f bst)
  (cond [(null? bst) bst]
        [#t (cons (f (car bst)) (cons (apply f (car (cdr bst))) (cons (apply f (car (cdr (cdr bst)))) '() )))]))
;(define (square x) (* x x))
;(define (negate x) (* -1 x))


;problem no.03
(define (samelists l1 l2)
  (cond [(null? l1) (null? l2)]
        [(= (car l1) (car l2)) (samelists (cdr l1) (cdr l2))]
        [#t #f]))

(define (equals bst1 bst2)
  (if (check_bst bst1)
      (if (check_bst bst2)
          (let ([list1 (make_bst_list bst1)]
                [list2 (make_bst_list bst2)])
            (samelists list1 list2))
          #f)
      #f))

