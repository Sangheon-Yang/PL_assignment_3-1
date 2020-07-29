#lang racket
(provide (all-defined-out)) ;; exports the defined variables in this file.

;;2015004693 Yang Sangheon

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0
;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
;; CHANGE (put your solutions here)]

(define (racketlist->mupllist rack_list)
  (cond[(null? rack_list) (aunit)]
       [(null? (cdr rack_list)) (apair (car rack_list) (aunit))]
       [#t (apair (car rack_list) (racketlist->mupllist (cdr rack_list)))]))

(define (mupllist->racketlist MUPL_list)
  (cond[(aunit? MUPL_list) '()]
       [(apair? MUPL_list) (cons (apair-e1 MUPL_list) (mupllist->racketlist (apair-e2 MUPL_list)))]
       [#t MUPL_list]))

;; Problem 2
;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if(> (int-num v1) (int-num v2))
                  (eval-under-env (ifgreater-e3 e) env)
                  (eval-under-env (ifgreater-e4 e) env))
               (error "e1,e2 not integer!!")))]
        [(fun? e) (closure env e)]
        [(call? e)
         (let ([c_fun_ex (eval-under-env (call-funexp e) env)]
               [c_arg_ex (eval-under-env (call-actual e) env)])
           (if(closure? c_fun_ex)
              (let* ([func (closure-fun c_fun_ex)]
                     [func_env (closure-env c_fun_ex)]
                     [newenv1_f_name (cons (fun-nameopt func) c_fun_ex)]
                     [newenv2_f_arg (cons (fun-formal func) c_arg_ex)])
                (if(fun-nameopt func)
                   (eval-under-env (fun-body func) (cons newenv1_f_name (cons newenv2_f_arg func_env)))
                   (eval-under-env (fun-body func) (cons newenv2_f_arg func_env))))
              (error "e1 not closure!!")))]
        [(mlet? e)
         (eval-under-env (mlet-body e) (cons (cons (mlet-var e) (eval-under-env (mlet-e e) env)) env))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v1 (eval-under-env (fst-e e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "e is not apair!!")))]
        [(snd? e)
         (let ([v1 (eval-under-env (snd-e e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "e is not apair!!")))]
        [(aunit? e) e]
        [(isaunit? e)
         (let ([v1 (eval-under-env (isaunit-e e) env)])
           (if(aunit? v1)
              (int 1)
              (int 0)))]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if(null? lstlst)
     e2
     (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2)) (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4
(define mupl-map (fun #f "arg_f"
                      (fun "map" "arg_list"
                           (ifaunit (var "arg_list")
                                    (aunit)
                                    (let ([head_arg (fst (var "arg_list"))]
                                          [tail_arg (snd (var "arg_list"))])
                                      (apair (call (var "arg_f") head_arg) (call (var "map") tail_arg)))))))

;(eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
 ;              (apair (int 8) (aunit))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "I" (call (var "map") (fun #f "x_var" (add (var "I") (var "x_var")))))))

;(eval-exp (call (call mupl-mapAddN (int 2)) (apair (int 1) (apair (int 2) (apair (int 3) (aunit)))))) 
;               (apair (int 1) (apair (int 2) (apair (int 3) (aunit))))
;; mupl-mapAddN (int i) => mupl fun : takes mupllist as a argument and return 

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
