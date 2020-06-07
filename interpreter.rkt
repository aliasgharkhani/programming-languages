#lang racket
(require racket/include)
(require "parser.rkt")
;predicators
(define program?
  (lambda (command)
    (cond
      ((null? (cdr command)) (unitcom? (car command)))
      (else (and (unitcom? (car command)) (program? (cdr command)))))
    ))

(define unitcom?
  (lambda (unitcom) (or (while-com? unitcom) (if-com? unitcom) (assign? unitcom) (return? unitcom))))

(define while-com?
  (lambda (unit-com) (eqv? (car unit-com) 'while)))

(define if-com?
  (lambda (unit-com) (eqv? (car unit-com) 'if)))

(define assign?
  (lambda (unit-com) (eqv? (car unit-com) 'assign)))

(define return?
  (lambda (unit-com) (eqv? (car unit-com) 'return)))

(define more?
  (lambda (exp) (eqv? (car exp) 'more?)))

(define less?
  (lambda (exp) (eqv? (car exp) 'less?)))

(define equal?
  (lambda (exp) (eqv? (car exp) 'equal?)))

(define nequal?
  (lambda (exp) (eqv? (car exp) 'nequal?)))

(define sub?
  (lambda (exp) (eqv? (car exp) 'sub?)))

(define add?
  (lambda (exp) (eqv? (car exp) 'add?)))

(define mult?
  (lambda (exp) (eqv? (car exp) 'mult?)))

(define div?
  (lambda (exp) (eqv? (car exp) 'div?)))

(define symmetric?
  (lambda (exp) (eqv? (car exp) '-)))

(define my-null?
  (lambda (exp) (eqv? exp 'null)))

(define var?
  (lambda (exp) (eqv? (car exp) 'var)))

(define var-listmem?
  (lambda (cexp) (eqv? (car cexp) 'list-ref?)))

;extractors

(define while-com->exp
  (lambda (unit-com) (caadr unit-com)))

(define while-com->command
  (lambda (unit-com) (caaddr unit-com)))

(define if-com->exp
  (lambda (unit-com) (caadr unit-com)))

(define if-com->com1
  (lambda (unit-com) (caddar unit-com)))

(define if-com->com2
  (lambda (unit-com) (cdddar unit-com)))

(define assign->var
  (lambda (unit-com) (cadr unit-com)))

(define assign->exp
  (lambda (unit-com) (caddr unit-com)))

(define return->exp
  (lambda (unit-com) (cadr unit-com)))

(define exp->aexp1
  (lambda (exp) (cadr exp)))

(define exp->aexp2
  (lambda (exp) (caddr exp)))

(define aexp->bexp
  (lambda (aexp) (cadr aexp)))

(define aexp->aexp
  (lambda (aexp) (caddr aexp)))

(define bexp->cexp
  (lambda (bexp) (cadr bexp)))

(define bexp->bexp
  (lambda (bexp) (caddr bexp)))

(define cexp->cexp
  (lambda (cexp) (cadr cexp)))

(define cexp->exp
  (lambda (cexp) (car cexp)))

(define cexp->var
  (lambda (cexp) (cadr cexp)))

(define cexp->listmem
  (lambda (cexp) (caddr cexp)))







;helper functions

(define init-env '())
(define (extend-env name value env) (cons (cons name value) env))
(define (apply-env name env) (cond ((assq name env) => cdr) (else #f)))

(define (list-greater-than-string ls str)(cond
                                            [(null? ls) #t]
                                            [(string<=? (car ls) str) #f]
                                            [else (list-greater-than-string (cdr ls) str)]
                                            ))

(define (list-less-than-string ls str)(cond
                                            [(null? ls) #t]
                                            [(string>=? (car ls) str) #f]
                                            [else (list-less-than-string (cdr ls) str)]
                                            ))

(define (list-equal-string ls str)(cond
                                            [(null? ls) #t]
                                            [(not (string? (car ls))) #f]
                                            [(not (string=? (car ls) str)) #f]
                                            [else (list-equal-string (cdr ls) str)]
                                            ))


(define (list-greater-than-number ls number)(cond
                                            [(null? ls) #t]
                                            [(<= (car ls) number) #f]
                                            [else (list-greater-than-number (cdr ls) number)]
                                            ))

(define (list-less-than-number ls number)(cond
                                            [(null? ls) #t]
                                            [(>= (car ls) number) #f]
                                            [else (list-less-than-number (cdr ls) number)]
                                            ))

(define (list-equal-number ls number)(cond
                                            [(null? ls) #t]
                                            [(not (number? (car ls))) #f]
                                            [(not (= (car ls) number)) #f]
                                            [else (list-equal-number (cdr ls) number)]
                                            ))
                                          
                                          
                                          

(define (list-equal-list ls1 ls2) (cond [(and (null? ls1 )(null? ls2)) #t]
                                    [(null? ls1) #f]
                                    [(null? ls2) #f]
                                    [(not (eq? (car ls1) (car ls2))) #f]
                                    [else (list-equal-list (cdr ls1) (cdr ls2))]
                               ))


(define (list-equal-exp ls exp)(cond
                                            [(null? ls) #t]
                                            
                                            [(not (eq? (car ls) exp)) #f]
                                            [else (list-equal-exp (cdr ls) exp)]
                                            ))




(define (reverse x) (cond
                           [ (null? x) '()]
                           [ else (cond [(list? (car x)) (append (reverse (cdr x)) (list (reverse (car x))))]
                                       [else  (append (reverse (cdr x)) (list (car x)))])]))


(define (mul-list lst x)
  (map (lambda (n) (* x n)) lst))
(define (add-list lst x)
  (map (lambda (n) (+ x n)) lst))
(define (sub-list-from-number lst x)
  (map (lambda (n) (- n x)) lst))
(define (sub-number-from-list lst x)
  (map (lambda (n) (- x n)) lst))

(define (div-list-by-number lst x)
  (map (lambda (n) (/ n x)) lst))
(define (div-number-by-list lst x)
  (map (lambda (n) (/ x n)) lst))


(define (bool-plus-list lst x)
  (map (lambda (n) (or x n)) lst))
(define (bool-mul-list lst x)
  (map (lambda (n) (and x n)) lst))

(define (string-plus-list lst x)
  (map (lambda (n) (string-append x n)) lst))
(define (list-plus-string lst x)
  (map (lambda (n) (string-append n x)) lst))
;interpreter

(define run
  (lambda (string)
    (value-of-program (my-parser string))))

(define value-of-program
  (lambda (pgm)
    (cond
      ((program? pgm) (value-of pgm init-env ))))
    )

(define value-of
  (lambda (unitcom env)
    (cond
      ((while-com? unitcom) ('()));todo
      ((if-com? unitcom) (let ((exp (value-of (if-com->exp unitcom) env)))
                           (if (exp)
                               (value-of (if-com->com1 unitcom) env)
                               (value-of (if-com->com2 unitcom) env))))
							   
      ((assign? unitcom) (extend-env (assign->var unitcom) (value-of (assign->exp unitcom) env)))
      ((return? unitcom) (value-of (return->exp unitcom)))
      
      ((more? unitcom) (let ([aexp1 (exp->aexp1 unitcom)] [aexp2 (exp->aexp2 unitcom)])
                              (cond 
                              [(and (number? aexp1) (number? aexp2)) (> aexp1 aexp2)]
                              [(and (string? aexp1) (string? aexp2)) (string>? aexp1 aexp2)]
                              [(and (list? aexp1) (string? aexp2)) (list-greater-than-string aexp1 aexp2)]
                              [(and (list? aexp1) (number? aexp2)) (list-greater-than-number aexp1 aexp2)]
                              )))

      ((less? unitcom) (let ([aexp1 (exp->aexp1 unitcom)] [aexp2 (exp->aexp2 unitcom)])
                              (cond 
                              [(and (number? aexp1) (number? aexp2)) (< aexp1 aexp2)]
                              [(and (string? aexp1) (string? aexp2)) (string<? aexp1 aexp2)]
                              [(and (list? aexp1) (string? aexp2)) (list-less-than-string aexp1 aexp2)]
                              [(and (list? aexp1) (number? aexp2)) (list-less-than-number aexp1 aexp2)]
                              )))

    
      ((equal? unitcom) (let ([aexp1 (exp->aexp1 unitcom)] [aexp2 (exp->aexp2 unitcom)])
                              (cond 
                              [(and (number? aexp1) (number? aexp2)) (= aexp1 aexp2)]
                              [(and (string? aexp1) (string? aexp2)) (string=? aexp1 aexp2)]
                              [(and (null? aexp1) (null? aexp2)) #t]
                              [(and (boolean? aexp1) (boolean? aexp2)) (eq? aexp1 aexp2)]
                              [(and (list? aexp1) (list? aexp2))(list-equal-list aexp1 aexp2)]
                              
                              [(and (list? aexp1) (or (number? aexp2) (string? aexp2) (boolean? aexp2) (null? aexp2))) (list-equal-exp aexp1 aexp2)]
                              
                              [else #f]
                              )))

      
      ;((nequal? unitcom) (let ([aexp1 (exp->aexp1 unitcom)] [aexp2 (exp->aexp2 unitcom)])
       ;                       (not (value-of equal-string))))


      ((symmetric? unitcom) (let ([cexp (cexp->cexp unitcom)] )
                              (cond
                              [(number? cexp) (* -1 cexp)]
                              [(boolean? cexp) (not cexp)]
                              [(list? cexp) (reverse cexp)]) 
      
      
                              ))

      ((add? unitcom) (let ([cexp (bexp->cexp unitcom)] [bexp (bexp->bexp unitcom)])
                              (cond
                                [(and (number? cexp) (number? bexp)) (+ cexp bexp)]
                                
                                [(and (boolean? cexp) (boolean? bexp)) (or cexp bexp)]
                                [(and (string? cexp) (string? bexp)) (string-append cexp bexp)]
                                
                                [(and (number? cexp) (list? bexp)) (add-list bexp cexp)]
                                [(and (list? cexp) (number? bexp)) (add-list cexp bexp)]

                                [(and (boolean? cexp) (list? bexp)) (bool-plus-list bexp cexp)]
                                [(and (list? cexp) (boolean? bexp)) (bool-plus-list cexp bexp)]
                                [(and (string? cexp) (list? bexp)) (string-plus-list bexp cexp)]
                                [(and (list? cexp) (string? bexp)) (list-plus-string cexp bexp)]
                                [(and (list? cexp) (list? bexp)) (append cexp bexp)]
                                )
                                
      
      
                              ))
      ((sub? unitcom) (let ([cexp (bexp->cexp unitcom)] [bexp (bexp->bexp unitcom)])
                              (cond
                                
                                [(and (number? cexp) (number? bexp)) (- cexp bexp)]                             
                                
                                [(and (list? cexp) (number? bexp)) (sub-list-from-number cexp bexp)]
                                [(and (number? cexp) (list? bexp)) (sub-number-from-list bexp cexp)]
                                )
                                
      
      
                              ))

      ((mult? unitcom) (let ([cexp (bexp->cexp unitcom)] [bexp (bexp->bexp unitcom)] )
                              (cond
                                [(and (number? cexp) (number? bexp)) (* cexp bexp)]
                                
                                [(and (boolean? cexp) (boolean? bexp)) (and cexp bexp)]
                                [(and (string? cexp) (string? bexp)) (string-append cexp bexp)]
                                
                                [(and (number? cexp) (list? bexp)) (mul-list bexp cexp)]
                                [(and (list? cexp) (number? bexp)) (mul-list cexp bexp)]

                                [(and (boolean? cexp) (list? bexp)) (bool-mul-list bexp cexp)]
                                [(and (list? cexp) (boolean? bexp)) (bool-mul-list cexp bexp)]
                             
                                )
                                
      
      
                              ))
      ((div? unitcom) (let ([cexp (bexp->cexp unitcom)] [bexp (bexp->bexp unitcom)])
                              (cond
                                
                                [(and (number? cexp) (number? bexp)) (/ cexp bexp)]                             
                               
                                [(and (list? cexp) (number? bexp)) (div-list-by-number cexp bexp)]
                                [(and (number? cexp) (list? bexp)) (div-number-by-list bexp cexp)]) 
      
      
                              ))

      
      
      )

    

    
    ))


(define (evaluate path) (run (file->string path)))
