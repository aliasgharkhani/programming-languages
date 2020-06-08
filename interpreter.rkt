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
  (lambda (unit-com) (caaddr unit-com)))

(define if-com->com2
  (lambda (unit-com) (caar (cdddr unit-com))))

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
(define (extend-env var value env) (append env (list (list var value))))
(define (apply-env var env) (cond ((assq var env) => cadr) (else #f)))

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
(define (list-equal-list ls1 ls2) (cond [(and (null? ls1 )(null? ls2)) #t]
                                    [(null? ls1) #f]
                                    [(null? ls2) #f]
                                    [(not (eq? (car ls1) (car ls2))) #f]
                                    [else (list-equal-list (cdr ls1) (cdr ls2))]
                               ))


(define (list-equal-number ls number)(cond
                                            [(null? ls) #t]
                                            [(not (number? (car ls))) #f]
                                            [(not (= (car ls) number)) #f]
                                            [else (list-equal-number (cdr ls) number)]
                                            ))

(define (list-equal-string ls str)(cond
                                            [(null? ls) #t]
                                            [(not (string? (car ls))) #f]
                                            [(not (string=? (car ls) str)) #f]
                                            [else (list-equal-string (cdr ls) str)]
                                            ))


(define (list-equal-bool ls bool)(cond
                                            [(null? ls) #t]
                                            
                                            [(not (eq? (car ls) bool)) #f]
                                            [else (list-equal-bool (cdr ls) bool)]
                                            ))


(define (list-equal-null ls null)(cond
                                            [(null? ls) #t]
                                            
                                            [(not (eq? (car ls) null)) #f]
                                            [else (list-equal-null (cdr ls) null)]
                                            ))

(define (mul-list-num x1 x2)
  (cond
    [(list? x1) (map (lambda (n) (* x2 n)) x1)]
    [else (map (lambda (n) (* x1 n)) x2)]))

(define (add-list-num x1 x2)
  (cond
    [(list? x1) (map (lambda (n) (+ x2 n)) x1)]
    [else (map (lambda (n) (+ x1 n)) x2)]))

(define (sub-list-num x1 x2)
  (cond
    [(list? x1) (map (lambda (n) (- n x2)) x1)]
    [else (map (lambda (n) (- x1 n)) x2)]))

(define (div-list-num x1 x2)
  (cond
    [(list? x1) (map (lambda (n) (/ n x2)) x1)]
    [else (map (lambda (n) (/ x1 n)) x2)]))

(define (bool-plus-bool bool1 bool2)
  (or bool1 bool2))

(define (bool-mul-bool bool1 bool2)
  (and bool1 bool2))

(define (bool-plus-list x1 x2)
   (cond
    [(list? x1) (map (lambda (n) (or n x2)) x1)]
    [else (map (lambda (n) (or x1 n)) x2)]))

(define (bool-mul-list x1 x2)
   (cond
    [(list? x1) (map (lambda (n) (and n x2)) x1)]
    [else (map (lambda (n) (and x1 n)) x2)]))

(define (string-plus-string s1 s2)
  (string-append s1 s2))

(define (string-plus-list x1 x2)
   (cond
    [(list? x1) (map (lambda (n) (string-append x2 n)) x1)]
    [else (map (lambda (n) (string-append x1 n)) x2)]))

(define (list-plus-list l1 l2)
  (append l1 l2))


(define (reverse-list x) (cond
                           [ (null? x) '()]
                           [ else (cond [(list? (car x)) (append (reverse-list (cdr x)) (list (reverse-list (car x))))]
                                       [else  (append (reverse-list (cdr x)) (list (car x)))])]))

;interpreter

(define run
  (lambda (string)
    (value-of (my-parser string) init-env 1)))

(define value-of-program
  (lambda (pgm env)
    (cond
      ((program? pgm) (cond
                        [(null? (cdr pgm)) (value-of (car pgm) env)]
                        [(return? (car pgm)) (value-of (car pgm) env)])
    ))))


(define value-of
  (lambda (program env [p-u 0])
    (let ((unitcom (cond
                     [(eq? p-u 1) (car program)]
                     [else program]
                    )))
      (cond
      [(or (number? unitcom) (boolean? unitcom) (string? unitcom) (null? unitcom)) unitcom] 
      [(while-com? unitcom) ('())];todo
      ((if-com? unitcom) (cond
                           [(null? (cdr program)) '()]
                           [else (let ((exp (value-of (if-com->exp unitcom) env)))
                                   (if (exp)
                                       (value-of (if-com->com1 unitcom) env)
                                       (value-of (if-com->com2 unitcom) env)))]
       ))
							   
      ((assign? unitcom) (cond
                           [(null? (cdr program)) '()]
                           [else (value-of (cdr program) (extend-env (assign->var unitcom) (value-of (assign->exp unitcom) env) env) 1)]
                           ))
      ((return? unitcom) (value-of (return->exp unitcom) env))
      
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
                              
                              [(and (list? aexp1) (or (number? aexp2) (string? aexp2) (boolean? aexp2) (null? aexp2))) (list-equal-bool aexp1 aexp2)]
                              
                              [else #f]
                              )))

      ((nequal? unitcom) (not (value-of (append (list 'equal?) (cdr unitcom)))))

      
      
      ((symmetric? unitcom) (let ([cexp (cexp->cexp unitcom)] )
                              (cond
                              [(number? cexp) (* -1 cexp)]
                              [(boolean? cexp) (not cexp)]
                              [(list? cexp) (reverse-list cexp)]) 
      
      
                              ))

      ((add? unitcom) (let ([cexp (bexp->cexp unitcom)] [bexp (bexp->bexp unitcom)])
                              (cond
                                [(and (number? cexp) (number? bexp)) (+ cexp bexp)]
                                
                                [(and (boolean? cexp) (boolean? bexp)) (or cexp bexp)]
                                [(and (string? cexp) (string? bexp)) (string-append cexp bexp)]
                                
                                [(and (number? cexp) (list? bexp)) (add-list-num bexp cexp)]
                                [(and (list? cexp) (number? bexp)) (add-list-num cexp bexp)]

                                [(and (boolean? cexp) (list? bexp)) (bool-plus-list bexp cexp)]
                                [(and (list? cexp) (boolean? bexp)) (bool-plus-list cexp bexp)]
                                [(and (string? cexp) (list? bexp)) (string-plus-list bexp cexp)]
                                [(and (list? cexp) (string? bexp)) (string-plus-list cexp bexp)]
                                [(and (list? cexp) (list? bexp)) (append cexp bexp)]
                                )
                                
      
      
                              ))
      ((sub? unitcom) (let ([cexp (bexp->cexp unitcom)] [bexp (bexp->bexp unitcom)])
                              (cond
                                
                                [(and (number? cexp) (number? bexp)) (- cexp bexp)]                             
                                
                                [(and (list? cexp) (number? bexp)) (sub-list-num cexp bexp)]
                                [(and (number? cexp) (list? bexp)) (sub-list-num bexp cexp)]
                                )
                                
      
      
                              ))

      ((mult? unitcom) (let ([cexp (bexp->cexp unitcom)] [bexp (bexp->bexp unitcom)] )
                              (cond
                                [(and (number? cexp) (number? bexp)) (* cexp bexp)]
                                
                                [(and (boolean? cexp) (boolean? bexp)) (and cexp bexp)]
                                [(and (string? cexp) (string? bexp)) (string-append cexp bexp)]
                                
                                [(and (number? cexp) (list? bexp)) (mul-list-num bexp cexp)]
                                [(and (list? cexp) (number? bexp)) (mul-list-num cexp bexp)]

                                [(and (boolean? cexp) (list? bexp)) (bool-mul-list bexp cexp)]
                                [(and (list? cexp) (boolean? bexp)) (bool-mul-list cexp bexp)]
                             
                                )
                                
      
      
                              ))
      ((div? unitcom) (let ([cexp (bexp->cexp unitcom)] [bexp (bexp->bexp unitcom)])
                              (cond
                                
                                [(and (number? cexp) (number? bexp)) (/ cexp bexp)]                             
                               
                                [(and (list? cexp) (number? bexp)) (div-list-num cexp bexp)]
                                [(and (number? cexp) (list? bexp)) (div-list-num bexp cexp)]) 
                              ))
      [(var? unitcom) (apply-env (cexp->var unitcom) env)]
      [else (unitcom)]
      
      
      ))
    

    

    
    )
  )


(define (evaluate path) (run (file->string path)))
