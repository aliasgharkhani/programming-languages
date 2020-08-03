#lang racket
(require racket/include)
(require "parser.rkt")
(require "lib.rkt")
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

(define my-equal?
  (lambda (exp) (eqv? (car exp) 'equal?)))

(define my-nequal?
  (lambda (exp) (eqv? (car exp) 'nequal?)))

(define sub?
  (lambda (exp) (eqv? (car exp) 'sub)))

(define add?
  (lambda (exp) (eqv? (car exp) 'add)))

(define mult?
  (lambda (exp) (eqv? (car exp) 'mult)))

(define div?
  (lambda (exp) (eqv? (car exp) 'div)))

(define symmetric?
  (lambda (exp) (eqv? (car exp) '-)))

(define my-null?
  (lambda (exp) (eqv? exp 'null)))

(define var?
  (lambda (exp) (eqv? (car exp) 'var)))

(define var-listmem?
  (lambda (cexp) (eqv? (car cexp) 'list-ref)))

(define par?
  (lambda (cexp) (eqv? (car cexp) 'par)))

(define func?
  (lambda (unit-com) (cond
                       ((not (list? unit-com)) #f)
                       (else (eqv? (car unit-com) 'func)))))

(define func-call?
  (lambda (unit-com) (eqv? (car unit-com) 'call)))
;extractors

(define while-com->exp
  (lambda (unit-com) (caadr unit-com)))

(define while-com->com
  (lambda (unit-com) (caddr unit-com)))

(define if-com->exp
  (lambda (unit-com) (cadr unit-com)))

(define if-com->com1
  (lambda (unit-com) (caddr unit-com)))

(define if-com->com2
  (lambda (unit-com) (cadddr unit-com)))

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

(define cexp->var
  (lambda (cexp) (cadr cexp)))

(define cexp->listmem
  (lambda (cexp) (caddr cexp)))

(define cexp->exp
  (lambda (cexp) (cadr cexp)))

(define func->vars
  (lambda (unit-com) (cadar unit-com)))

(define func->com
  (lambda (unit-com) (caddar unit-com)))


(define func->env
  (lambda (unit-com) (cadr unit-com)))
(define func-call->args
  
  (lambda (unit-com) (caddr unit-com)))

(define func-call->name
  (lambda (unit-com) (cadr unit-com)))

;helper functions

(define (before-and-after var env) (cond
                                     [(null? env) '()]
                                     [(eq? (caar env) var) (cdr env)]
                                         
                                     [else (append (list (car env)) (before-and-after var (cdr env)))]
                                         
                                     ))
(define init-env '((listmaker ((func (a b) ((if (less? (var a) 1) ((return ())) ((assign a (call listmaker ((sub (var a) 1) (var b)))) (return (add (var a) ((var b)))))))) ((pow ((func (a b) ((if (equal? (var b) 0) ((return 1)) ((assign result (call pow ((var a) (sub (var b) 1)))) (return (mult (var result) (var a))))))) ()))))) (pow ((func (a b) ((if (equal? (var b) 0) ((return 1)) ((assign result (call pow ((var a) (sub (var b) 1)))) (return (mult (var result) (var a))))))) ()))))
(define lib-env '((eval 1) (pow (define pow (lambda (a b) (if (eq? b 0) 1 (* a (pow a (- b 1)))))))
                           (makelist (define makelist (lambda (a b)(cond[(<= a 0) '()][else (cons b (makelist (- a 1) b))]))))
                           (reverse (define reverse (lambda (a) (cond [(null? a) a] [else (append (reverse (cdr a)) (list (car a)))]))))
                           (reverseall (define reverseall (lambda (a) (cond [(null? a) a] [else (let ((head (car a)) (tail (cdr a))) (append (reverseall tail) (cond [(list? head) (list (reverseall head))] [else (list head)]) ))]))))
                           (set (define set (lambda (a index value) (cond [(null? a) "list index outof range"] [(eq? index 0) (cons value (cdr a))] [else (let ((aux (set (cdr a) (- index 1) value))) (if (eq? aux "list index outof range") "list index outof range" ((cons (car a) aux))))]))))
                           (merge (define merge (lambda (a b) (cond [(null? a) b] [(null? b) a] [(> (car a) (car b)) (cons (car b) (merge a (cdr b)))] [else (cons (car a) (merge (cdr a) b))]))))
                           (mergesort (define (mergesort vs) (match vs [(list)  vs] [(list a)  vs] [_  (define-values (lvs rvs) (split-at vs (quotient (length vs) 2))) (merge (mergesort lvs) (mergesort rvs))])))))

(define (extend-env var value env) (cond
                                     [(null? env) (list (list var value))]
                                     ;[(eq? (apply-env var env) "not in env") (begin     (append  (list (list var value)) env))]
                                     ;[else (begin (append (before-and-after var env)  (list (list var value))))]
                                     [else (begin     (append  (list (list var value)) env))]
                                     ))
(define (apply-lib-env var env) (cond
                               [(null? env)  "not in env"]
                               ;[(eq? (caar env) var) (cond (func? (caar env) (begin (display (caar env) (display "dzzzzzzz\n\n")  (cdar env))) (else  (cadr (value-of (caadar env) (car (cdadar env)))))) ]
                               [(eq? (caar env) var) (cadar env)]
                               [else (apply-lib-env var (cdr env))]
                               ))


(define (apply-env var env) (cond
                               [(null? env)  "not in env"]
                               ;[(eq? (caar env) var) (cond (func? (caar env) (begin (display (caar env) (display "dzzzzzzz\n\n")  (cdar env))) (else  (cadr (value-of (caadar env) (car (cdadar env)))))) ]
                               [(eq? (caar env) var) (cond   [(begin  (func? (caadar env))) (cadar env)] 
                                                            [else (cadr (value-of (caadar env) (car (cdadar env))))]) ]
                               [else (apply-env var (cdr env))]
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
                                    [(not (equal? (car ls1) (car ls2))) #f]
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
                           [(null? x) '()]
                           [else (cond [(list? (car x)) (append (list (reverse-list (car x))) (reverse-list (cdr x)))]
                                       [else  (append  (list (- 0 (car x))) (reverse-list (cdr x)))])]))

(define (compare-error first second)(cond
                                      [(and (number? first) (list? second)) (raise "Error! Cannot compare number and list.")]
                                      [(and (boolean? first) (list? second)) (raise "Error! Cannot compare boolean and list.")]
                                      [(and (string? first) (list? second)) (raise "Error! Cannot compare string and list.")]
                                      [(and (null? first) (list? second)) (raise "Error! Cannot compare null and list.")]
                                      [(and (list? first) (null? second)) (raise "Error! Cannot compare list and null.")]
                                      [(and (list? first) (list? second)) (raise "Error! Cannot compare list and list.")]
                                      [(and (number? first) (string? second)) (raise "Error! Cannot compare number and string.")]
                                      [(and (string? first) (number? second)) (raise "Error! Cannot compare string and number.")]
                                      [(and (string? first) (boolean? second)) (raise "Error! Cannot compare string and boolean.")]
                                      [(and (boolean? first) (string? second)) (raise "Error! Cannot compare boolean and string.")]
                                      [(and (number? first) (boolean? second)) (raise "Error! Cannot compare number and boolean.")]
                                      [(and (boolean? first) (number? second)) (raise "Error! Cannot compare boolean and number.")]
                                      [(or (null? first) (null? second)) (raise "Error! Cannot compare null with other types.")]
                                      [else (raise "Error! Comparison cannot be done.")]
                                     
                                      
                                      

                                      )
  ) 


(define (bound args vars env current-env) (cond
                                [(null? args) env]
                                ;[else (bound (cdr args) (cdr vars)  (cons (list (car vars) (list (car args) current-env))) current-env)]))
                                [else (bound (cdr args) (cdr vars) (extend-env (car vars) (list (car args) current-env) env) current-env)]))   


(define pow (lambda (a b) (if (eq? b 0) 1 (* a (pow a (- b 1))))))
(define makelist (lambda (a b)(cond[(<= a 0) '()][else (cons b (makelist (- a 1) b))])))
(define reverse (lambda (a) (cond [(null? a) a] [else (append (reverse (cdr a)) (list (car a)))])))
(define reverseall (lambda (a) (cond [(null? a) a] [else (let ((head (car a)) (tail (cdr a))) (append (reverseall tail) (cond [(list? head) (list (reverseall head))] [else (list head)]) ))])))
(define set (lambda (a index value) (cond [(null? a) "list index outof range"] [(eq? index 0) (cons value (cdr a))] [else (let ((aux (set (cdr a) (- index 1) value))) (if (eq? aux "list index outof range") "list index outof range" ((cons (car a) aux))))])))
(define merge (lambda (a b) (cond [(null? a) b] [(null? b) a] [(> (car a) (car b)) (cons (car b) (merge a (cdr b)))] [else (cons (car a) (merge (cdr a) b))])))
(define (mergesort vs) (match vs [(list)  vs] [(list a)  vs] [_  (define-values (lvs rvs) (split-at vs (quotient (length vs) 2))) (merge (mergesort lvs) (mergesort rvs))]))


;interpreter


(define run 
  (lambda (string) 
    (cadr (value-of (my-parser string) init-env 1))))

(define value-of-program
  (lambda (pgm env)
    (cond
      ((program? pgm) (cond
                        [(null? (cdr pgm)) (value-of (car pgm) env)]
                        [(return? (car pgm)) (value-of (car pgm) env)])
    ))))




; order of return arguments in value-of procedure : (env return-value)
(define value-of
  
  (lambda (program env [p-u 0] [r-fr 0])
    (begin ;(display env) (display "\n") (display program) (display "hrer\n\n")  
      (cond [(not (eq? "not in env" (apply-env 'return-value env))) (list env (apply-env 'return-value env))]
            [else (cond
     
        [(eq? p-u 1) (cond
                       [(null? (cdr program)) (value-of (car program) env)]
                       ;[(return? (car program)) (value-of (car program) env)]
                       [else (value-of (cdr program) (car (value-of (car program) env)) 1)])]
        [else (cond
                [(or (number? program) (boolean? program) (string? program) (null? program)) (list env program)]
                [(my-null? program)  (list env 'null)]
                [(while-com? program) (cond
                                        [(cadr (value-of (while-com->exp program) env)) (value-of program (car (value-of (while-com->com program) env 1)))]
                                        [else (list env '())]
                                        )]
                
                [(if-com? program) (let ((exp (cadr (value-of (if-com->exp program) env))))
                                     (if exp
                                         (value-of (if-com->com1 program) env 1)
                                         (value-of (if-com->com2 program) env 1)))]
                
                ((assign? program) (begin (list (extend-env (assign->var program)  (begin ( list (assign->exp program) env)) env)  null))
                 ;(cond
                                     ;[(func? (assign->exp program)) (list (extend-env (assign->var program)  (begin ( list (assign->exp program) (extend-env (assign->var program)  (begin ( list (assign->exp program) env)) env))) env)  null)]
                                     ;[#t (list (extend-env (assign->var program)  (begin ( list (assign->exp program) env)) env)  null)]
                                     ;)
                                   )
                ;((return? program)  (value-of (return->exp program) env))
                ((return? program) (begin (define aux (value-of (return->exp program) env)) (list (extend-env 'return-value (list (cadr aux) '()) env) (cadr aux))))
                
                ((more? program) (let ([aexp1 (cadr (value-of (exp->aexp1 program) env))] [aexp2 (cadr (value-of (exp->aexp2 program) env))])
                                   (cond 
                                     [(and (number? aexp1) (number? aexp2)) (list env (> aexp1 aexp2))]                                     
                                     [(and (string? aexp1) (string? aexp2)) (list env (string>? aexp1 aexp2))]
                                     [(and (list? aexp1) (string? aexp2)) (list env (list-greater-than-string aexp1 aexp2))]
                                     [(and (list? aexp1) (number? aexp2)) (list env (list-greater-than-number aexp1 aexp2))]
                                     [else (list env (compare-error aexp1 aexp2))]
                                     )))
                
                ((less? program) (let ([aexp1 (cadr (value-of (exp->aexp1 program) env))] [aexp2 (cadr (value-of (exp->aexp2 program) env))])
                                   (cond 
                                     [(and (number? aexp1) (number? aexp2)) (list env (< aexp1 aexp2))]
                                     [(and (string? aexp1) (string? aexp2)) (list env (string<? aexp1 aexp2))]
                                     [(and (list? aexp1) (string? aexp2)) (list env (list-less-than-string aexp1 aexp2))]
                                     [(and (list? aexp1) (number? aexp2)) (list env (list-less-than-number aexp1 aexp2))]
                                     [else (list env (compare-error aexp1 aexp2))]
                                     )))
                
                
                ((my-equal? program) (let ([aexp1 (cadr (value-of (exp->aexp1 program) env))] [aexp2 (cadr (value-of (exp->aexp2 program) env))])
                                       (cond 
                                         [(and (number? aexp1) (number? aexp2)) (list env (= aexp1 aexp2))]
                                         [(and (string? aexp1) (string? aexp2)) (list env (string=? aexp1 aexp2))]
                                         [(and (null? aexp1) (null? aexp2)) (list env #t)]
                                         [(and (boolean? aexp1) (boolean? aexp2)) (list env (eq? aexp1 aexp2))]
                                         [(and (list? aexp1) (and (list? aexp2) (not (null? aexp2)))) (list env (equal? aexp1 aexp2))]
                                         [(and (list? aexp1) (number? aexp2))  (list env (list-equal-number aexp1 aexp2))]
                                         [(and (list? aexp1) (string? aexp2))  (list env (list-equal-string aexp1 aexp2))]
                                         [(and (list? aexp1) (boolean? aexp2)) (list env (list-equal-bool aexp1 aexp2))]
                                         [(and (list? aexp1) (null? aexp2)) (list env (list-equal-null aexp1 aexp2))]
                                         [else (list env #f)]
                                         )))
                
                ((my-nequal? program) (list env (not (cadr (value-of (append (list 'equal?) (cdr program)) env)))))
                
                
                
                ((symmetric? program) (let ([cexp (cadr (value-of (cexp->cexp program) env))] )
                                        (cond
                                          [(number? cexp) (list env (* -1 cexp))]                                          
                                          [(boolean? cexp) (list env (not cexp))]
                                          [(list? cexp) (list env (reverse-list cexp))]
                                          
                                          ) 
                                        
      
                                        ))

                ((add? program) (let ([cexp (cadr (value-of (bexp->cexp program) env))] [bexp (cadr (value-of (bexp->bexp program) env))])
                                  (cond
                                    [(and (number? cexp) (number? bexp)) (list env (+ cexp bexp))]
                                    [(and (boolean? cexp) (boolean? bexp)) (list env (or cexp bexp))]
                                    [(and (string? cexp) (string? bexp)) (list env (string-append cexp bexp))]
                                    [(and (number? cexp) (list? bexp)) (list env (add-list-num bexp cexp))]
                                    [(and (list? cexp) (number? bexp)) (list env (add-list-num cexp bexp))]
                                    [(and (boolean? cexp) (list? bexp)) (list env (bool-plus-list bexp cexp))]
                                    [(and (list? cexp) (boolean? bexp)) (list env (bool-plus-list cexp bexp))]
                                    [(and (string? cexp) (list? bexp)) (list env (string-plus-list bexp cexp))]
                                    [(and (list? cexp) (string? bexp)) (list env (string-plus-list cexp bexp))]
                                    [(and (list? cexp) (list? bexp)) (list env (append cexp bexp))]
                                    )
                                  
                                  
                                  
                                  ))
                ((sub? program) (let ([cexp (cadr (value-of (bexp->cexp program) env))] [bexp (cadr (value-of (bexp->bexp program) env))])
                                  (cond
                                    
                                    [(and (number? cexp) (number? bexp)) (list env (- cexp bexp))]                             
                                    [(and (list? cexp) (number? bexp)) (list env (sub-list-num cexp bexp))]
                                    [(and (number? cexp) (list? bexp)) (list env (sub-list-num bexp cexp))]
                                    )
                                  ))
                
                ((mult? program) (let ([cexp (cadr (value-of (bexp->cexp program) env))])
                                   (cond
                                     [(eq? cexp 0) (list env 0)]
                                     [(eq? cexp #f) (list env #f)]
                                     [else (let ([bexp (cadr (value-of (bexp->bexp program) env))]) (cond
                                                                                                      [(and (number? cexp) (number? bexp)) (list env (* cexp bexp))]                                
                                                                                                      [(and (boolean? cexp) (boolean? bexp)) (list env (and cexp bexp))]
                                                                                                      [(and (string? cexp) (string? bexp)) (list env (string-append cexp bexp))]                                
                                                                                                      [(and (number? cexp) (list? bexp)) (list env (mul-list-num bexp cexp))]
                                                                                                      [(and (list? cexp) (number? bexp)) (list env (mul-list-num cexp bexp))]
                                                                                                      [(and (boolean? cexp) (list? bexp)) (list env (bool-mul-list bexp cexp))]
                                                                                                      [(and (list? cexp) (boolean? bexp)) (list env (bool-mul-list cexp bexp))]                             
                                                                                                      ))]
                                     
                                     )                                
                                   ))
                ((div? program) (let ([cexp (cadr (value-of (bexp->cexp program) env))] [bexp (cadr (value-of (bexp->bexp program) env))])
                                  (cond
                                    
                                    [(and (number? cexp) (number? bexp))(cond
                                                                          
                                                                          [(= 0 bexp) (raise "Error! Cannot divide by zero")]
                                                                          [else (list env (/ cexp bexp))]
                                                                          )]                                                            
                                    [(and (list? cexp) (number? bexp)) (list env (div-list-num cexp bexp))]
                                    [(and (number? cexp) (list? bexp)) (list env (div-list-num bexp cexp))]) 
                                  ))
                
                [(var? program) (list env (begin(apply-env (cexp->var program) env)))]
                ;[(var? program) (begin (display env)(display "ata\n\n") (list env  (apply-env (cexp->var program) env)))]
                [(var-listmem? program) (list env (list-index (apply-env (cexp->var program) env) (values (make-indices (cexp->listmem program)) env) ))]
                [(par? program) (value-of (cexp->exp program) env)]

                [(func-call? program) (begin (cond
                                               [(not (eq? (apply-lib-env (func-call->name program) lib-env) "not in env")) (cond [(eq? (func-call->name program) 'eval)  (list env (run (car (func-call->args program))))]
                                                                                                                                 [(eq? (func-call->name program) 'reverse)  (list env (reverse (car (func-call->args program))))]
                                                                                                                                 [(eq? (func-call->name program) 'reverseall)  (list env (reverseall (car (func-call->args program))))]
                                                                                                                                 [(eq? (func-call->name program) 'pow)  (list env (pow (car (func-call->args program)) (cadr (func-call->args program))))]
                                                                                                                                 [(eq? (func-call->name program) 'merge)   (list env (merge (car (cadr (value-of (func-call->args program) env))) (cadr (value-of (cadr (func-call->args program)) env))))]
                                                                                                                                 [(eq? (func-call->name program) 'mergesort)  (list env (mergesort (cadr (value-of (car (func-call->args program)) env))))]
                                                                                                                                 [(eq? (func-call->name program) 'set)  (list env (set (cadr (value-of (car (func-call->args program)) env))))]
                                                                                                                                 [(eq? (func-call->name program) 'makelist)   (list env (makelist (car (cadr (value-of (func-call->args program) env))) (cadr (value-of (cadr (func-call->args program)) env))))]
                                                                                                                                 [else (begin (eval (apply-lib-env (func-call->name program) lib-env))
                                                                                                                                          (list env (eval (cons (func-call->name program)
                                                                                                                                                                (cond
                                                                                                                                                                  ;[(list? (car (func-call->args program))) (list (cons 'list (car (func-call->args program))))]
                                                                                                                                                                  [#t  (quote (func-call->args program))])))))])]
                                               [else (let ([func (apply-env (func-call->name program) env)])
                                               ;(display env) (display "\n\n")  
                                               (list env (cadr (value-of (func->com func) (extend-env (func-call->name program) func (bound (func-call->args program) (func->vars func) (func->env func) env)) 1))))]
                                             ))]
                
                ;[else (list env program)]
                [else (cond
                        [(null? program) program]
                        [else (list env (cons (cadr (value-of (car program) env)) (cadr (value-of (cdr program) env))))])] 
                )])])

    )
  
  )
  )




(define (make-indices indices) (cond [(null?  (cdr indices)) indices]
                                     [else (cons (car indices) (make-indices (cadr indices)))]
                                     ))
(define (list-index-1d lst index) (cond
                                    [(not (number? index)) (raise "Error! Array index must be positive number, not other types.")]
                                    [(null? lst) (raise "Error! Array index out of bound.")]
                                    [(< index 0) (raise "Error! Array index must be positive number, not negative number.")]
                                   
                                    [(= 0 index) (car lst) ]
                                    [else (list-index-1d (cdr lst) (- index 1))]
                                     ))

(define (list-index lst index) (cond [(null? (cdr index)) (list-index-1d lst (car index)) ]
                                     [else (list-index (list-index-1d lst (car index)) (cdr index ))]
                                     ))

(define (values indices env)
  (map (lambda (n) (cadr (value-of n env))) indices))



(define (evaluate path) (with-handlers ([string? (λ (exn) (display exn))]) (run (file->string path))) )
