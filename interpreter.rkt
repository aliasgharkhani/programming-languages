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




(define init-env '())
(define (extend-env name value env) (cons (cons name value) env))
(define (apply-env name env) (cond ((assq name env) => cdr) (else #f)))


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
	  ((more? unitcom) '())
      
      )))


(define (evaluate path) (run (file->string path)))
