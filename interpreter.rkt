(define if-com
  (lambda () ()))

(define while-com?
  (lambda (unit-com) (eqv? (car(unit-com)) 'while)))

(define if-com?
  (lambda (unit-com) (eqv? (car(unit-com)) 'if)))

(define assign?
  (lambda (unit-com) (eqv? (car(unit-com)) 'assign)))

(define assign?
  (lambda (unit-com) (eqv? (car(unit-com)) 'assign)))

(define return?
  (lambda (unit-com) (eqv? (car(unit-com)) 'return)))

(define more?
  (lambda (exp) (eqv? (car(exp)) 'more?)))

(define less?
  (lambda (exp) (eqv? (car(exp)) 'less?)))

(define equal?
  (lambda (exp) (eqv? (car(exp)) 'equal?)))

(define nequal?
  (lambda (exp) (eqv? (car(exp)) 'nequal?)))

