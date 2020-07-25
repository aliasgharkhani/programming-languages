#lang racket
(provide pow make_list reverse reverse_all set merge merge_sort)


(define pow
  (lambda (a b)
    (expt a b)
    ))

(define make_list
  (lambda (a b)
    (cond
      [(<= a 0) '()]
      [else (cons b (make_list (- a 1) b))]
      )
    ))
(define reverse (lambda (a) (cond [(null? a) a] [else (append (reverse (cdr a)) (list (car a)))])))

(define reverse_all (lambda (a) (cond [(null? a) a] [else (let ((head (car a)) (tail (cdr a))) (append (reverse_all tail) (cond [(list? head) (list (reverse_all head))] [else (list head)]) ))])))

(define set (lambda (a index value) (cond [(null? a) "list index outof range"] [(eq? index 0) (cons value (cdr a))] [else (let ((aux (set (cdr a) (- index 1) value))) (if (eq? aux "list index outof range") "list index outof range" ((cons (car a) aux))))])))

(define merge (lambda (a b) (cond [(null? a) b] [(null? b) a] [(> (car a) (car b)) (cons (car b) (merge a (cdr b)))] [else (cons (car a) (merge (cdr a) b))])))

(define (merge_sort vs) (match vs [(list)  vs] [(list a)  vs] [_  (define-values (lvs rvs) (split-at vs (quotient (length vs) 2))) (merge (merge_sort lvs) (merge_sort rvs))]))


;(define eval
;  (lambda (s)
;    (with-handlers ([string? (λ (exn) (display exn))]) (run s))
;    ))


