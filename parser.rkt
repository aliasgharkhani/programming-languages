#lang racket
(provide my-parser)


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define simple-math-lexer
           (lexer
            ("if" (token-if))
            ("while" (token-while))
            ("do" (token-do))
            ("end" (token-end))
            ("then" (token-then))
            ("else" (token-else))
            ("endif" (token-endif))
            ("return" (token-returnt))
            ("null" (token-null))
            ("true" (token-true))
            ("false" (token-false))
            ("func" (token-func))
            ;("string" (token-string (lexeme)))
            ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-posnumber (string->number lexeme)))
            ((:+ (char-range #\A #\Z) (char-range #\a #\z)) (token-string lexeme))
            ((:: #\" (:+ (:~ #\")) #\") (token-mystring lexeme))
            ("+" (token-pos))
            ("==" (token-eq))
            ("=" (token-eq-assign))
            ("-" (token-neg))
            ("*" (token-mult))
            ("/" (token-div))
            ("<" (token-less))
            (">" (token-more))
            ("!=" (token-neq))
            ("(" (token-lpar))
            (")" (token-rpar))
            ("[" (token-lbrack))
            ("]" (token-rbrack))
            (";" (token-semicolon))
            ("," (token-camma))
            ("\"" (token-qotation))
            ("{" (token-lcbrack))
            ("}" (token-rcbrack))
            ("'" (token-ali))
            
            (whitespace (simple-math-lexer input-port))
            ((eof) (token-EOF))))

(define-tokens a (string posnumber mystring))
(define-empty-tokens b (EOF pos ali neg eq-assign eq mult div less more neq lpar rpar lbrack rbrack if semicolon camma while do end then else endif returnt null true false qotation func lcbrack rcbrack))


(define simple-math-parser
           (parser
            (start command)
            (end EOF)
            (error void)
            (tokens b a)
            (grammar
           (command ((unitcom) (list $1)) ((command semicolon unitcom) (append $1  (list $3))))
           (unitcom ((whilecom) $1) ((ifcom) $1) ((assign) $1) ((return) $1))
           (whilecom ((while exp do command end) (list 'while (list $2) $4)) ((while do command end) (raise "missing conditoin of when")) ((while exp do end) (raise "missing command of when")))
           (ifcom ((if exp then command else command endif) (list 'if $2 $4 $6)) ((if then command else command endif) (raise "missing conditoin of if")) ((if exp then else command endif) (raise "missing first command of if")) ((if exp then command else endif) (raise "missing second command of if")))
           (assign ((string eq-assign exp) (list 'assign (string->symbol $1) $3)) ((string eq-assign function) (list 'assign (string->symbol $1) $3)) ((string eq-assign call) (list 'assign (string->symbol $1) $3)) ((string eq exp) (raise "use = for assigning not ==")))
           (return ((returnt exp) (list 'return $2)))
           (exp ((aexp) $1) ((aexp more aexp) (list 'more? $1 $3)) ((aexp less aexp) (list 'less? $1 $3)) ((aexp eq aexp) (list 'equal? $1 $3)) ((aexp neq aexp) (list 'nequal? $1 $3)))
           (aexp ((bexp neg aexp) (list 'sub $1 $3)) ((bexp pos aexp) (list 'add $1 $3)) ((bexp) $1))
           (bexp ((cexp mult bexp) (list 'mult $1 $3)) ((cexp div bexp) (list 'div $1 $3)) ((cexp) $1))
           (cexp ((neg cexp) (list '- $2)) ((lpar exp rpar) (list 'par $2)) ((posnumber) $1) ((null) 'null) ((string) (list 'var (string->symbol $1))) ((true) '#t) ((false) '#f) ((mystring) (string-replace $1 "\"" "")) ((list) $1) ((string listmem) (list 'list-ref (string->symbol $1) $2)))
           (list ((lbrack listvalues rbrack) $2) ((lbrack rbrack) (list)))
           (listvalues ((exp) (list $1)) ((exp camma listvalues) (append (list $1) $3)))
           (listmem ((lbrack exp rbrack) (list $2)) ((lbrack exp rbrack listmem) (list $2 $4)))
           (function ((func lpar vars rpar lcbrack command rcbrack) (list 'func $3 $6)))
           (vars ((string) (list (string->symbol $1))) ((string camma vars) (append (list (string->symbol $1)) $3)))
           (call ((string lpar args rpar) (list 'call (string->symbol $1) $3)))
           (args ((exp) (list $1)) ((exp camma args) (append (list $1) $3)))
             )))

;
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
;(define my-lexer (lex-this simple-math-lexer (open-input-string "if 1==3 then a=4 else b=0 endif")))
;(let ((parser-res (simple-math-parser my-lexer))) parser-res)
(define (my-parser input-str) (let ((parser-res (simple-math-parser (lex-this simple-math-lexer (open-input-string input-str))))) parser-res)) 
