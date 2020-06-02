#lang racket


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
            ("string" (token-string (lexeme)))
            ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-posnumber (string->number lexeme)))
            ((:+ (char-range #\a #\z)) (token-var lexeme))
            ("+" (token-plus))
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
            
            
            (whitespace (simple-math-lexer input-port))
            ((eof) (token-EOF))))

(define-tokens a (var string posnumber))
(define-empty-tokens b (EOF plus neg eq-assign eq mult div less more neq lpar rpar lbrack rbrack if semicolon camma while do end then else endif returnt null true false))


(define simple-math-parser
           (parser
            (start command)
            (end EOF)
            (error void)
            (tokens b a)
            (grammar
           (command ((unitcom) $1) ((command semicolon unitcom) (list $1  $3)))
           (unitcom ((whilecom) $1) ((ifcom) $1) ((assign) $1) ((return) $1))
           (whilecom ((while exp do command end) (list 'while ($2) ($4))))
           (ifcom ((if exp then command else command endif) (list 'if (list $2) (list $4) (list $6))))
           (assign ((var eq-assign exp) (list 'define $1 $3)))
           (return ((returnt exp) $2))
           (exp ((aexp) $1) ((aexp more aexp) (list '> $1 $3)) ((aexp less aexp) (list '< $1 $3)) ((aexp eq aexp) (list 'eq? $1 $3)) ((aexp neq aexp) (list 'neq? $1 $3)))
           (aexp ((bexp) $1) ((bexp neg aexp) (list '- $1 $3)) ((bexp plus aexp) (list '+ $1 $3)))
           (bexp ((cexp) $1) ((cexp mult bexp) (list '* $1 $3)) ((cexp div bexp) (list '/ $1 $3)))
           (cexp ((neg cexp) (list '- 0 $2)) ((lpar exp rpar) (list $2)) ((posnumber) $1) ((null) 'null) ((var) $1) ((true) '#t) ((false) '#f) ((string) $1) ((list) $1) ((var listmem) (list 'list-ref $1 $2)))
           (list ((lbrack listvalues rbrack) (list $2)) ((lbrack rbrack) (list)))
           (listvalues ((exp) $1) ((exp camma listvalues) (list $1 $3)))
           (listmem ((lbrack exp rbrack) (list $2)) ((lbrack exp rbrack listmem) (list $2 $4)))
             )))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "b = a[9][8][7]")))
(let ((parser-res (simple-math-parser my-lexer))) parser-res)

