#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

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
            ((:+ (char-range #\a #\z)) (token-var (string->symbol lexeme)))
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
            
            
            (whitespace (simple-math-lexer input-port))
            ((eof) (token-EOF))))

(define-tokens a (var string posnumber))
(define-empty-tokens b (EOF pos neg eq-assign eq mult div less more neq lpar rpar lbrack rbrack if semicolon camma while do end then else endif returnt null true false))


;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "a=3")))
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
