#lang br/quicklang

(define (indent-counter string) (if (string-prefix? string "  ") (+ 1 (indent-counter (substring string 2))) 0))

(define (line-reader line) ( list (indent-counter line) (format-datum '(~a) line) ) )

(define (merge-lines super-line sub-line) `( ,@super-line ,sub-line))

(define (envelope-line line) (list (- (car line) 1) `(,@(cdr line) ) ))

(define test (map (lambda (line) (line-reader line)) (list "+ 1 2" "  - 3 4" "    * 5 6 (+ 1 2)")))

(define (collapser lines) (match lines
                            ['() '()]
                            [(list-rest (list 0 line) rest) (cons (car lines) (collapser rest))]
                            [(list-rest (list a line-a) (list b line-b) rest) #:when (= (+ 1 b) a) (cons (list b (merge-lines line-b line-a)) rest)]
                            [(list-rest (list a line-a) (list b line-b) rest) #:when (>= b a) (collapser (cons (car lines) (collapser (cdr lines) ) ))]
                            [(list-rest line rest) (collapser (cons (envelope-line line) rest))]))
(define (is-collapsed? lines) (if (empty? lines) #t (if (= 0 (car (car lines)) ) (is-collapsed? (cdr lines)) #f)))
(define (collapse lines) (if (is-collapsed? lines) lines (collapse (collapser lines))))

(define (collapse-at phrase) (match phrase
                               ['() '()]
                               [(list-rest (list-rest '% rest1) rest2) (append (collapse-at rest1) (collapse-at rest2))  ]
                               [(list-rest (list-rest rest1) rest2) (cons (collapse-at rest1) (collapse-at rest2))  ]
                               [_ phrase]))


(define (replace-words string words) (match words
                                       [ '() string ]
                                       [ (list-rest (list a b) rest) (replace-words (string-replace (string-replace string b (string-append b "-1") ) a b) rest)] )) 

(define (collapse-dollar boi) (if (empty? boi) '() (let
                                 ([head (car boi)]
                                  [new-tail (collapse-dollar (cdr boi))])
                                 (if (list? head)
                                     (cons (collapse-dollar head) new-tail )
                                     (case head
                                       [($) (list new-tail)]
                                       [else (cons head new-tail)] )))))

(define keywords (list
                 (list "izznaczi" "define")
                 (list "nehaj" "let")
                 (list "povranty-nehaj" "letrec")
                 (list "ako" "if")
                 (list "uslovje" "cond")
                 (list "sluczaj" "case")
                 (list "i" "and")
                 (list "ili" "or")
                 (list "modul" "modulo") ))
                

(define seek-and-replace (lambda (datums)
  (map (lambda (next) (if (list? next) (seek-and-replace next) (case next
                                                                 [(izznaczi) 'define]
                                                                 [(define) 'define-1]

                                                                 [(izznaczi-sintaksu) 'define-syntax]
                                                                 [(define-syntax) 'define-syntax-1]
                                                                 
                                                                 [(nehaj) 'let]
                                                                 [(let) 'let-1]

                                                                 [(nehaj-sintaksu) 'let-syntax]
                                                                 [(let-syntax) 'let-syntax-1]

                                                                 [(nehaj*) 'let*]
                                                                 [(let*) 'let*-1]

                                                                 [(povratny-nehaj-sintaksu) 'letrec-syntax]
                                                                 [(letrec-syntax) 'letrec-syntax-1]

                                                                 [(pravila-sintaks) 'syntax-rules]
                                                                 [(syntax-rules) 'syntax-rules-1]

                                                                 [(povratny-nehaj) 'letrec]
                                                                 [(letrec) 'letrec-1]

                                                                 [(povratny-nehaj*) 'letrec*]
                                                                 [(letrec*) 'letrec*-1]

                                                                 [(ako) 'if]
                                                                 [(if) 'if-1]

                                                                 [(uslovje) 'cond]
                                                                 [(cond) 'cond-1]

                                                                 [(sluczaj) 'case]
                                                                 [(case) 'case-1]
                                                                 
                                                                 [(i) 'and]
                                                                 [(and) 'and-1]

                                                                 [(ili) 'or]
                                                                 [(or) 'or-1]

                                                                 [(modul) 'modulo]
                                                                 [(modulo) 'modulo-1]

                                                                 [(istinny) '#t]
                                                                 [(#t) 'ttttttet]

                                                                 [(lozsny) '#f]
                                                                 [(#f) 'gerg]

                                                                 [(cituj) 'quote]
                                                                 [(quote) 'quote-1]

                                                                 [(necituj) 'unquute]
                                                                 [(unquote) 'unquote-1]

                                                                 [(pol-cituj) 'quasiquote]
                                                                 [(quasiquote) 'quasiquote-1]

                                                                 [(kolj) 'unquote-splicing]
                                                                 [(unquote-splicing) 'unquote-splicing-1]

                                                                 [(naznaczaj) 'set!]
                                                                 [(set!) 'set!-1]

                                                                 [(odkladaj) 'delay]
                                                                 [(delay) 'delay-1]

                                                                 [(glava) 'car]
                                                                 [(car) 'car-1]

                                                                 [(hvost) 'cdr]
                                                                 [(cdr) 'cdr-1]

                                                                 [(pridaj) 'cons]
                                                                 [(cons) 'cons-1]

                                                                 
                                                                 


                                                                 
                                                                 [else next]
                                                                   )  )) datums)))
;;TODO, make the dollar collapse first, THEN the comma
(define (read-syntax path port)
  (define src-lines  (map (lambda (string) (string-replace string ", " "% ") ) (filter-not (lambda (str) (equal? str "")) (port->lines port)) )   )
  ;;(print src-lines)
  ;;(define src-datums (seek-and-replace (format-datums '~a  src-lines )))  ;;(map (lambda (line) (line-reader line))
  (define parsed-lines (map (lambda (line) (line-reader line)) (reverse src-lines)   ))
  (define collapsed-lines (map (lambda (line) (collapse-at (second line))) (reverse (collapse parsed-lines)))   )
  (define src-datums (collapse-dollar (seek-and-replace  collapsed-lines )))
  
  (define module-datum `(module stacker-mod scheme
                          ,@src-datums))
  (datum->syntax #f module-datum))
(provide read-syntax)