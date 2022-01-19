;;;----------------------------------------------------------------------------

;; Get host version of some primitives before they are modified.

(define _vector         vector)
(define _vector?        vector?)
(define _vector-ref     vector-ref)
(define _vector-set!    vector-set!)

(define _eq?            eq?)
(define _<              <)

(define _+              +)
(define _-              -)
(define _*              *)
(define _quotient       quotient)

(define _read-char      read-char)
(define _peek-char      peek-char)
(define _write-char     write-char)

(define _eof-object?    eof-object?)
(define _char->integer  char->integer)
(define _integer->char  integer->char)

(define _pair?          pair?)
(define _cons           cons)
(define _car            car)
(define _cdr            cdr)
(define _set-car!       set-car!)
(define _set-cdr!       set-cdr!)
(define _cadr           cadr)
(define _caddr          caddr)
(define _map            map)
(define _symbol->string symbol->string)
(define _string->list   string->list)
(define _list->string   list->string)
(define _fold           fold)
(define _reverse        reverse)
(define _length         length)
(define _list-ref       list-ref)
(define _equal?         equal?)

;;;----------------------------------------------------------------------------

;; Redefine the conditional special forms so that they work with the
;; false value of the uVM.

(define-macro (if x y z)
  `(##if (_eq? ,x false) ,z ,y))

(define-macro (and x . rest)
  (if (null? rest)
      x
      `(if ,x (and ,@rest) false)))

(define-macro (or x . rest)
  (if (null? rest)
      x
      `(let ((_ ,x))
         (if _ _ (or ,@rest)))))

(define-macro (cond . clauses)
  (if (null? clauses)
      `false
      (let ((clause (car clauses)))
        (if (eq? (car clause) 'else)
            `(begin ,@(cdr clause))
            `(if ,(car clause)
                 (begin ,@(cdr clause))
                 (cond ,@(cdr clauses)))))))

(define-macro (export . rest)
  `(begin))

(define-macro (quote x)
  (cond ((symbol? x) `(string->symbol ',(symbol->string x)))
        ((string? x) `(list->string ,(fold (lambda (c tail) `(cons ',c ,tail))
                                           'null
                                           (reverse (string->list x)))))
        ((char? x)   (char->integer x))
        ((eq? x #f)  `false)
        ((eq? x #t)  `true)
        ((eq? x '()) `null)
        (else        `(##quote ,x))))

;;;----------------------------------------------------------------------------

;; Low-level operations supported by the uVM.

(define (cons field0 field1) (_vector field0 field1 0))
(define (clump? o) (##if (_vector? o) true false))
(define (field0 o) (_vector-ref o 0))
(define (field1 o) (_vector-ref o 1))
(define (field2 o) (_vector-ref o 2))
(define (field0-set! o x) (_vector-set! o 0 x) o)
(define (field1-set! o x) (_vector-set! o 1 x) o)
(define (field2-set! o x) (_vector-set! o 2 x) o)

(define (eq? x y) (##if (_eq? x y) true false))
(define (< x y) (##if (_< x y) true false))

(define (+ x y) (_+ x y))
(define (- x y) (_- x y))
(define (* x y) (_* x y))
(define (quotient x y) (_quotient x y))

(define (getchar)
  (let ((c (_read-char)))
    (##if (_eof-object? c)
          -1
          (_char->integer c))))

(define (putchar c)
  (_write-char (_integer->char c)))

;;;----------------------------------------------------------------------------
