LittleSECD
==========

LittleSECD machine is implemented in C. My purpose is LittleSECD machine made with one file, it to compile with one command, then run it quickly, input directly, and its result show.

Compile
-------

    $ make

or

    $ cc littlesecd.c -o littlesecd

Test
----

    $ make test

or

    $ ksh test.ksh

Basic Example
-------------

LittleSECD reads expression from standard input, then compiled it, and show its result.

    $ ./littlesecd
    
    > (define list (lambda (a . b)
        (cons a b)))
    list
    
    > (disassemble list)
    (ldf (ld (0 . 0) ld (0 . -2) cons rtn))
    
    > (defmacro my-let (var val . body)
        (cons (cons 'lambda (cons (list var) body)) (list val)))
    my-let
    
    > (defmacro or (expr . rest)
        (if rest
            (my-let var (gensym)
                  (list 'my-let var expr
                        (list 'if var var (cons 'or rest))))
          expr))
    or
    
    > (my-let x 3 (+ x x))
    6
    
    > (or nil nil 3)
    3
    
    > (macroexpand (my-let x 3 (+ x x)))
    ((lambda (x) (+ x x)) 3)
    
    > (disassemble (my-let x 3 (+ x x)))
    (ldc 3 args 1 ldf (ld (0 . 0) ld (0 . 0) add rtn) app stop)
    
    > (macroexpand (or nil nil 1))
    (my-let G__0 nil (if G__0 G__0 (or nil 1)))
    
    > (macroexpand (my-let G__0 nil (if G__0 G__0 (or nil 1))))
    ((lambda (G__0) (if G__0 G__0 (or nil 1))) nil)
    
    > (disassemble ((lambda (G__0) (if G__0 G__0 (or (+ 3 4)))) (+ 1 2)))
    (ldc 1 ldc 2 add args 1 ldf (ld (0 . 0) sel (ld (0 . 0) join) (ldc 3 ldc 4 add join) rtn) app stop)
    
    > (let (+ x y) (x . 1)
                   (y . 2))
    3
    
    > (letrec (fact 10) (fact . (lambda (x)
                                  (if (= x 1)
                                      1
                                    (* x (fact (dec x))))))
                        (dec  . (lambda (x)
                                  (- x 1))))
    3628800
    
    > (define fact (lambda (x)
                     (if (= x 1)
                         1
                       (* x (fact (- x 1))))))
    fact
    
    > (disassemble (fact 10))
    (ldc 10 args 1 ldg fact app stop)
    
    > (disassemble fact)
    (ldf (ld (0 . 0) ldc 1 neq sel (ldc 1 join) (ld (0 . 0) ld (0 . 0) ldc 1 sub args 1 ldg fact app mul join) rtn))
    
    > (quit)
    $

Features
--------

### Quote and Bool

'EXPR

    > 'a
    a
    > '(1 2 a)
    (1 2 a)
    >

T

    > t
    t
    > (if t 1 0)
    1
    >

NIL

    > nil
    nil
    > (if nil 1 0)
    0
    >

### Definitions

(define SYMBOL EXPR)

    > (define x (+ 1 2 3))
    x
    > (+ x x)
    12
    > (define fn (lambda (x) (* x x)))
    fn
    > (fn 5)
    25
    >

### Conditionals

(if EXPR T-CLAUSE F-CLAUSE)

    > (if 1 'a)
    a
    > (if nil 0 (if 'x 'a 'b))
    a
    >

### Lambda expression

(lambda (VARS) BODY ...)

    > (lambda (x) x)
    <closure>
    > ((lambda () t))
    t
    > ((lambda (a b . c) c) 1 2 3 4)
    (3 4)
    > ((lambda a a) 1 2 3)
    (1 2 3)
    >

### Let/Letrec

(let BODY (VAR . EXPR) (VAR . EXPR) ...)

    > (let (+ x y) (x . 2) (y . 3))
    5
    >

(letrec BODY (VAR . EXPR) (VAR . EXPR) ...)

    > (letrec (fact 10) (fact . (lambda (x) (if (= x 1) 1 (* x (fact (- x 1)))))))
    3628800
    >

### Cell operators

(cons X Y)

    > (cons 1 2)
    (1 . 2)
    >

(car CELL)

    > (car (cons (cons 1 2) 3))
    (1 . 2)
    >

(cdr CELL)

    > (cdr (cons 1 3))
    3
    >

### Numeric operators

(+ X Y Z ...)

    > (+)
    0
    > (+ 1)
    1
    > (+ 1 2)
    3
    > (+ 1 2 3)
    6
    > (+ (+ 1 2) (+ 3 4))
    10
    >

(- X Y Z ...)

    > (- 1)
    -1
    > (- -1)
    1
    > (- 3 5)
    -2
    > (- 3 5 -2)
    0
    >

(* X Y Z ...)

    > (*)
    1
    > (* -1)
    -1
    > (* 2 3 5)
    30
    >

(/ X Y Z ...)

    > (/ 5)
    0
    > (/ 10 2)
    5
    > (/ 100 10 5)
    2
    >

(% X Y)

    > (% -5 3)
    1
    > (% 5 -3)
    -1
    > (% -5 -3)
    -2
    > (% 5 3)
    2
    >

### Numeric comparisons

(< X Y)

    > (< 2 3)
    t
    > (< 3 2)
    nil
    >

(<= X Y)

    > (<= 3 3)
    t
    > (<= 5 2)
    nil
    >

(= X Y)

    > (= 3 3)
    t
    > (= 1 2)
    nil
    >

### Atomic predicates

(atom X)

    > (atom 1)
    t
    > (atom 'a)
    t
    > (atom '(1))
    nil
    >

### Equivalence test operators

(eq X Y)

    > (eq 3 3)
    t
    > (define x 3)
    x
    > (define y 3)
    y
    > (eq x y)
    t
    > (eq 'a 'a)
    t
    > (eq 'a 'b)
    nil
    > (eq '(1 2) '(1 2))
    nil
    >

### Comments

; STRING

    > (define x (lambda (x)
                ;; any message...
                (+ x x)))
    x
    > (x 5)
    10
    >

### Macro and Disassemble

(defmacro SYMBOL (SYMBOL ...) EXPR ...)

    ;;
    ;; my-let
    ;;
    > (define list (lambda (x . y) (cons x y)))
    list
    > (defmacro my-let (var val . body)
        (cons (cons 'lambda (cons (list var) body)) (list val)))
    my-let
    > (my-let x 3 (+ x x))
    6
    > ((lambda (x) (my-let x 3 (+ x x))) 8192)
    6
    > (define x 8192)
    x
    > (my-let x 3 (+ x x))
    6
    ;;
    ;; or
    ;;
    > (defmacro or (expr . rest)
          (if rest
              (my-let var (gensym)
                    (list 'my-let var expr
                          (list 'if var var (cons 'or rest))))
            expr))
    or
    > (or nil nil 3)
    3
    > 

(macroexpand EXPR)

    ;;
    ;; my-let
    ;;
    > (define list (lambda (x . y) (cons x y)))
    list
    > (defmacro my-let (var val . body)
        (cons (cons 'lambda (cons (list var) body)) (list val)))
    my-let
    > (macroexpand (my-let x 3 (+ x x)))
    ((lambda (x) (+ x x)) 3)
    >
    ;;
    ;; or
    ;;
    > (defmacro or (expr . rest)
          (if rest
              (my-let var (gensym)
                    (list 'my-let var expr
                          (list 'if var var (cons 'or rest))))
            expr))
    or
    > (macroexpand (or nil nil 1))
    (my-let G__0 nil (if G__0 G__0 (or nil 1)))
    > (macroexpand (my-let G__0 nil (if G__0 G__0 (or nil 1))))
    ((lambda (G__0) (if G__0 G__0 (or nil 1))) nil)
    > 

(disassemble EXPR)

    ;;
    ;; can't disassemble the symbol of binding to <primitive function> or <special form>
    ;;
    $ ./littlesecd
    > (disassemble +)
    The head of a list mube be a function or macro
    $ ./littlesecd
    > (disassemble if)
    The head of a list mube be a function or macro
    $
    ;;
    ;; disassemble an expression
    ;;
    $ ./littlesecd
    > (disassemble (+ 1 2 3))
    (ldc 1 ldc 2 add ldc 3 add stop)
    > (disassemble (if 1 2 3))
    (ldc 1 sel (ldc 2 join) (ldc 3 join) stop)
    ;;
    ;; disassemble the symbol of binding to function
    ;;
    $ ./littlesecd
    > (define fn (lambda (x) (+ x x)))
    fn
    > (disassemble fn)
    (ldf (ld (0 . 0) ld (0 . 0) add rtn))
    >
    ;;
    ;; disassemble the symbol of binding to macro
    ;;
    $ ./littlesecd
    > (define list (lambda (x . y) (cons x y)))
    list
    > (defmacro my-let (var val . body)
        (cons (cons 'lambda (cons (list var) body)) (list val)))
    my-let
    > (disassemble my-let)
    (ldm (ldc lambda ld (0 . 0) args 1 ldg list app ld (0 . -3) cons cons ld (0 . 1) args 1 ldg list app cons rtn))
    >

### Acknowledgments

This program made based on minilisp (nogc version) and micro scheme.

minilisp (nogc version):
https://github.com/rui314/minilisp/blob/nogc/minilisp.c

micro scheme:
http://www.nct9.ne.jp/m_hiroi/func/abcscm33.html

And minilisp also has gc version.

minilisp:
https://github.com/rui314/minilisp

I'd like to take this opportunity to thank you all.
