LittleSECD
==========

LittleSECD machine is implemented in C. My purpose is LittleSECD machine
made with one file, it to compile with one command, then run it quickly,
input directly, and its result show with compiled code.

Compile
-------

    $ make

Test
----

    $ make test

Features
--------

LittleSECD reads expression from standard input, then compiled it, and
show its result.

    $ ./littlesecd
    > (+ 1 2)
    ;; compiled: (ldc 1 ldc 2 add stop)
    3
    >

### Basic Examples

    > (define doubler (lambda (x) (+ x x)))
      ;; compiled: (ldf (ld (0 . 0) ld (0 . 0) add rtn) def doubler stop)
    doubler
    > doubler
      ;; compiled: (ldg doubler stop)
    <closure>
    > (doubler 3)
      ;; compiled: (ldc 3 args 1 ldg doubler app stop)
    6
    > (define fact (letrec fact (fact . (lambda (x)
                                          (if (= x 1)
                                              1
                                            (* x (fact (dec x))))))
                                (dec . (lambda (x)
                                           (- x 1)))))
      ;; compiled: (dum ldf (ld (0 . 0) ldc 1 sub rtn) ldf (ld (0 . 0) ldc 1 neq sel (ldc 1 join) (ld (0 . 0) ld (0 . 0) args 1 ld (1 . 0) app args 1 ld (1 . 1) app mul join) rtn) args 2 ldf (ld (0 . 1) rtn) rap def fact stop)
    fact
    > fact
      ;; compiled: (ldg fact stop)
    <closure>
    > (fact 10)
      ;; compiled: (ldc 10 args 1 ldg fact app stop)
    3628800
    >

### Quote and Bool

'EXPR

    > 'a
      ;; compiled: (ldc a stop)
    a
    > '(1 2 a)
      ;; compiled: (ldc (1 2 a) stop)
    (1 2 a)
    >

T

    > t
      ;; compiled: (ldg t stop)
    t
    > (if t 1 0)
      ;; compiled: (ldg t sel (ldc 1 join) (ldc 0 join) stop)
    1
    >

NIL

    > nil
      ;; compiled: (ldg nil stop)
    nil
    > (if nil 1 0)
      ;; compiled: (ldg nil sel (ldc 1 join) (ldc 0 join) stop)
    0
    >

### Definitions

(define SYMBOL EXPR)

    > (define x (+ 1 2 3))
      ;; compiled: (ldc 1 ldc 2 add ldc 3 add def x stop)
    x
    > (+ x x)
      ;; compiled: (ldg x ldg x add stop)
    12
    > (define fn (lambda (x) (* x x)))
      ;; compiled: (ldf (ld (0 . 0) ld (0 . 0) mul rtn) def fn stop)
    fn
    > (fn 5)
      ;; compiled: (ldc 5 args 1 ldg fn app stop)
    25
    >

### Conditionals

(if EXPR T-CLAUSE F-CLAUSE)

    > (if 1 'a)
      ;; compiled: (ldc 1 sel (ldc a join) (ldg nil join) stop)
    a
    > (if nil 0 (if 'x 'a 'b))
      ;; compiled: (ldg nil sel (ldc 0 join) (ldc x sel (ldc a join) (ldc b join) join) stop)
    a
    >

### Lambda expression

(lambda (VARS) BODY ...)

    > (lambda (x) x)
      ;; compiled: (ldf (ld (0 . 0) rtn) stop)
    <closure>
    > ((lambda () t))
      ;; compiled: (args 0 ldf (ldg t rtn) app stop)
    t
    > ((lambda (a b . c) c) 1 2 3 4)
      ;; compiled: (ldc 1 ldc 2 ldc 3 ldc 4 args 4 ldf (ld (0 . -3) rtn) app stop)
    (3 4)
    > ((lambda a a) 1 2 3)
      ;; compiled: (ldc 1 ldc 2 ldc 3 args 3 ldf (ld (0 . -1) rtn) app stop)
    (1 2 3)
    >

### Let/Letrec

(let BODY (VAR . EXPR) (VAR . EXPR) ...)

    > (let (+ x y) (x . 2) (y . 3))
      ;; compiled: (ldc 3 ldc 2 args 2 ldf (ld (0 . 1) ld (0 . 0) add rtn) app stop)
    5
    >

(letrec BODY (VAR . EXPR) (VAR . EXPR) ...)

    > (letrec (fact 10) (fact . (lambda (x) (if (= x 1) 1 (* x (fact (- x 1)))))))
      ;; compiled: (dum ldf (ld (0 . 0) ldc 1 neq sel (ldc 1 join) (ld (0 . 0) ld (0 . 0) ldc 1 sub args 1 ld (1 . 0) app mul join) rtn) args 1 ldf (ldc 10 args 1 ld (0 . 0) app rtn) rap stop)
    3628800
    >

### Cell operators

(cons X Y)

    > (cons 1 2)
      ;; compiled: (ldc 1 ldc 2 cons stop)
    (1 . 2)
    >

(car CELL)

    > (car (cons (cons 1 2) 3))
      ;; compiled: (ldc 1 ldc 2 cons ldc 3 cons car stop)
    (1 . 2)
    >

(cdr CELL)

    > (cdr (cons 1 3))
      ;; compiled: (ldc 1 ldc 3 cons cdr stop)
    3
    >

### Numeric operators

(+ X Y Z ...)

    > (+)
      ;; compiled: (ldc 0 stop)
    0
    > (+ 1)
      ;; compiled: (ldc 1 stop)
    1
    > (+ 1 2)
      ;; compiled: (ldc 1 ldc 2 add stop)
    3
    > (+ 1 2 3)
      ;; compiled: (ldc 1 ldc 2 add ldc 3 add stop)
    6
    > (+ (+ 1 2) (+ 3 4))
      ;; compiled: (ldc 1 ldc 2 add ldc 3 ldc 4 add add stop)
    10
    >

(- X Y Z ...)

    > (- 1)
      ;; compiled: (ldc -1 stop)
    -1
    > (- -1)
      ;; compiled: (ldc 1 stop)
    1
    > (- 3 5)
      ;; compiled: (ldc 3 ldc 5 sub stop)
    -2
    > (- 3 5 -2)
      ;; compiled: (ldc 3 ldc 5 sub ldc -2 sub stop)
    0
    >

(* X Y Z ...)

    > (*)
      ;; compiled: (ldc 1 stop)
    1
    > (* -1)
      ;; compiled: (ldc -1 stop)
    -1
    > (* 2 3 5)
      ;; compiled: (ldc 2 ldc 3 mul ldc 5 mul stop)
    30
    >

(/ X Y Z ...)

    > (/ 5)
      ;; compiled: (ldc 1 ldc 5 div stop)
    0
    > (/ 10 2)
      ;; compiled: (ldc 10 ldc 2 div stop)
    5
    > (/ 100 10 5)
      ;; compiled: (ldc 100 ldc 10 div ldc 5 div stop)
    2
    >

(% X Y)

    > (% -5 3)
      ;; compiled: (ldc -5 ldc 3 mod stop)
    1
    > (% 5 -3)
      ;; compiled: (ldc 5 ldc -3 mod stop)
    -1
    > (% -5 -3)
      ;; compiled: (ldc -5 ldc -3 mod stop)
    -2
    > (% 5 3)
      ;; compiled: (ldc 5 ldc 3 mod stop)
    2
    >

### Numeric comparisons

(< X Y)

    > (< 2 3)
      ;; compiled: (ldc 2 ldc 3 lt stop)
    t
    > (< 3 2)
      ;; compiled: (ldc 3 ldc 2 lt stop)
    nil
    >

(<= X Y)

    > (<= 3 3)
      ;; compiled: (ldc 3 ldc 3 le stop)
    t
    > (<= 5 2)
      ;; compiled: (ldc 5 ldc 2 le stop)
    nil
    >

(= X Y)

    > (= 3 3)
      ;; compiled: (ldc 3 ldc 3 neq stop)
    t
    > (= 1 2)
      ;; compiled: (ldc 1 ldc 2 neq stop)
    nil
    >

### Atomic predicates

(atom X)

    > (atom 1)
      ;; compiled: (ldc 1 atom stop)
    t
    > (atom 'a)
      ;; compiled: (ldc a atom stop)
    t
    > (atom '(1))
      ;; compiled: (ldc (1) atom stop)
    nil
    >

### Equivalence test operators

(eq X Y)

    > (eq 3 3)
      ;; compiled: (ldc 3 ldc 3 eq stop)
    t
    > (define x 3)
      ;; compiled: (ldc 3 def x stop)
    x
    > (define y 3)
      ;; compiled: (ldc 3 def y stop)
    y
    > (eq x y)
      ;; compiled: (ldg x ldg y eq stop)
    t
    > (eq 'a 'a)
      ;; compiled: (ldc a ldc a eq stop)
    t
    > (eq 'a 'b)
      ;; compiled: (ldc a ldc b eq stop)
    nil
    > (eq '(1 2) '(1 2))
      ;; compiled: (ldc (1 2) ldc (1 2) eq stop)
    nil
    >

### Comments

; STRING

    > (define x (lambda (x)
                ;; any message...
                (+ x x)))
      ;; compiled: (ldf (ld (0 . 0) ld (0 . 0) add rtn) def x stop)
    x
    > (x 5)
      ;; compiled: (ldc 5 args 1 ldg x app stop)
    10
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
