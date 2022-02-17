#!/bin/ksh

function fail {
    echo -n '[ERROR] '
    echo "$1"
    exit 1
}

function do_run {
    error=$(echo "$3" | ./littlesecd 2>&1 > /dev/null)
    if [[ -n "$error" ]]; then
        echo FAILED
        fail "$error"
    fi

    result=$(echo "$3" | ./littlesecd 2> /dev/null | tail -2 | head -1)
    if [[ "$result" != "$2" ]]; then
        echo FAILED
        fail "$2 expected, but got $result"
    fi
}

function run {
    echo -n "Testing $1 ..."
    do_run "$@"
    echo ok
}

# Basic data types
run integer  0 0
run integer  1 1
run integer -1 -1

run quote a "'a"
run quote a '(quote a)'

run 'literal list' '(1 b c)' "'(1 b c)"
run 'literal list' '(a b . c)' "'(a b . c)"
run 'literal list' '(a 2 c)' "'(a  2   c)"

run '+'  0 '(+)'
run '+'  0 '(+ 0 0)'
run '+'  1 '(+ 0 1)'
run '+'  1 '(+ 1 0)'
run '+'  2 '(+ 1 1)'
run '+' -5 '(+ -3 -2)'
run '+' -6 '(+ -3 -2 -1)'
run '+'  0 '(+ (+) (+ -1 1) (+ 1 -1))'
run '+' 15 '(+ 1 2 3 4 5)'

run '-'  0 '(- 0)'
run '-'  0 '(- -0)'
run '-' -1 '(- 1)'
run '-'  1 '(- -1)'
run '-'  0 '(- 0 0)'
run '-' -1 '(- 0 1)'
run '-'  1 '(- 1 0)'
run '-'  0 '(- 1 1)'
run '-'  4 '(- -1 -2 -3)'
run '-'  0 '(- -3 -2 -1)'
run '-'  0 '(- (- 0) (- 2 1) (- 1 2))'
run '-' -5 '(- 5 4 3 2 1)'

run '*'  1 '(*)'
run '*'  0 '(* 0)'
run '*'  1 '(* 1)'
run '*' -1 '(* -1)'
run '*'  0 '(* 0 0)'
run '*'  0 '(* 0 1)'
run '*'  0 '(* 1 0)'
run '*'  1 '(* 1 1)'
run '*'  6 '(* -3 -2)'
run '*' -6 '(* -3 -2 -1)'
run '*' 25 '(* 5 5 (*))'
run '*' 32 '(* 2 2 2 2 2)'

run '/'   1 '(/ 1)'
run '/'  -1 '(/ -1)'
run '/'   0 '(/ 2)'
run '/'   0 '(/ 0 1)'
run '/'   2 '(/ 10 5)'
run '/' -10 '(/ 100 -2 5)'
run '/'  10 '(/ 100 -2 -5)'
run '/'  50 '(/ 100 2 (/ 1))'
run '/'   2 '(/ 256 2 2 2 2 2 2 2)'

run '%'  2 '(%  5  3)'
run '%'  1 '(% -5  3)'
run '%' -1 '(%  5 -3)'
run '%' -2 '(% -5 -3)'
run '%'  0 '(%  0  3)'
run '%'  0 '(%  0 -3)'
run '%'  1 '(% 5 (% 5 3))'
run '%'  2 '(% (% 5 3) 5)'

# Comments
run comment 5 "
  ; 2
  5 ; 3"

# Global variables
run define  7 '(define x 7) x'
run define 10 '(define x 7) (+ x 3)'
run define  7 '(define + 7) +'
run define 11 '(define x 7) (define x 11) x'

# Conditionals
run if   a "(if 0 'a)"
run if   a "(if 1 'a)"
run if   a "(if 'x 'a)"
run if nil "(if nil 'a)"
run if   a "(if (if t t) 'a)"
run if   a "(if (if t t) (if t 'a))"
run if   a "(if 0 'a 'b)"
run if   a "(if 1 'a 'b)"
run if   a "(if 'x 'a 'b)"
run if   b "(if nil 'a (if nil 'a 'b))"
run if   a "(if (if t t) 'a 'b)"
run if   a "(if (if t t) (if t 'a) 'b)"
run if   b "(if (if t nil) 'a 'b)"

# Numeric comparisons
run =   t '(= 0 0)'
run =   t '(= -0 0)'
run =   t '(= 0 -0)'
run =   t '(= -0 -0)'
run =   t '(= 1 1)'
run = nil '(= 1 2)'
run = nil '(= 2 1)'
run =   t '(= 1 (+ 1 0))'
run =   t '(= (+ 1 0) 1)'
run = nil '(= 1 (+ 1 1))'
run = nil '(= (+ 1 1) 1)'
run =   t '(= (+ 1 1) (+ 1 1))'
run = nil '(= (+ 1 1) (+ 2 2))'

run "<" nil '(< 0 0)'
run "<" nil '(< -0 0)'
run "<" nil '(< 0 -0)'
run "<" nil '(< -0 -0)'
run "<" nil '(< 1 0)'
run "<"   t '(< 0 1)'
run "<"   t '(< 1 (+ 1 1))'
run "<" nil '(< (+ 1 1) 1)'
run "<"   t '(< (+ 1 2) (+ 3 4))'
run "<" nil '(< (+ 3 4) (+ 1 2))'

run "<="   t '(<= 0 0)'
run "<="   t '(<= -0 0)'
run "<="   t '(<= 0 -0)'
run "<="   t '(<= -0 -0)'
run "<="   t '(<= 1 1)'
run "<="   t '(<= 1 2)'
run "<=" nil '(<= 2 1)'
run "<=" nil '(<= (+ 1 1) 0)'
run "<="   t '(<= (+ 1 1) 2)'
run "<="   t '(<= (+ 1 1) 3)'
run "<="   t '(<= 0 (+ 1 1))'
run "<="   t '(<= 2 (+ 1 1))'
run "<=" nil '(<= 3 (+ 1 1))'
run "<="   t '(<= (+ 1 1) (+ 1 1))'
run "<="   t '(<= (+ 1 1) (+ 1 2))'
run "<=" nil '(<= (+ 1 2) (+ 1 1))'

# Atomic predicates
run atom   t '(atom 1)'
run atom   t "(atom 'a)"
run atom nil "(atom '(1))"
run atom nil "(atom '(1 2))"
run atom   t '(atom (if t 1))'

# Equality comparison
run eq   t '(eq 3 3)'
run eq   t '(define x 3) (define y 3) (eq x y)'
run eq   t '(define x 3) (define x 8) (eq x x)'
run eq   t "(eq 'a 'a)"
run eq nil "(eq 'a 'b)"
run eq nil "(eq '(1 2) '(1 2))"

# Functions
run lambda '<closure>' '(lambda a a)'
run lambda           t '((lambda () t))'
run lambda          15 '((lambda (x) (+ x x x)) 5)'
run lambda   '(1 2 3)' '((lambda a a) 1 2 3)'
run lambda   '(2 3 4)' '((lambda (a . b) b) 1 2 3 4)'

# Lexical closures
run closure 3 '(define call (lambda (f) ((lambda (var) (f)) 5)))
               ((lambda (var) (call (lambda () var))) 3)'

# Let
run let         7 '(let (+ x y) (x . 3) (y . 4))'
run let         6 '(let (+ x y) (x . (+ 1 1)) (y . 4))'
run let         5 '(let (+ x y) (x . 3) (y . (+ 1 1)))'
run let         4 '(let (+ x y) (x . (+ 1 1)) (y . (+ 1 1)))'
run let '(3 . 5)' '(let (cons y x) (x . 5) (y . 3))'

# Cons
run cons '(1 . 2)'         '(cons 1 2)'
run cons '((1 . 2) . 3)'   '(cons (cons 1 2) 3)'
run cons '(3 1 . 2)'       '(cons 3 (cons 1 2))'
run cons '((1 . 2) 3 . 5)' '(cons (cons 1 2) (cons 3 5))'

# Car
run car         1 '(car (cons 1 2))'
run car '(1 . 2)' '(car (cons (cons 1 2) 3))'

# Cdr
run cdr         2 '(cdr (cons 1 2))'
run cdr         3 '(cdr (cons (cons 1 2) 3))'
run cdr '(3 . 4)' '(cdr (cons (cons 1 2) (cons 3 4)))'

# Letrec
run letrec 3628800 '(letrec (fact 10)
                      (dec . (lambda (x) (- x 1)))
                      (fact . (lambda (x) (if (= x 1)
                                              1
                                            (* x (fact (dec x)))))))'
run letrec 3628800 '(define fn
                      (letrec fn
                        (fn . (lambda (x) (if (= x 1)
                                              1
                                            (* x (fn (dec x))))))
                        (dec . (lambda (x) (- x 1)))))
                    (fn 10)'
