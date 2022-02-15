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
run integer 0 0
run integer 1 1
run integer -1 -1
run symbol a "'a"
run quote a '(quote a)'
run quote 63 "'63"
run quote '(+ 1 2)' "'(+ 1 2)"
run "+" 0 '(+)'
run "+" 0 '(+ 0 0 0)'
run "+" 0 '(+ (+ 1 -1) 0)'
run "+" 55 '(+ 1 2 3 4 5 6 7 8 9 (+ 5 5))'
run "-" 0 '(- 0 0)'
run "-" 1 '(- 0 -1)'
run "-" -1 '(- -1 0)'
run "-" -4 '(- 3 5 2)'
run "*" 1 '(*)'
run "*" 0 '(* 0 -5)'
run "*" 256 '(* 2 2 2 2 2 2 2 2)'
run "*" -5 '(* 1 -5)'
run "/" 0 '(/ 5)'
run "/" 0 '(/ 0 3)'
run "/" -2 '(/ -10 5)'
run "/" -10 '(/ 100 -2 5)'
run "/" 50 '(/ 100 2 1)'
run "%" 2 '(% 5 3)'
run "%" 1 '(% -5 3)'
run "%" -1 '(% 5 -3)'
run "%" -2 '(% -5 -3)'
run "%" 0 '(% 0 -1)'
run "%" 0 '(% 0 5)'
run 'literal list' '(a b c)' "'(a b c)"
run 'literal list' '(a b . c)' "'(a b . c)"

# Comments
run comment 5 "
  ; 2
  5 ; 3"

# Global variables
run define 7 '(define x 7) x'
run define 10 '(define x 7) (+ x 3)'
run define 7 '(define + 7) +'
run define 11 '(define x 7) (define x 11) x'

# Conditionals
run if a "(if 1 'a)"
run if nil "(if nil 'a)"
run if a "(if 1 'a 'b)"
run if a "(if 0 'a 'b)"
run if a "(if 'x 'a 'b)"
run if b "(if nil 'a 'b)"
run if c "(if nil 'a (if t 'c))"
run if c "(if t (if nil 'a 'c))"
run if 5 '(if t (+ 2 3))'
run if 5 '(if t (if t 5))'
run if 8 '(if t (if nil (if t 5) (+ 3 5)))'

# Numeric comparisons
run = t '(= 3 (+ 1 2))'
run = nil '(= (+ 3 5) 1)'
run "<" t '(< (+ 1 2) 5)'
run "<" nil '(< 5 (+ 1 1))'
run "<=" t '(<= 5 (+ 2 3))'
run "<=" nil '(<= (+ 2 3) 3)'

# Atomic predicates
run atom t '(atom 1)'
run atom t "(atom 'a)"
run atom nil "(atom '(1))"
run atom nil "(atom '(1 2))"
run atom t '(atom (if t 1))'

# Equality comparison
run eq t '(eq 3 3)'
run eq t '(define x 3) (define y 3) (eq x y)'
run eq t '(define x 3) (define x 8) (eq x x)'
run eq t "(eq 'a 'a)"
run eq nil "(eq 'a 'b)"
run eq nil "(eq '(1 2) '(1 2))"

# Functions
run lambda '<closure>' '(lambda a a)'
run lambda t '((lambda () t))'
run lambda 15 '((lambda (x) (+ x x x)) 5)'
run lambda '(1 2 3)' '((lambda a a) 1 2 3)'
run lambda '(2 3 4)' '((lambda (a . b) b) 1 2 3 4)'

# Lexical closures
run closure 3 '(define call (lambda (f) ((lambda (var) (f)) 5)))
               ((lambda (var) (call (lambda () var))) 3)'

# Let
run let 7 '(let (+ x y) (x . 3) (y . 4))'
run let '(3 . 5)' '(let (cons y x) (x . 5) (y . 3))'

# Cons
run cons '(1 . 2)' '(cons 1 2)'
run cons '((1 . 2) . 3)' '(cons (cons 1 2) 3)'
run cons '((1 . 2) 3 . 5)' '(cons (cons 1 2) (cons 3 5))'

# Car
run car 1 '(car (cons 1 2))'
run car '(1 . 2)' '(car (cons (cons 1 2) 3))'

# Cdr
run cdr 2 '(cdr (cons 1 2))'
run cdr 3 '(cdr (cons (cons 1 2) 3))'
run cdr '(3 . 4)' '(cdr (cons (cons 1 2) (cons 3 4)))'

# Letrec
run letrec 3628800 '(letrec (fact 10)
                       (dec . (lambda (x) (- x 1)))
                       (fact . (lambda (x) (if (= x 1) 1 (* x (fact (dec x)))))))'
run letrec 3628800 '(define fn (letrec fn (fn . (lambda (x) (if (= x 1)
                                                                1
                                                              (* x (fn (dec x))))))
                                           (dec . (lambda (x) (- x 1)))))
                    (fn 10)'
