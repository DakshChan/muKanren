#lang racket/base

(define (var c) (box c)) ; like a vector, except only 1 element, don't know why we need a full vector
(define (var? x) (box? x))
(define (var=? x1 x2) (= (unbox x1) (unbox x2))) ; box makes this make a bit more sense since we don't need to access 0th index

; repeatedly following subsitutions, to determine if u equals v, under the substitutions given in s
(define (walk u s)
    (let 
        ((pr (and (var? u) (hash-ref s u u)))) ; switched to use a hashmap, makes it easier to read
        (if (== pr u) u (walk pr s)) ; still able to walk; #f is a potential value
    )
)

; extends the substitutions
(define (ext-s x v s) (hash-set s x v))

; s/c : substitution and variable counter

; goals are functions that take a state, and return a stream of states
(define (== u v)
    (lambda (s/c)
        (let 
            ((s (unify u v (car s/c)))) ; tries to unify u and v
            (if s (unit `(, s . , (cdr s/c))) mzero) ; if unify succeeds, return the new state otherwise return mzero
        )
    )
) ; could we unify goals with this implementation?
        ; we could, but it's just based on equality of the statment, so it's not actually interpreting the goals

; stream with a single value
(define (unit s/c) (cons s/c mzero))

; empty stream
(define mzero '())

; comes up with a substitution that ensures u and v are equal, or dies trying
(define (unify u v s)
    (let 
        ((u (walk u s))
         (v (walk v s)))
        (cond
            ((and (var? u) (var? v) (var=? u v))
             s
            )
            ((var? u)
                (ext-s u v s)
            )
            ((var? v)
                (ext-s v u s)
            )
            ((and (pair? u) (pair? v))
                (let 
                    ((s (unify (car u) (car v) s)))
                    (and s (unify (cdr u) (cdr v) s))
                )
            )
            (else 
                (and (eqv? u v) s)
            )
        )
    )
)

(define (call/fresh f )
    (lambda (s/c)
        (let 
            ((c (cdr s/c)))
            ((f (var c)) `(, (car s/c) . , (+ c 1))) ; takes new goal and applies substitution with incremented counter
        )
    )
)

(define (disj g1 g2) 
    (lambda (s/c) 
        (mplus (g1 s/c) (g2 s/c))
    )
)

(define (conj g1 g2)
    (lambda (s/c)
        (bind (g1 s/c) g2)
    )
)

; combines streams like append
(define (mplus $1 $2)
    (cond
        ((null? $1) $2)
        ((procedure? $1) (lambda () (mplus $2 ($1))))
        (else (cons (car $1) (mplus (cdr $1) $2)))
    )
)

; first state applied to goal, then second state applied to goal, so on, all appended together
(define (bind $ g)
    (cond
        ((null? $) mzero)
        ((procedure? $) (lambda () (bind ($) g)))
        (else (mplus (g (car $)) (bind (cdr $) g)))
    )
)

; testing stuffs

(define empty-state (cons (hash) 0))

; from paper, should be finite stream, so original mplus should work
(define a-and-b
    (conj
        (call/fresh (lambda (a) (== a 7)))
        (call/fresh (lambda (b) (disj (== b 5) (== b 6))))
    )
)

(a-and-b empty-state)

;infinite stream, wrapping the serious call in an inverse n delay
(define (fives x)
    (disj (== x 5) (lambda (s/c) (lambda () ((fives x) s/c)))))

((call/fresh fives) empty-state)

(define (sixes x)
    (disj (== x 6) (lambda (s/c) (lambda () ((sixes x) s/c)))))

(define fives-and-sixes
    (call/fresh (lambda (x) (disj (fives x) (sixes x)))))

(fives-and-sixes empty-state) ; 5
((cdr (fives-and-sixes empty-state))) ; 6
((cdr ((cdr (fives-and-sixes empty-state)))))
((cdr ((cdr ((cdr (fives-and-sixes empty-state)))))))

;five-and-sixes