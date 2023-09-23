#lang eopl


(define length-of-list
  (lambda (list-of-values)
    (if (eqv? list-of-values empty)
        0
        (+ 1 (length-of-list (cdr list-of-values))))))


(define get-element-at-index
  (lambda (list-of-elements index)
    (cond

      [(eqv? index 0)
       (car list-of-elements)]

      [(eqv? (cdr list-of-elements) empty)
       (eopl:error 'OutOfBoundError "Index ~s mayor al tamaño de la lista ~s." index list-of-elements)]
      
      [else
        (get-element-at-index (cdr list-of-elements) (- index 1))])))


(define or-exp
  (lambda (list-of-operands)
    (cond

      [(or
        (not ((list-of number?) list-of-operands))
        (eqv? (length-of-list list-of-operands) 0))
       (eopl:error 'IllegalArgument "Para construir una expresión OR, la lista ~s debe ser no vacía y solo debe contener números" list-of-operands)]

      [(eqv? (length-of-list list-of-operands) 1)
       (cons (car list-of-operands) empty)]

      [else
       (cons
        (car list-of-operands)
        (cons "OR" (or-exp (cdr list-of-operands))))])))


(define or-exp?
  (lambda (exp)
    (cond

      [(and
        (eqv? (length-of-list exp) 1)
        (number? (car exp)))
       #t]

      [(or
        (eqv? (length-of-list exp) 0)
        (not (eqv? (cadr exp) "OR")))
       #f]


      [else
       (and
        (number? (car exp))
        (or-exp? (cddr exp)))])))


(define and-exp
  (lambda (list-of-operands)
    (cond

      [(or
        (not ((list-of or-exp?) list-of-operands))
        (eqv? (length-of-list list-of-operands) 0))
       (eopl:error 'IllegalArgument "Para construir una expresión AND, la lista ~s debe ser no vacía y solo debe contener expresiones OR" list-of-operands)]

      [(eqv? (length-of-list list-of-operands) 1)
       (cons (car list-of-operands) empty)]

      [else
       (cons
        (car list-of-operands)
        (cons "AND" (and-exp (cdr list-of-operands))))])))


;(define and-exp?
 ; (lambda (exp)
  ;  (cond

   ;   [(and
    ;    (eqv? (length-of-list exp) 1)
     ;   (or-exp? exp))
      ;  #t]

      ;[(or
       ; (eqv? (length-of-list exp) 0)
        ;(not (eqv? (cadr exp) "AND")))
        ;#f]

      ;[else
      ; (and
       ; (or-exp? (car exp))
        ;(
      



;(define fnc
 ; (lambda (num-of-var and-statement)
  ;  (list 'FNC num-of-var (and-statement))))







