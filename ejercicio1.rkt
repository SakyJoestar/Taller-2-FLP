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


(define or-exp?
  (lambda (exp)
    (cond
      
      [(not (list? exp))
       #f]

      [(null? exp)
       #t]

      [(and
        (eqv? (length-of-list exp) 1)
        (number? (car exp)))
       #t]

      [else
       (and
        (number? (car exp))
        (eqv? (cadr exp) "OR")
        (not (null? (cddr exp)))
        (or-exp? (cddr exp)))])))

;; Pruebas
(eqv? (or-exp? '()) #t)
(eqv? (or-exp? '(1)) #t)
(eqv? (or-exp? '(1 "OR" -1)) #t)
(eqv? (or-exp? '(1 "OR" -1 "OR" -5)) #t)
(eqv? (or-exp? '(1 "OR" -1 -5)) #f)
(eqv? (or-exp? '(1 "OR" -1 "OR" (-5 "OR" 2))) #f)
(eqv? (or-exp? '(1 "OR")) #f)


(define or-exp
  (lambda (num exp)
    (cond
      
      [(not (number? num))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser un número" num)]

      [(not (or-exp? exp))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una expresion OR" exp)]

      [(null? exp)
       (cons num empty)]

      [else
       (cons num
             (cons "OR" exp))])))

;; Pruebas
(define or-exp1 (or-exp 1 empty))
(define or-exp2 (or-exp 1 (or-exp 2 empty)))
(define or-exp3 (or-exp 1 (or-exp 2 (or-exp -3 empty))))
;(define or-exp4 (or-exp 1 (or-exp 2 (or-exp -3 4))))


(define or-exp->varlist
  (lambda (exp)
    (cond
      
      [(not (or-exp? exp))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una expresion OR" exp)]

      [(eqv? (length-of-list exp) 1)
       (cons (car exp) empty)]

      [else
       (cons (car exp) (or-exp->varlist (cddr exp)))])))

;; Pruebas
(or-exp->varlist or-exp1)
(or-exp->varlist or-exp2)
(or-exp->varlist or-exp3)


(define and-exp?
  (lambda (exp)
    (cond
      
      [(not (list? exp))
       #f]

      [(null? exp)
       #t]

      [(and
        (eqv? (length-of-list exp) 1)
        (or-exp? (car exp)))
       #t]

      [else
       (and
        (or-exp? (car exp))
        (eqv? (cadr exp) "AND")
        (not (null? (cddr exp)))
        (and-exp? (cddr exp)))])))

;; Pruebas
(eqv? (and-exp? '() ) #t)
(eqv? (and-exp? '((1)) ) #t)
(eqv? (and-exp? '((1 "OR" -1)) ) #t)
(eqv? (and-exp? '((1 "OR" -1 "OR" 3) "AND" (2)) ) #t)
(eqv? (and-exp? '((1 "OR" -1) "AND" (2 "OR" -2)) ) #t)
(eqv? (and-exp? '((1 "OR" -1 "OR")) ) #f)
(eqv? (and-exp? '((1 "OR" -1) "AND" (2 2)) ) #f)
(eqv? (and-exp? '((1 "OR" -1) "AND" (2 "OR" -2) "AND") ) #f)


(define and-exp
  (lambda (exp1 exp2)
    (cond

      [(or
        (not (or-exp? exp1))
        (null? exp1))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una expresión OR válida y no vacía" exp1)]

      [(not (and-exp? exp2))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una expresión AND" exp2)]

      [(null? exp2)
       (cons exp1 empty)]

      [else
       (cons exp1
             (cons "AND" exp2))])))

;; Pruebas
(define and-exp1 (and-exp or-exp1 empty))
(define and-exp2 (and-exp or-exp2 and-exp1))
(define and-exp3 (and-exp or-exp3 '((4 "OR" -2))))
(define and-exp4 (and-exp '(1) (and-exp '(-1 "OR" -3) and-exp3)))
;(define and-exp5 (and-exp '(1 "OR") empty))
;(define and-exp6 (and-exp empty empty))
;(define and-exp7 (and-exp or-exp3 '((1 "OR" 2) "AND")))


(define and-exp->clauses
  (lambda (exp)
  (cond
    
    [(not (and-exp? exp))
     (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una expresión AND" exp)]

    
     [(eqv? (length-of-list exp) 1)
      (cons (car exp) empty)]

     [else
      (cons (car exp) (and-exp->clauses (cddr exp)))])))

;; Pruebas
(and-exp->clauses and-exp1)
(and-exp->clauses and-exp2)
(and-exp->clauses and-exp3)
(and-exp->clauses and-exp4)


(define fnc-exp
  (lambda (num-of-var exp)
    (cond

      [(not (number? num-of-var))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser un número" num-of-var)]
      
      [(not (and-exp? exp))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una expresión AND" exp)]
      
      [else
       (list "FNC" num-of-var exp)])))

;; Pruebas
(define fnc1 (fnc-exp 1 and-exp1))
(define fnc2 (fnc-exp 3 '((-1 "OR" 2 "OR" -3) "AND" (3 "OR" 1) "AND" (2))))
(define fnc3 (fnc-exp 4 and-exp4))
;(define fnc4 (fnc-exp "AND" and-exp1))
;(define fnc5 (fnc-exp 2 or-exp3))
;(define fnc6 (fnc-exp 3 '((-1 "OR" 2 "OR" -3) "AND" (3 "OR" 1) "AND")))
;(define fnc7 (fnc-exp 0 (and-exp '() '())))


(define fnc-exp->var
  (lambda (exp)
    (get-element-at-index exp 1)))

;; Pruebas
(fnc-exp->var fnc1)
(fnc-exp->var fnc2)
(fnc-exp->var fnc3)


(define fnc-exp->clauses
  (lambda (exp)
    (and-exp->clauses (get-element-at-index exp 2))))


;; Pruebas
(fnc-exp->clauses fnc1)
(fnc-exp->clauses fnc2)
(fnc-exp->clauses fnc3)


(define eqv-operator?
  (lambda (operator)
    (lambda (compared-string)
      (eqv? operator compared-string))))


(define-datatype d-or d-or?
  (log-operand (operand number?))
  (d-or-exp (operand d-or?) (operator (eqv-operator? "OR")) (exp d-or?)))

;; Pruebas
(define d-or-exp1 (log-operand 1))
(define d-or-exp2 (d-or-exp (log-operand 2) "OR" (log-operand 1)))
(define d-or-exp3 (d-or-exp (log-operand -1) "OR" (d-or-exp (log-operand -2) "OR" (log-operand 3))))
(define d-or-exp4 (d-or-exp (log-operand 4) "OR" (d-or-exp (log-operand -1) "OR" (d-or-exp (log-operand -3) "OR"(log-operand 2)))))


(define-datatype d-and d-and?
  (and-operand (exp d-or?))
  (d-and-exp (exp1 d-and?) (operator (eqv-operator? "AND")) (exp2 d-and?)))

;; Pruebas
(define d-and-exp1 (and-operand d-or-exp1))
(define d-and-exp2 (and-operand d-or-exp2))
(define d-and-exp3 (d-and-exp (and-operand d-or-exp3) "AND" (and-operand d-or-exp4)))
(define d-and-exp4 (d-and-exp d-and-exp1 "AND" (d-and-exp d-and-exp2 "AND" d-and-exp3)))


(define-datatype d-fnc d-fnc?
  (d-fnc-exp (intro (eqv-operator? "FNC")) (num-var number?) (exp d-and?)))

;; Pruebas
(define d-fnc1 (d-fnc-exp "FNC" 1 d-and-exp1))
(define d-fnc2 (d-fnc-exp "FNC" 2 d-and-exp2))
(define d-fnc3 (d-fnc-exp "FNC" 4 d-and-exp3))
(define d-fnc4 (d-fnc-exp "FNC" 4 d-and-exp4))






