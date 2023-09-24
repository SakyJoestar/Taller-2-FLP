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


(define fnc-exp
  (lambda (num-of-var exp)
    (cond

      [(not (number? num-of-var))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser un número" num-of-var)]
      
      [(not (and-exp? exp))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una expresión AND" exp)]
      
      [else
       (list "FNC" num-of-var exp)])))



(define eqv-operator?
  (lambda (operator)
    (lambda (compared-string)
      (eqv? operator compared-string))))


(define-datatype d-or d-or?
  (log-operand (operand number?))
  (d-or-exp (operand d-or?) (operator (eqv-operator? "OR")) (exp d-or?)))


(define-datatype d-and d-and?
  (and-operand (exp d-or?))
  (d-and-exp (exp1 d-and?) (operator (eqv-operator? "AND")) (exp2 d-and?)))


(define-datatype d-fnc d-fnc?
  (d-fnc-exp (intro (eqv-operator? "FNC")) (num-var number?) (exp d-and?)))


(define PARSEOR
  (lambda (exp)
    (cond

      [(not (or-exp? exp))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una expresión OR" exp)]

      [(eqv? (length-of-list exp) 1)
       (log-operand (car exp))]

      [else
       (d-or-exp (log-operand (car exp)) (cadr exp) (PARSEOR (cddr exp)))])))

;; Pruebas
(define parseor1 (PARSEOR '(1)))
(define parseor2 (PARSEOR '(1 "OR" 2)))
(define parseor3 (PARSEOR '(1 "OR" 2 "OR" -3 "OR" 4 "OR" -5)))
;(define parseor4 (PARSEOR '(1 "OR")))


(define PARSEAND
  (lambda (exp)
    (cond

      [(not (and-exp? exp))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una expresión AND" exp)]

      [(eqv? (length-of-list exp) 1)
       (and-operand (PARSEOR(car exp)))]

      [else
       (d-and-exp (and-operand (PARSEOR (car exp))) (cadr exp) (PARSEAND (cddr exp)))])))

;; Pruebas
(define parseand1 (PARSEAND '((1))))
(define parseand2 (PARSEAND '((1 "OR" 2))))
(define parseand3 (PARSEAND '((1) "AND" (2))))
(define parseand4 (PARSEAND '((1 "OR" 2) "AND" (3 "OR" -4 "OR" 5))))


(define PARSEBNF
  (lambda (exp)
    (d-fnc-exp (car exp) (cadr exp) (PARSEAND (caddr exp)))))

;; Pruebas

(define parsebnf1 (PARSEBNF '("FNC" 1  ((1)))))
(define parsebnf2 (PARSEBNF '("FNC" 2  ((1 "OR" 2)))))
(define parsebnf3 (PARSEBNF '("FNC" 2  ((1) "AND" (2)))))
(define parsebnf4 (PARSEBNF '("FNC" 1  ((1 "OR" 2) "AND" (3 "OR" -4 "OR" 5)))))



       




