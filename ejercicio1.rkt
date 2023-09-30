#|
Fundamentos de Interpretación y Compilación de Lenguajes de Programación
750017C - G01
=======
Taller 2: Abstracción de datos y sintáxis abstracta
Ejercicio 1: Gramatica BNF

Autores:
Samuel Galindo 2177491
Nicolás Herrera 2182551
Christian Vargas 2179172
|#


#lang eopl

(provide (all-defined-out))

;; ################################## GRAMÁTICA ##################################
;; La gramática para expresiones lógica en forma normal conjuntiva (FNC) que escogimos es la siguiente:
;; 
;; Sintáxis concreta:
;; <fnc> := 'FNC <int> (<and>)
;; <and> := (<or>) | (<or>) 'AND <and>
;; <or> := <int> | <int> 'OR <or>
;;
;; Sintáxis abstracta basada en listas:
;; <fnc-exp> := <int> (<and-exp>)
;; <and-exp> := (<or-exp>) | (<or-exp>) <and-exp> 
;; <or-exp> := <int> | <int> <or-exp>


;; ****************************************** Funciones propias ******************************************
;; length-of-list:  lista -> int
;; Propósito:
;; Retorna el número de elementos en una lista.
;;
;; <lista> := ( {<SchemeValue>}* )

(define length-of-list
  (lambda (list-of-values)
    (if (eqv? list-of-values empty)
        0
        (+ 1 (length-of-list (cdr list-of-values))))))

;; Pruebas
(eqv? (length-of-list '()) 0)
(eqv? (length-of-list '(a)) 1)
(eqv? (length-of-list '(a b c)) 3)
(eqv? (length-of-list '(a b () c)) 4)


;; get-element-at-index:  lista -> SchemeValue
;; Propósito:
;; Retorna el elemento en la i-ésima posición de la lista.
;;
;; <lista> := ( {<SchemeValue>}* )

(define get-element-at-index
  (lambda (list-of-elements i)
    (cond

      [(eqv? list-of-elements empty)
       (eopl:error 'OutOfBoundError "Index mayor al tamaño de la lista.")]

      [(eqv? i 0)
       (car list-of-elements)]
      
      [else
        (get-element-at-index (cdr list-of-elements) (- i 1))])))

;; Pruebas
(eqv? (get-element-at-index '(a) 0) 'a)
(eqv? (get-element-at-index '(a b c) 2) 'c)
(eqv? (get-element-at-index '(a b () c) 2) '())
;; Reconocimiento de entradas no válidas
;(get-element-at-index '() 0)
;(get-element-at-index '(a) 1)
;(get-element-at-index '(a b c) 3)


;; list-of-numbers?:  lista -> boolean
;; Propósito:
;; Predicado que verifica si una lista es no vacía y está compuesta unicamente por números.
;;
;; <lista-de-numeros> := ( {<int>}+ )

(define list-of-numbers?
  (lambda (exp)
    (and
     ((list-of number?) exp)
     (not (null? exp)))))

;; Pruebas
(eqv? (list-of-numbers? '(1)) #t)
(eqv? (list-of-numbers? '(1 -2)) #t)
(eqv? (list-of-numbers? '(1 -2 -3 4)) #t)
(eqv? (list-of-numbers? '()) #f)
(eqv? (list-of-numbers? '(1 not-a-number)) #f)


;; list-of-list-of-numbers?:  lista -> boolean
;; Propósito:
;; Predicado que verifica si una lista es no vacía y está compuesta unicamente por listas de números.
;;
;; <lista-de-listas-de-numeros> := ( {<lista-de-numeros>}+ )
;; <lista-de-numeros> := ( {<int>}+ )

(define list-list-of-numbers
  (lambda (exp)
    (and
     ((list-of list-of-numbers?) exp)
     (not (null? exp)))))

;; Pruebas
(eqv? (list-list-of-numbers '((1))) #t)
(eqv? (list-list-of-numbers '((1 -2))) #t)
(eqv? (list-list-of-numbers '((1 -2) (3 -4))) #t)
(eqv? (list-list-of-numbers '()) #f)
(eqv? (list-list-of-numbers '((1 not-a-number))) #f)
(eqv? (list-list-of-numbers '((1 -2) (3 -4) not-a-list-of-numbers)) #f)



;; ################################## Representación basada en listas ##################################

;; ****************************************** Expresiones or ******************************************

;; or-exp:  lista-de-numeros -> or-exp
;; Propósito:
;; Construye el árbol de sintáxis abstracta basado en listas para una expresión or a partir de una lista de números.
;; Como la sintáxis abstracta basada en listas que escogimos para las expresiones or es justamente una lista de números,
;; entonces la función or-exp retorna la lista de números que recibe como entrada.

(define or-exp
  (lambda (exp)
    exp))

;; Pruebas
(define or-exp1 (or-exp '(1)))
(define or-exp2 (or-exp '(1 -2)))
(define or-exp3 (or-exp '(1 -2 3 -4)))
or-exp1
or-exp2
or-exp3


;; or-exp->varlist:  or-exp -> lista-de-numeros
;; Propósito:
;; Retorna una lista con los números que conforman la expresión or pasada como argumento.
;; Como la sintáxis abstracta basada en listas que escogimos define una expresión or como una lista
;; de números, entonces esta función retorna la lista de números que recibe como entrada.

(define or-exp->varlist
  (lambda (exp)
    exp))

;; Pruebas
(or-exp->varlist or-exp1)
(or-exp->varlist or-exp2)
(or-exp->varlist or-exp3)



;; ****************************************** Expresiones and ******************************************

;; and-exp:  lista-lista-de-numeros -> and-exp
;; Propósito:
;; Construye el árbol de sintáxis abstracta basado en listas para una expresión and a partir de una lista de listas de números.
;; Como la sintáxis abstracta basada en listas que escogimos para las expresiones and es justamente una lista de listas de números,
;; entonces la función and-exp retorna la lista de listas que recibe como entrada.

(define and-exp
  (lambda (exp)
    exp))

;; Pruebas
(define and-exp1 (and-exp '((1))))
(define and-exp2 (and-exp '((1 -2))))
(define and-exp3 (and-exp '((1 -2) (3 -4))))
(define and-exp4 (and-exp '((1 -2) (3 -4) (-5))))
and-exp1
and-exp2
and-exp3
and-exp4


;; and-exp->clauses:  and-exp -> lista-lista-de-numeros
;; Propósito:
;; Retorna una lista con las listas de números que conforman la expresión and pasada como argumento.
;; Como la sintáxis abstracta basada en listas que escogimos define una expresión and como una lista
;; de listas de números, entonces esta función retorna la lista que recibe como entrada.

(define and-exp->clauses
  (lambda (exp)
    exp))

;; Pruebas
(and-exp->clauses and-exp1)
(and-exp->clauses and-exp2)
(and-exp->clauses and-exp3)
(and-exp->clauses and-exp4)



;; ****************************************** Expresiones fnc ******************************************

;; fnc-exp:  int X lista-lista-de-numeros -> fnc-exp
;; Propósito:
;; Construye el árbol de sintáxis abstracta basado en listas para una expresión fnc a partir de un número y
;; una lista de listas de números que representan una expresión and.
;; Como la sintáxis abstracta basada en listas que escogimos para las expresiones fnc es una lista de dos elementos,
;; donde el primer elemento es un número y el segundo elemento es una lista de listas de números, entonces la función
;; fnc-exp retorna una lista con los dos argumentos que recibe como entrada.

(define fnc-exp
  (lambda (num-of-var exp)
    (list num-of-var exp)))

;; Pruebas
(define fnc-exp1 (fnc-exp 1 '((1))))
(define fnc-exp2 (fnc-exp 2 '((1 -2))))
(define fnc-exp3 (fnc-exp 4 '((1 -2) (3 -4))))
(define fnc-exp4 (fnc-exp 5 '((1 -2) (3 -4)(-5))))
fnc-exp1
fnc-exp2
fnc-exp3
fnc-exp4


;; fnc-exp->var:  fnc-exp -> int
;; Propósito:
;; Retorna el número de variables que conforman la expresión fnc pasada como argumento.
;; Como la sintáxis abstracta basada en listas que escogimos define una expresión fnc como una lista
;; de dos elementos, donde el primer elemento es el número de variables y el segundo elemento es una lista de listas de números
;; que representan una expresión and, entonces esta función retorna el primer elemento de la expresión fnc que recibe como entrada.

(define fnc-exp->var
  (lambda (exp)
    (car exp)))

;; Pruebas
(fnc-exp->var fnc-exp1)
(fnc-exp->var fnc-exp2)
(fnc-exp->var fnc-exp3)
(fnc-exp->var fnc-exp4)


;; fnc-exp->clauses:  fnc-exp -> lista-lista-de-numeros
;; Propósito:
;; Retorna una lista con las listas de números que conforman la expresión and que hace parte de la expresión fnc
;; pasada como argumento. Como la sintáxis abstracta basada en listas que escogimos define una expresión fnc como una lista
;; de dos elementos, donde el primer elemento es el número de variables y el segundo elemento es la expresión and, entonces
;; esta función retorna el segundo elemento de la expresión fnc que recibe como entrada.

(define fnc-exp->clauses
  (lambda (exp)
    (cadr exp)))

;; Pruebas
(fnc-exp->clauses fnc-exp1)
(fnc-exp->clauses fnc-exp2)
(fnc-exp->clauses fnc-exp3)
(fnc-exp->clauses fnc-exp4)



;; ################################## Representación basada en datatypes ##################################

;; ****************************************** Expresiones or ******************************************

;; datatype-or
;; Representación de una expresión or como un datatype.
;; Construye el árbol de sintáxis abstracta basado en datatypes para una expresión or a partir de una lista de números.

(define-datatype datatype-or-exp datatype-or-exp?
  (d-or-exp (exp list-of-numbers?)))

;; Pruebas
(define d-or-exp1 (d-or-exp '(1)))
(define d-or-exp2 (d-or-exp '(1 -2)))
(define d-or-exp3 (d-or-exp '(1 -2 -3 4)))
d-or-exp1
d-or-exp2
d-or-exp3
;; Reconocimiento de entradas no válidas
;(define d-or-exp4 (d-or-exp 'not-a-list))
;(define d-or-exp5 (d-or-exp '()))
;(define d-or-exp6 (d-or-exp '(1 not-a-number)))
;(define d-or-exp7 (d-or-exp '(1 2 ())))



;; ****************************************** Expresiones and ******************************************

;; list-of-d-or-exp?:  lista -> boolean
;; Propósito:
;; Predicado que verifica si una lista es no vacía y está compuesta unicamente por expresiones or basadas en datatypes.
;;
;; <lista-de-expresiones-d-or> := ( {<datatype-or-exp>}+ )

(define list-of-d-or-exp?
  (lambda (exp)
    (and
     ((list-of datatype-or-exp?) exp)
     (not (null? exp)))))

;; Pruebas
(eqv? (list-of-d-or-exp? (list d-or-exp1)) #t)
(eqv? (list-of-d-or-exp? (list d-or-exp1 d-or-exp2)) #t)
(eqv? (list-of-d-or-exp? (list d-or-exp1 d-or-exp2 d-or-exp3)) #t)
(eqv? (list-of-d-or-exp? '()) #f)
(eqv? (list-of-d-or-exp? (list d-or-exp1 'not-a-d-or-exp)) #f)


;; datatype-and-exp
;; Representación de una expresión and como un datatype.
;; Construye el árbol de sintáxis abstracta basado en datatypes para una expresión and
;; a partir de una lista de expresiones or basadas en datatypes.

(define-datatype datatype-and-exp datatype-and-exp?
  (d-and-exp (exp list-of-d-or-exp?)))

;; Pruebas
(define d-and-exp1 (d-and-exp (list d-or-exp1)))
(define d-and-exp2 (d-and-exp (list d-or-exp1 d-or-exp2)))
(define d-and-exp3 (d-and-exp (list d-or-exp1 d-or-exp2 d-or-exp3)))
d-and-exp1
d-and-exp2
d-and-exp3
;; Reconocimiento de entradas no válidas
;(define d-and-exp4 (d-and-exp 'not-a-list))
;(define d-and-exp5 (d-and-exp '()))
;(define d-and-exp6 (d-and-exp (list d-or-exp1 'not-a-d-or-exp)))



;; ****************************************** Expresiones fnc ******************************************

;; datatype-fnc-exp
;; Representación de una expresión fnc como un datatype.
;; Construye el árbol de sintáxis abstracta basado en datatypes para una expresión fnc
;; a partir de un número y una expresión and basada en datatypes.

(define-datatype datatype-fnc-exp datatype-fnc-exp?
  (d-fnc-exp (num-of-var number?) (exp datatype-and-exp?)))

;; Pruebas
(define d-fnc-exp1 (d-fnc-exp 1 d-and-exp1))
(define d-fnc-exp2 (d-fnc-exp 2 d-and-exp2))
(define d-fnc-exp3 (d-fnc-exp 4 d-and-exp3))
d-fnc-exp1
d-fnc-exp2
d-fnc-exp3
;; Reconocimiento de entradas no válidas
;(define d-fnc-exp4 (d-fnc-exp 'not-a-number d-and-exp1))
;(define d-fnc-exp5 (d-fnc-exp 1 'not-a-d-and))
;(define d-fnc-exp6 (d-fnc-exp 1 d-or-exp1))
