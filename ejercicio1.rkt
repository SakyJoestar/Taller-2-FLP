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



;; ################################## Representación basada en listas ##################################

;; ****************************************** Expresiones or ******************************************

;; or-exp?:  lista -> boolean
;; Propósito:
;; Identifica si una lista está construida con la estructura definida para una expresión or.
;; 1) Primero evalúa si la expresión pasada como argumento es una lista no vacía.
;; 2) Después evalúa si la lista tiene un solo elemento, en cuyo caso verifica que sea un número.
;; 3) Finalmente si la lista tiene más de un elemento, verifica que el primer elemento sea un número,
;; que el segundo sea el símbolo 'OR, y que el resto de la lista no sea vacía y sea una expresión or.
;;
;; <or-exp> := <int> | <int> 'OR <or-exp>

(define or-exp?
  (lambda (exp)
    (cond
      
      [(or
        (not (list? exp))
        (null? exp))
       #f]

      [(and
        (eqv? (length-of-list exp) 1)
        (number? (car exp)))
       #t]

      [else
       (and
        (number? (car exp))
        (eqv? (cadr exp) 'OR)
        (not (null? (cddr exp)))
        (or-exp? (cddr exp)))])))

;; Pruebas
(eqv? (or-exp? '(1)) #t)
(eqv? (or-exp? '(1 OR -1)) #t)
(eqv? (or-exp? '(1 OR -1 OR -5)) #t)
(eqv? (or-exp? '()) #f)
(eqv? (or-exp? '(1 OR -1 -5)) #f)
(eqv? (or-exp? '(1 OR -1 OR (-5 OR 2))) #f)
(eqv? (or-exp? '(1 OR)) #f)


;; or-exp:  lista -> or-exp
;; Propósito:
;; Construye una expresión or a partir de una lista que cumpla con la gramática.
;; 1) Primero evalúa si la lista pasada como argumento tiene la estructura definida para una expresión or.
;; 2) Después evalúa si la lista tiene un solo elemento, en cuyo caso retorna la lista con este número.
;; 3) Finalmente si la lista tiene más de un elemento, retorna una lista con el primer elemento y
;; el resultado de aplicar recursivamente el constructor or-exp al resto de la lista.
;;
;; <or-exp> := <int> | <int> 'OR <or-exp>

(define or-exp
  (lambda (exp)
    (cond

      [(not (or-exp? exp))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una lista que siga la gramática <or-exp> := <int> | <int> OR <or-exp>" exp)]

      [(eqv? (length-of-list exp) 1)
       (cons (car exp) empty)]

      [else
       (cons (car exp) (or-exp (cddr exp)))])))

;; Pruebas
(define or-exp1 (or-exp '(1)))
(define or-exp2 (or-exp '(1 OR -2)))
(define or-exp3 (or-exp '(1 OR -2 OR 3 OR -4)))
or-exp1
or-exp2
or-exp3

;; Reconocimiento de entradas no válidas
;(define or-exp4 (or-exp '()))
;(define or-exp5 (or-exp '(1 OR not-a-number)))
;(define or-exp6 (or-exp '(1 OR)))


;; or-exp->varlist:  lista -> or-exp
;; Propósito:
;; Retorna una lista con los números que conforman la expresión or pasada como argumento.
;; Como la sintáxis abstracta basada en listas que escogimos define una expresión or como una lista
;; de números, esta función llama al constructor or-exp, que se encarga de realizar las validaciones
;; y construcciones necesarias.
;;
;; <or-exp> := <int> | <int> 'OR <or-exp>

(define or-exp->varlist
  (lambda (exp)
    (or-exp exp)))

;; Pruebas
(or-exp->varlist '(1))
(or-exp->varlist '(1 OR -2))
(or-exp->varlist '(1 OR -2 OR 3 OR -4))



;; ****************************************** Expresiones and ******************************************

;; and-exp?:  lista -> boolean
;; Propósito:
;; Identifica si una lista está construida con la estructura definida para una expresión and.
;; 1) Primero evalúa si la expresión pasada como argumento es una lista no vacía.
;; 2) Después evalúa si la lista tiene un solo elemento, en cuyo caso verifica que sea una expresión or.
;; 3) Finalmente si la lista tiene más de un elemento, verifica que el primer elemento sea una expresión or,
;; que el segundo sea el símbolo 'AND, y que el resto de la lista no sea vacía y sea una expresión and.
(define and-exp?
  (lambda (exp)
    (cond

      [(or
        (not (list? exp))
        (null? exp))
       #f]
      
      [(and
        (eqv? (length-of-list exp) 1)
        (or-exp? (car exp)))
       #t]

      [else
       (and
        (or-exp? (car exp))
        (eqv? (cadr exp) 'AND)
        (not (null? (cddr exp)))
        (and-exp? (cddr exp)))])))

;; Pruebas
(eqv? (and-exp? '((1)) ) #t)
(eqv? (and-exp? '((1 OR -1)) ) #t)
(eqv? (and-exp? '((1 OR -1 OR 3) AND (2)) ) #t)
(eqv? (and-exp? '((1 OR -1) AND (2 OR -2)) ) #t)
(eqv? (and-exp? '() ) #f)
(eqv? (and-exp? '(()) ) #f)
(eqv? (and-exp? '((1 OR -1 OR)) ) #f)
(eqv? (and-exp? '((1 OR -1) AND (2 2)) ) #f)
(eqv? (and-exp? '((1 OR -1) AND (2 OR -2) AND) ) #f)


;; and-exp:  lista -> and-exp
;; Propósito:
;; Construye una expresión and a partir de una lista que cumpla con la gramática.
;; 1) Primero evalúa si la lista pasada como argumento tiene la estructura definida para una expresión and.
;; 2) Después evalúa si la lista tiene un solo elemento, en cuyo caso retorna la lista con este elemento.
;; 3) Finalmente si la lista tiene más de un elemento, retorna una lista con el primer elemento y
;; el resultado de aplicar recursivamente el constructor and-exp al resto de la lista.
;;
;; <and-exp> := (<or-exp>) | (<or-exp>) AND <and-exp>
;; <or-exp> := <int> | <int> 'OR <or-exp>

(define and-exp
  (lambda (exp)
    (cond

      [(not (and-exp? exp))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una lista que siga la gramática <and-exp> := (<or-exp>) | (<or-exp>) AND <and-exp>" exp)]

      [(eqv? (length-of-list exp) 1)
       (cons (or-exp (car exp)) empty)]

      [else
       (cons (or-exp (car exp)) (and-exp (cddr exp)))])))

;; Pruebas
(define and-exp1 (and-exp '((1))))
(define and-exp2 (and-exp '((1 OR -2))))
(define and-exp3 (and-exp '((1 OR -2) AND (3 OR -4))))
(define and-exp4 (and-exp '((1 OR -2) AND (3 OR -4) AND (-5))))
and-exp1
and-exp2
and-exp3
and-exp4

; Reconocimiento de entradas no válidas
;(define and-exp5 (and-exp '(1)))
;(define and-exp6 (and-exp '(())))
;(define and-exp7 (and-exp '((1 OR -2) AND (3 OR))))
;(define and-exp8 (and-exp '((1 OR -2) AND (3 OR -4) AND)))


;; and-exp->clauses:  lista -> and-exp
;; Propósito:
;; Retorna una lista con las expresiones or que conforman la expresión and pasada como argumento.
;; Como la sintáxis abstracta basada en listas que escogimos define una expresión and como una lista
;; de listas de números, esta función llama al constructor and-exp, que se encarga de realizar las validaciones
;; y construcciones necesarias.

(define and-exp->clauses
  (lambda (exp)
    (and-exp exp)))

;; Pruebas
(and-exp->clauses '((1)))
(and-exp->clauses '((1 OR -2)))
(and-exp->clauses '((1 OR -2) AND (3 OR -4)))
(and-exp->clauses '((1 OR -2) AND (3 OR -4) AND (-5)))



;; ****************************************** Expresiones fnc ******************************************

;; fnc-exp?:  lista -> boolean
;; Propósito:
;; Identifica si una lista está construida con la estructura definida para una expresión fnc.
;; 1) Primero evalúa si la expresión pasada como argumento es una lista no vacía.
;; 2) Después evalúa si la lista tiene tres elementos, en cuyo caso verifica que el primero sea el símbolo 'FNC,
;; que el segundo sea un número, y que el tercero sea una expresión and.
;;
;; <fnc-exp> := FNC <int> (<and-exp>)
;; <and-exp> := (<or-exp>) | (<or-exp>) AND <and-exp>
;; <or-exp> := <int> | <int> 'OR <or-exp>

(define fnc-exp?
  (lambda (exp) 
    (if (not (list? exp))
        #f
        (and
         (eqv? (length-of-list exp) 3)
         (eqv? (car exp) 'FNC)
         (number? (cadr exp))
         (and-exp? (caddr exp))))))

;; Pruebas
(eqv? (fnc-exp? '(FNC 1 ((1 OR 2)))) #t)
(eqv? (fnc-exp? '(FNC 1 ((1 OR 2 OR -3) AND (2)))) #t)
(eqv? (fnc-exp? '(FNC 5 and-exp4)) #t)
(eqv? (fnc-exp? '()) #f)
(eqv? (fnc-exp? '(FNC 'not-a-number ((1 OR 2 OR -3) AND (2)))) #f)
(eqv? (fnc-exp? '(FNC 1 ((1 OR 2 OR -3) AND (2 OR 1) AND (2)) '4th-element)) #f)


;; fnc-exp:  lista -> fnc-exp
;; Propósito:
;; Construye una expresión fnc a partir de una lista que cumpla con la gramática.
;; 1) Primero evalúa si la lista pasada como argumento tiene la estructura definida para una expresión fnc.
;; 2) Si la verificación es correcta, entonces retorna una lista con el segundo y tercer elemento de la lista pasada como argumento.
;;
;; <fnc-exp> := FNC <int> (<and-exp>)
;; <and-exp> := (<or-exp>) | (<or-exp>) AND <and-exp>
;; <or-exp> := <int> | <int> 'OR <or-exp>

(define fnc-exp
  (lambda (exp)
    (if (not (fnc-exp? exp))
        (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una lista que siga la gramática <fnc-exp> := FNC <int> (<and-exp>)" exp)
        (cons (cadr exp)(cddr exp)))))

;; Pruebas
(define fnc-exp1 (fnc-exp '(FNC 1 ((1)))))
(define fnc-exp2 (fnc-exp '(FNC 2 ((1 OR -2)))))
(define fnc-exp3 (fnc-exp '(FNC 4 ((1 OR -2) AND (3 OR -4)))))
(define fnc-exp4 (fnc-exp '(FNC 5 ((1 OR -2) AND (3 OR -4) AND (-5)))))
fnc-exp1
fnc-exp2
fnc-exp3
fnc-exp4

;; Reconocimiento de entradas no válidas
;(define fnc-exp5 (fnc-exp '()))
;(define fnc-exp6 (fnc-exp '(FNC not-a-number ((1 OR -2) AND (3 OR -4)))))
;(define fnc-exp6 (fnc-exp '(FNC not-a-number ((1 OR -2) AND (3 OR -4)) '4th-element)))


;; fnc-exp->var:  lista -> int
;; Propósito:
;; Retorna el número de variables que conforman la expresión fnc pasada como argumento.
;; 1) Primero evalúa si la lista pasada como argumento tiene la estructura definida para una expresión fnc.
;; 2) Si se cumple lo anterior, retorna el segundo elemento de la lista pasada como argumento, que es el número de variables
;; que dentro de la expresión fnc.
;;
;; <fnc-exp> := FNC <int> (<and-exp>)

(define fnc-exp->var
  (lambda (exp)
    (if (not (fnc-exp? exp))
        (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una lista que siga la gramática <fnc-exp> := FNC <int> (<and-exp>)" exp)
        (cadr exp))))

;; Pruebas
(fnc-exp->var '(FNC 1 ((1))))
(fnc-exp->var '(FNC 2 ((1 OR -2))))
(fnc-exp->var '(FNC 4 ((1 OR -2) AND (3 OR -4))))
(fnc-exp->var '(FNC 5 ((1 OR -2) AND (3 OR -4) AND (-5))))


;; fnc-exp->clauses:  lista -> and-exp
;; Propósito:
;; Retorna una lista con las expresiones or que conforman la expresión and de la expresión fnc pasada como argumento.
;; Como la sintáxis abstracta basada en listas que escogimos define una expresión and como una lista
;; de listas de números, esta función llama al constructor and-exp sobre el tercer elemento de la expresión fnc,
;; y este constructor se encarga de realizar las validaciones y construcciones necesarias.

(define fnc-exp->clauses
  (lambda (exp)
    (if (not (fnc-exp? exp))
        (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una lista que siga la gramática <fnc-exp> := FNC <int> (<and-exp>)" exp)
        (and-exp->clauses (caddr exp)))))

;; Pruebas
(fnc-exp->clauses '(FNC 1 ((1))))
(fnc-exp->clauses '(FNC 2 ((1 OR -2))))
(fnc-exp->clauses '(FNC 4 ((1 OR -2) AND (3 OR -4))))
(fnc-exp->clauses '(FNC 5 ((1 OR -2) AND (3 OR -4) AND (-5))))



;; ################################## Representación basada en datatypes ##################################

;; ****************************************** Expresiones or ******************************************

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


;; datatype-or-exp
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

;; list-of-or-exps?:  lista -> boolean
;; Propósito:
;; Predicado que verifica si una lista es no vacía y está compuesta unicamente por expresiones or basadas en datatypes.
;;
;; <lista-de-expresiones-or> := ( {<datatype-or-exp>}+ )

(define list-of-or-exps?
  (lambda (exp)
    (and
     ((list-of datatype-or-exp?) exp)
     (not (null? exp)))))

;; Pruebas
(eqv? (list-of-or-exps? (list d-or-exp1)) #t)
(eqv? (list-of-or-exps? (list d-or-exp1 d-or-exp2)) #t)
(eqv? (list-of-or-exps? (list d-or-exp1 d-or-exp2 d-or-exp3)) #t)
(eqv? (list-of-or-exps? '()) #f)
(eqv? (list-of-or-exps? (list d-or-exp1 'not-a-d-or-exp)) #f)


;; datatype-and-exp
;; Representación de una expresión and como un datatype.
;; Construye el árbol de sintáxis abstracta basado en datatypes para una expresión and
;; a partir de una lista de expresiones or basadas en datatypes.

(define-datatype datatype-and-exp datatype-and-exp?
  (d-and-exp (exp list-of-or-exps?)))

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
;(define d-and-exp6 (d-and-exp (list d-or-exp1 not-a-d-or-exp)))


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
;(define d-fnc-exp5 (d-fnc-exp 1 'not-a-d-and-exp))
;(define d-fnc-exp6 (d-fnc-exp 1 d-or-exp1))

