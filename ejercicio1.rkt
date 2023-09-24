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
;; length-of-list:  Lista -> int
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


;; get-element-at-index:  Lista -> SchemeValue
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

;; or-exp?:  Lista -> boolean
;; Propósito:
;; Identifica si una lista está construida con la estructura definida para una expresión or.
;; 1) Primero evalúa si la expresión pasada como argumento es una lista.
;; 2) Después evalúa si la lista está vacía (como el constructor de expresiones or esperará dos argumentos, 
;; entonces la lista vacía permitirá construir expresiones or con un solo elemento: (or-exp 1 empty) -> (1)).
;; 3) Después evalúa si la lista tiene un solo elemento, en cuyo caso verifica que sea un número.
;; 4) Finalmente si la lista tiene más de un elemento, evalúa que el primer elemento sea un número, que el segundo
;; sea el string "OR" y que el tercer elemento no sea vacío y que sea una expresión or.
;;
;; <or-exp> := <int> | <int> "OR" <or-exp>

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


;; or-exp:  int X or-exp -> or-exp
;; Propósito:
;; Construye una expresión or a partir de dos argumentos.
;; 1) Primero valida que el primer argumento sea un número.
;; 2) Después valida que el segundo argumento sea una expresión or.
;; 3) A continuación valida si el segundo argumento es una lista vacía, en cuyo caso retorna una lista
;; con el primer argumento como único elemento (retorna una expresión or con un solo elemento).
;; 4) Finalmente si el segundo argumento no es vacío, entonces retorna una lista con la estructura de
;; una expresión or, donde el primer elemento es el número pasado como primer argumento, el segundo elemento
;; es el string "OR" y el tercer elemento es la expresión or pasada como segundo argumento.
;;
;; <or-exp> := <int> | <int> "OR" <or-exp>

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
or-exp1
or-exp2
or-exp3

;; Reconocimiento de entradas no válidas
;(define or-exp4 (or-exp 1 (or-exp 2 (or-exp -3 4))))


;; or-exp->varlist: or-exp -> Lista-de-int
;; Propósito:
;; Retorna una lista con los números que conforman la expresión or pasada como argumento.
;; 1) Primero valida que el argumento sea una expresión or.
;; 2) Después valida si la expresión or tiene un solo elemento, en cuyo caso retorna una lista con ese elemento (caso base).
;; 3) Finalmente si la expresión or tiene más de un elemento, retorna una lista con el primer elemento y el resultado de
;; aplicar la función or-exp->varlist a la expresión or que se encuentra después del string "OR".
;;
;; <or-exp> := <int> | <int> "OR" <or-exp>

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


;; ****************************************** Expresiones and ******************************************

;; and-exp?:  Lista -> boolean
;; Propósito:
;; Identifica si una lista está construida con la estructura definida para una expresión and.
;; 1) Primero evalúa si la expresión pasada como argumento es una lista.
;; 2) Después evalúa si la lista está vacía (como el constructor de expresiones and esperará dos argumentos,
;; entonces la lista vacía permitirá construir expresiones and con un solo elemento: (and-exp (1 "OR" 2) empty) -> ((1 "OR" 2)).
;; 3) Después evalúa si la lista tiene un solo elemento, en cuyo caso verifica que sea una expresión or.
;; 4) Finalmente si la lista tiene más de un elemento, evalúa que el primer elemento sea una expresión or, que el segundo
;; sea el string "AND" y que el tercer elemento no sea vacío y que sea una expresión and.
;;
;; <and-exp> := <or-exp> | <or-exp> "AND" <and-exp>
;; <or-exp> := <int> | <int> "OR" <or-exp>

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



;; and-exp:  or-exp X and-exp -> and-exp
;; Propósito:
;; Construye una expresión and a partir de dos argumentos.
;; 1) Primero valida que el primer argumento sea una expresión or no vacía.
;; 2) Después valida que el segundo argumento sea una expresión and.
;; 3) A continuación valida si el segundo argumento es una lista vacía, en cuyo caso retorna una lista
;; con el primer argumento como único elemento (retorna una expresión and con una sola cláusula or).
;; 4) Finalmente si el segundo argumento no es vacío, entonces retorna una lista con la estructura de
;; una expresión and, donde el primer elemento es la expresión or pasada como primer argumento, el segundo elemento
;; es el string "AND" y el tercer elemento es la expresión and pasada como segundo argumento.
;;
;; <and-exp> := <or-exp> | <or-exp> "AND" <and-exp>
;; <or-exp> := <int> | <int> "OR" <or-exp>

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
and-exp1
and-exp2
and-exp3
and-exp4

;; Reconocimiento de entradas no válidas
;(define and-exp5 (and-exp '(1 "OR") empty))
;(define and-exp6 (and-exp empty empty))
;(define and-exp7 (and-exp or-exp3 '((1 "OR" 2) "AND")))


;; and-exp->clauses: and-exp -> Lista-de-or-exp
;; Propósito:
;; Retorna una lista con las cláusulas or que conforman la expresión and pasada como argumento.
;; 1) Primero valida que el argumento sea una expresión and.
;; 2) Después valida si la expresión and tiene un solo elemento, en cuyo caso retorna una lista con ese elemento (caso base).
;; 3) Finalmente si la expresión and tiene más de un elemento, retorna una lista con el primer elemento y el resultado de
;; aplicar la función and-exp->clauses a la expresión and que se encuentra después del string "AND".
;;
;; <and-exp> := <or-exp> | <or-exp> "AND" <and-exp>
;; <or-exp> := <int> | <int> "OR" <or-exp>

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


;; ****************************************** Expresiones fnc ******************************************

;; fnc-exp: int X and-exp -> fnc-exp
;; Propósito:
;; Construye una expresión fnc a partir de dos argumentos.
;; 1) Primero valida que el primer argumento sea un número.
;; 2) Después valida que el segundo argumento sea una expresión and.
;; 3) Finalmente retorna una lista con la estructura de una expresión fnc, donde el primer elemento es el string
;; "FNC", el segundo elemento es el número pasado como primer argumento y el tercer elemento es la expresión and
;; pasada como segundo argumento.
;;
;; <fnc-exp> := "FNC" <int> (<and-exp>)
;; <and-exp> := <or-exp> | <or-exp> "AND" <and-exp>
;; <or-exp> := <int> | <int> "OR" <or-exp>

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
fnc1
fnc2
fnc3

;; Reconocimiento de entradas no válidas
;(define fnc4 (fnc-exp "AND" and-exp1))
;(define fnc5 (fnc-exp 2 or-exp3))
;(define fnc6 (fnc-exp 3 '((-1 "OR" 2 "OR" -3) "AND" (3 "OR" 1) "AND")))
;(define fnc7 (fnc-exp 0 (and-exp '() '())))


;; fnc-exp->var: fnc-exp -> int
;; Propósito:
;; Retorna el número de variables que conforman la expresión fnc pasada como argumento.
;;
;; <fnc-exp> := "FNC" <int> (<and-exp>)

(define fnc-exp->var
  (lambda (exp)
    (get-element-at-index exp 1)))

;; Pruebas
(fnc-exp->var fnc1)
(fnc-exp->var fnc2)
(fnc-exp->var fnc3)


;; fnc-exp->clauses: fnc-exp -> Lista-de-and-exp
;; Propósito:
;; Retorna una lista con las cláusulas and que conforman la expresión fnc pasada como argumento.
;; Para ello aplica el procedimiento and-exp->clauses a la expresión and que se encuentra en la posición 2
;; de la expresión fnc.
;;
;; <fnc-exp> := "FNC" <int> (<and-exp>)

(define fnc-exp->clauses
  (lambda (exp)
    (and-exp->clauses (get-element-at-index exp 2))))

;; Pruebas
(fnc-exp->clauses fnc1)
(fnc-exp->clauses fnc2)
(fnc-exp->clauses fnc3)


;; ################################## Representación basada en datatypes ##################################

;; eqv-operator?:  String -> boolean
;; Propósito:
;; Devuelve una función que hace las veces de predicado para comparar strings.
;; Verifica que el string pasado como argumento sea igual al string sobre el cual se aplica la función devuelta.

(define eqv-operator?
  (lambda (operator)
    (lambda (compared-string)
      (eqv? operator compared-string))))

;; Pruebas
(eqv? ((eqv-operator? "OR") "OR") #t)
(eqv? ((eqv-operator? "AND") "AND") #t)
(eqv? ((eqv-operator? "OR") "AND") #f)


;; ****************************************** Expresiones or ******************************************

;; datatype d-or
;; Representa una expresión or.
;; <or-exp> := <int> | <or-exp> "OR" <or-exp>
;;
;; log-operand
;; Representa la variante terminal de una expresión or, que es un número (en últimas un operador lógico).
;;
;; d-or-exp
;; Representa la variante recursiva de una expresión or, que es una expresión or seguida del string "OR" y otra expresión or.

(define-datatype d-or d-or?
  (log-operand (operand number?))
  (d-or-exp (operand d-or?) (operator (eqv-operator? "OR")) (exp d-or?)))

;; Pruebas
(define d-or-exp1 (log-operand 1))
(define d-or-exp2 (d-or-exp (log-operand 2) "OR" (log-operand 1)))
(define d-or-exp3 (d-or-exp (log-operand -1) "OR" (d-or-exp (log-operand -2) "OR" (log-operand 3))))
(define d-or-exp4 (d-or-exp (log-operand 4) "OR" (d-or-exp (log-operand -1) "OR" (d-or-exp (log-operand -3) "OR"(log-operand 2)))))
d-or-exp1
d-or-exp2
d-or-exp3
d-or-exp4


;; ****************************************** Expresiones and ******************************************

;; datatype d-and
;; Representa una expresión and.
;; <and-exp> := <or-exp> | <and-exp> "AND" <and-exp>
;;
;; and-operand
;; Representa la variante terminal de una expresión and, que es una expresión or.
;;
;; d-and-exp
;; Representa la variante recursiva de una expresión and, que es una expresión and seguida del string "AND" y otra expresión and.

(define-datatype d-and d-and?
  (and-operand (exp d-or?))
  (d-and-exp (exp1 d-and?) (operator (eqv-operator? "AND")) (exp2 d-and?)))

;; Pruebas
(define d-and-exp1 (and-operand d-or-exp1))
(define d-and-exp2 (and-operand d-or-exp2))
(define d-and-exp3 (d-and-exp (and-operand d-or-exp3) "AND" (and-operand d-or-exp4)))
(define d-and-exp4 (d-and-exp d-and-exp1 "AND" (d-and-exp d-and-exp2 "AND" d-and-exp3)))
d-and-exp1
d-and-exp2
d-and-exp3
d-and-exp4


;; ****************************************** Expresiones fnc ******************************************

;; datatype d-fnc
;; Representa una expresión fnc.
;; <fnc-exp> := "FNC" <int> (<and-exp>)
;;
;; d-fnc-exp
;; Representa la única variante de una expresión fnc, que es el string "FNC" seguido de un número y una expresión and.

(define-datatype d-fnc d-fnc?
  (d-fnc-exp (intro (eqv-operator? "FNC")) (num-var number?) (exp d-and?)))

;; Pruebas
(define d-fnc1 (d-fnc-exp "FNC" 1 d-and-exp1))
(define d-fnc2 (d-fnc-exp "FNC" 2 d-and-exp2))
(define d-fnc3 (d-fnc-exp "FNC" 4 d-and-exp3))
(define d-fnc4 (d-fnc-exp "FNC" 4 d-and-exp4))
d-fnc1
d-fnc2
d-fnc3
d-fnc4
