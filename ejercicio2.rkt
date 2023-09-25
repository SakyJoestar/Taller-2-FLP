#|
Fundamentos de Interpretación y Compilación de Lenguajes de Programación
750017C - G01
=======
Taller 2: Abstracción de datos y sintáxis abstracta
Ejercicio 2: Funciones Parse y Unparse.

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


;; fnc-exp->var: fnc-exp -> int
;; Propósito:
;; Retorna el número de variables que conforman la expresión fnc pasada como argumento.
;;
;; <fnc-exp> := "FNC" <int> (<and-exp>)

(define fnc-exp->var
  (lambda (exp)
    (get-element-at-index exp 1)))


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


;; ################################## Representación basada en datatypes ##################################

;; eqv-operator?:  String -> boolean
;; Propósito:
;; Devuelve una función que hace las veces de predicado para comparar strings.
;; Verifica que el string pasado como argumento sea igual al string sobre el cual se aplica la función devuelta.

(define eqv-operator?
  (lambda (operator)
    (lambda (compared-string)
      (eqv? operator compared-string))))


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


;; ****************************************** Expresiones fnc ******************************************

;; datatype d-fnc
;; Representa una expresión fnc.
;; <fnc-exp> := "FNC" <int> (<and-exp>)
;;
;; d-fnc-exp
;; Representa la única variante de una expresión fnc, que es el string "FNC" seguido de un número y una expresión and.

(define-datatype d-fnc d-fnc?
  (d-fnc-exp (intro (eqv-operator? "FNC")) (num-var number?) (exp d-and?)))


;; ################################## Funciones Parse y Unparse ###############################

;; ****************************************** Parsers ******************************************

;; PARSEOR:  or-exp -> d-or
;; Propósito:
;; Convierte una expresión or basada en listas a una expresión or basada en datatypes.
;; 1) Primero valida que el argumento sea una expresión or.
;; 2) Después valida si la expresión or tiene un solo elemento, en cuyo caso retorna un log-operand con ese elemento
;; (variante terminal de la representación basada en datatypes).
;; 3) Finalmente si la expresión or tiene más de un elemento, retorna un d-or-exp con el log-operand del primer elemento,
;; el string "OR" y el resultado de aplicar la función PARSEOR a la expresión or que se encuentra después del string "OR"
;;(variante recursiva de la representación basada en datatypes).
;;
;; <or-exp> := <int> | <int> "OR" <or-exp>

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
(define parseor2 (PARSEOR '(1 "OR" 3 "OR" 2)))
(define parseor3 (PARSEOR '(1 "OR" 2 "OR" -3 "OR" 4 "OR" -5)))
parseor1
parseor2
parseor3


;; PARSEAND:  and-exp -> d-and
;; Propósito:
;; Convierte una expresión and basada en listas a una expresión and basada en datatypes.
;; 1) Primero valida que el argumento sea una expresión and.
;; 2) Después valida si la expresión and tiene un solo elemento, en cuyo caso retorna un and-operand con ese elemento
;; (variante terminal de la representación basada en datatypes).
;; 3) Finalmente si la expresión and tiene más de un elemento, retorna un d-and-exp con el and-operand del primer elemento,
;; el string "AND" y el resultado de aplicar la función PARSEAND a la expresión and que se encuentra después del string "AND"
;;(variante recursiva de la representación basada en datatypes).
;;
;; <and-exp> := <or-exp> | <or-exp> "AND" <and-exp>
;; <or-exp> := <int> | <int> "OR" <or-exp>

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
parseand1
parseand2
parseand3
parseand4


;; PARSEBNF:  fnc-exp -> d-fnc
;; Propósito:
;; Convierte una expresión fnc basada en listas a una expresión fnc basada en datatypes.
;; Convierte la expresión and de la expresión fnc a una expresión and basada en datatypes 
;; usando la función PARSEAND.
;;
;; <fnc-exp> := "FNC" <int> (<and-exp>)
;; <and-exp> := <or-exp> | <or-exp> "AND" <and-exp>
;; <or-exp> := <int> | <int> "OR" <or-exp>

(define PARSEBNF
  (lambda (exp)
    (d-fnc-exp (car exp) (cadr exp) (PARSEAND (caddr exp)))))

;; Pruebas
(define parsebnf1 (PARSEBNF '("FNC" 1  ((1)))))
(define parsebnf2 (PARSEBNF '("FNC" 2  ((1 "OR" 2)))))
(define parsebnf3 (PARSEBNF '("FNC" 2  ((1) "AND" (2)))))
(define parsebnf4 (PARSEBNF '("FNC" 5  ((1 "OR" 2) "AND" (3 "OR" -4 "OR" 5)))))
(define parsebnf5 (PARSEBNF '("FNC" 5  ((1 "OR" 2) "AND" (3 "OR" -4 "OR" 5) "AND" (3 "OR" -4)))))
(define parsebnf6 (PARSEBNF '("FNC" 5  ((1 "OR" 2) "AND" (3 "OR" -4 "OR" 5) "AND" (3 "OR" -4) "AND" (3 "OR" -4)))))
parsebnf1
parsebnf2
parsebnf3
parsebnf4
parsebnf5
parsebnf6

;; ****************************************** Unparsers ******************************************

;; UNPARSEOR:  d-or -> exp-or
;; Propósito:
;; Convierte el arbol de sintaxis abstracta de una expresión or a una expresión or basada en listas.
;; 1) Si la exp es un log-operand entonces se crea un lista con el operando que es un numero entero.
;; 2) Si la exp es un d-or-exp entonces se crea una lista donde el primer elemento es el primer elemento
;; de la lista resultante de la llamada recursiva de UNPARSEOR que por la gramatica, sabemos que sera un log-operand
;; y por tanto un numero entero. El segundo elemento consistira en la lista conformada por el operador y
;; la llamda recursiva de UNPARSEOR que por la gramatica sabemos que sera otra expresion d-or,
;; por ello no se llama junto a car.
;;
;; <or-exp> := <int> | <int> "OR" <or-exp>

(define UNPARSEOR
  (lambda (exp)
    (cases d-or exp
      (log-operand (operand)
                 (list operand))
      (d-or-exp (operand operator exp)
                 (cons (car (UNPARSEOR operand))
                       (cons operator (UNPARSEOR exp)))))
    ))

;; Pruebas
(define unparseor1 (UNPARSEOR parseor1))
(define unparseor2 (UNPARSEOR parseor2))
(define unparseor3 (UNPARSEOR parseor3))


;; UNPARSEAND:  d-and -> exp-and
;; Propósito:
;; Convierte el arbol de sintaxis abstracta de una expresión and a una expresión and basada en listas.
;; 1) Si la exp es un and-operand entonces se crea un lista con el resultado de invocar UNPARSEOR con
;; argumento exp.
;; 2) Si la exp es un d-and-exp entonces se crea una lista donde el primer elemento es el primer elemento
;; de la lista resultante de la llamada recursiva de UNPARSEAND que por la gramatica, sabemos que sera un d-or.
;; El segundo elemento consistira en la lista conformada por el operador y la llamda recursiva de UNPARSEAND que
;; por la gramatica sabemos que sera otra expresion d-and, por ello no se llama junto a car.
;;
;; <and-exp> := <or-exp> | <or-exp> "AND" <and-exp>
;; <or-exp> := <int> | <int> "OR" <or-exp>

(define UNPARSEAND
  (lambda (exp)
    (cases d-and exp
      (and-operand (exp)
                 (list (UNPARSEOR exp)))
      (d-and-exp (exp1 operator exp2)
                 (cons (car (UNPARSEAND exp1))
                       (cons operator (UNPARSEAND exp2)))))
    ))

;; Pruebas
(define unparseand1 (UNPARSEAND parseand1))
(define unparseand2 (UNPARSEAND parseand2))
(define unparseand3 (UNPARSEAND parseand3))
(define unparseand4 (UNPARSEAND parseand4))
