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


;; own-map: lista -> lista
;; Propósito:
;; Implementación propia de la función map para listas.
;; Retorna una lista con los resultados de aplicar la función pasada como argumento a cada elemento de la lista pasada como argumento.
;;
;; <lista> := ( {<SchemeValue>}* )

(define own-map
  (lambda (func list-of-elements)
    (if (null? list-of-elements)
        empty
        (cons (func (car list-of-elements)) (own-map func (cdr list-of-elements))))))

;; Pruebas
(own-map (lambda (x) (* 2 x)) '(1 2 3 4))
(own-map (lambda (x) (+ 100 x)) '(1 2 3 4))



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


;; and-exp->clauses:  lista -> and-exp
;; Propósito:
;; Retorna una lista con las expresiones or que conforman la expresión and pasada como argumento.
;; Como la sintáxis abstracta basada en listas que escogimos define una expresión and como una lista
;; de listas de números, esta función llama al constructor and-exp, que se encarga de realizar las validaciones
;; y construcciones necesarias.

(define and-exp->clauses
  (lambda (exp)
    (and-exp exp)))



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


;; datatype-or-exp
;; Representación de una expresión or como un datatype.
;; Construye el árbol de sintáxis abstracta basado en datatypes para una expresión or a partir de una lista de números.

(define-datatype datatype-or-exp datatype-or-exp?
  (d-or-exp (exp list-of-numbers?)))


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


;; datatype-and-exp
;; Representación de una expresión and como un datatype.
;; Construye el árbol de sintáxis abstracta basado en datatypes para una expresión and
;; a partir de una lista de expresiones or basadas en datatypes.

(define-datatype datatype-and-exp datatype-and-exp?
  (d-and-exp (exp list-of-or-exps?)))


;; ****************************************** Expresiones fnc ******************************************

;; datatype-fnc-exp
;; Representación de una expresión fnc como un datatype.
;; Construye el árbol de sintáxis abstracta basado en datatypes para una expresión fnc
;; a partir de un número y una expresión and basada en datatypes.

(define-datatype datatype-fnc-exp datatype-fnc-exp?
  (d-fnc-exp (num-of-var number?) (exp datatype-and-exp?)))



;; ################################## Funciones Parse y Unparse ###############################

;; ****************************************** Parsers para síntaxis abstracta basada en listas ******************************************

;; PARSEOR-list:  lista -> or-exp
;; Propósito:
;; Construye el árbol de síntaxs abstracta basado en listas para una expresión or a partir de una lista que cumpla con la gramática.
;; 1) Primero evalúa si la lista pasada como argumento tiene la estructura definida para una expresión or.
;; 2) Después evalúa si la lista tiene un solo elemento, en cuyo caso retorna la lista con este número.
;; 3) Finalmente si la lista tiene más de un elemento, retorna una lista con el primer elemento y
;; el resultado de aplicar recursivamente el PARSEOR-list al resto de la lista.

(define PARSEOR-list
  (lambda (exp)
    (cond

      [(not (or-exp? exp))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una lista que siga la gramática <or-exp> := <int> | <int> OR <or-exp>" exp)]

      [(eqv? (length-of-list exp) 1)
       (cons (car exp) empty)]

      [else
       (cons (car exp) (PARSEOR-list (cddr exp)))])))

;; Pruebas
(PARSEOR-list '(1))
(PARSEOR-list '(1 OR -2))
(PARSEOR-list '(1 OR -2 OR 3 OR -4))

; Reconocimiento de entradas no válidas
;(PARSEOR-list  '())
;(PARSEOR-list '(1 OR not-a-number))
;(PARSEOR-list  '(1 OR))


;; PARSEAND-list:  lista -> and-exp
;; Propósito:
;; Construye el árbol de síntaxs abstracta basado en listas para una expresión and a partir de una lista que cumpla con la gramática.
;; 1) Primero evalúa si la lista pasada como argumento tiene la estructura definida para una expresión and.
;; 2) Después evalúa si la lista tiene un solo elemento, en cuyo caso retorna la lista con este elemento.
;; 3) Finalmente si la lista tiene más de un elemento, retorna una lista con el primer elemento y
;; el resultado de aplicar recursivamente el PARSEAND-list al resto de la lista.

(define PARSEAND-list
  (lambda (exp)
    (cond

      [(not (and-exp? exp))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una lista que siga la gramática <and-exp> := (<or-exp>) | (<or-exp>) AND <and-exp>" exp)]

      [(eqv? (length-of-list exp) 1)
       (cons (or-exp (car exp)) empty)]

      [else
       (cons (or-exp (car exp)) (PARSEAND-list (cddr exp)))])))

;; Pruebas
(PARSEAND-list '((1)))
(PARSEAND-list '((1 OR -2)))
(PARSEAND-list '((1 OR -2) AND (3 OR -4)))
(PARSEAND-list '((1 OR -2) AND (3 OR -4) AND (-5)))

; Reconocimiento de entradas no válidas
;(PARSEAND-list  '())
;(PARSEAND-list '(1))
;(PARSEAND-list '(()))
;(PARSEAND-list '((1 OR -2) AND (3 OR)))


;; PARSEBNF-list:  lista -> fnc-exp
;; Propósito:
;; Construye el árbol de síntaxs abstracta basado en listas para una expresión fnc a partir de una lista que cumpla con la gramática.
;; 1) Primero evalúa si la lista pasada como argumento tiene la estructura definida para una expresión fnc.
;; 2) Si la verificación es correcta, entonces retorna una lista con el segundo y tercer elemento de la lista pasada como argumento.

(define PARSEBNF-list
  (lambda (exp) 
    (if (not (list? exp))
        #f
        (and
         (eqv? (length-of-list exp) 3)
         (eqv? (car exp) 'FNC)
         (number? (cadr exp))
         (and-exp? (caddr exp))))))

;; Pruebas
(PARSEBNF-list '(FNC 1 ((1))))
(PARSEBNF-list '(FNC 2 ((1 OR -2))))
(PARSEBNF-list '(FNC 4 ((1 OR -2) AND (3 OR -4))))
(PARSEBNF-list '(FNC 5 ((1 OR -2) AND (3 OR -4) AND (-5))))

; Reconocimiento de entradas no válidas
;(PARSEBNF-list '())
;(PARSEBNF-list '(FNC not-a-number ((1 OR -2) AND (3 OR -4))))
;(PARSEBNF-list '(FNC not-a-number ((1 OR -2) AND (3 OR -4)) '4th-element))



;; ****************************************** Parsers para síntaxis abstracta basada en datatypes ******************************************

;; PARSEOR-datatype:  lista -> d-or-exp
;; Propósito:
;; Construye el árbol de síntaxs abstracta basado en datatypes para una expresión or a partir de una lista que cumpla con la gramática.
;; 1) Primero evalúa si la lista pasada como argumento tiene la estructura definida para una expresión or.
;; 2) Si la verificación es correcta, entonces retorna el datatype d-or-exp construido a partir de la lista de variables
;; que conforman la expresión or pasada como argumento.

(define PARSEOR-datatype
  (lambda (exp)
   (if (not (or-exp? exp))
        (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una lista que siga la gramática <or-exp> := <int> | <int> OR <or-exp>" exp)
        (d-or-exp (or-exp->varlist exp)))))

;; Pruebas
(PARSEOR-datatype '(1))
(PARSEOR-datatype '(1 OR -2))
(PARSEOR-datatype '(1 OR -2 OR 3 OR -4))

; Reconocimiento de entradas no válidas
;(PARSEOR-datatype '())
;(PARSEOR-datatype '(1 OR not-a-number))
;(PARSEOR-datatype '(1 OR))


;; PARSEAND-datatype:  lista -> d-and-exp
;; Propósito:
;; Construye el árbol de síntaxs abstracta basado en datatypes para una expresión and a partir de una lista que cumpla con la gramática.
;; 1) Primero evalúa si la lista pasada como argumento tiene la estructura definida para una expresión and.
;; 2) Si la verificación es correcta, entonces retorna el datatype d-and-exp construido a partir de la lista de expresiones or
;; que conforman la expresión and pasada como argumento.

(define PARSEAND-datatype
  (lambda (exp)
    (if (not (and-exp? exp))

        (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una lista que siga la gramática <and-exp> := (<or-exp>) | (<or-exp>) AND <and-exp>" exp)
        (d-and-exp (own-map d-or-exp (and-exp->clauses exp)) ))))

;; Pruebas
(PARSEAND-datatype '((1)))
(PARSEAND-datatype '((1 OR -2)))
(PARSEAND-datatype '((1 OR -2) AND (3 OR -4)))
(PARSEAND-datatype '((1 OR -2) AND (3 OR -4) AND (-5)))

; Reconocimiento de entradas no válidas
;(PARSEAND-datatype '())
;(PARSEAND-datatype '(1))
;(PARSEAND-datatype '(()))
;(PARSEAND-datatype '((1 OR -2) AND (3 OR)))


;; PARSEBNF-datatype:  lista -> d-fnc-exp
;; Propósito:
;; Construye el árbol de síntaxs abstracta basado en datatypes para una expresión fnc a partir de una lista que cumpla con la gramática.
;; 1) Primero evalúa si la lista pasada como argumento tiene la estructura definida para una expresión fnc.
;; 2) Si la verificación es correcta, entonces retorna el datatype d-fnc-exp construido a partir del número de variables
;; y la expresión and que conforman la expresión fnc pasada como argumento.

(define PARSEBNF-datatype
  (lambda (exp)
    (if (not (fnc-exp? exp))
        (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una lista que siga la gramática <fnc-exp> := FNC <int> (<and-exp>)" exp)
        (d-fnc-exp (cadr exp) (PARSEAND-datatype (caddr exp))))))

;; Pruebas
(PARSEBNF-datatype '(FNC 1 ((1))))
(PARSEBNF-datatype '(FNC 2 ((1 OR -2))))
(PARSEBNF-datatype '(FNC 4 ((1 OR -2) AND (3 OR -4))))
(PARSEBNF-datatype '(FNC 5 ((1 OR -2) AND (3 OR -4) AND (-5))))

; Reconocimiento de entradas no válidas
;(PARSEBNF-datatype '())
;(PARSEBNF-datatype '(FNC not-a-number ((1 OR -2) AND (3 OR -4))))
;(PARSEBNF-datatype '(FNC not-a-number ((1 OR -2) AND (3 OR -4)) '4th-element))

;; ****************************************** Unparsers para síntaxis abstracta basada en listas ******************************************

;; UNPARSEOR:  or-exp -> lista
;; Propósito:
;; Convierte el arbol de sintaxis abstracta basado en listas de una expresión or a una lista que cumpla con la gramática.
;; 1) Si la exp tiene un solo elemento entonces se retorna exp (Que es una lista con un unico entero).
;; 2) En caso contrario entonces se crea una lista donde el primer elemento es el primer elemento de exp el cual es un numero entero.
;; El segundo elemento consistira en la lista conformada por el simbolo OR y la llamada recursiva de UNPARSEOR pasando como argumento
;; la cola de exp. Este proceso continuara hasta que exp quede con un solo elemento. Momento en el cual se construira la lista con los
;; cons.

(define UNPARSEOR
  (lambda (exp)
    (if (eqv? 1 (length-of-list exp))
        exp
        (cons (car exp)
              (cons 'OR (UNPARSEOR (cdr exp)))))))

;; Pruebas
(UNPARSEOR (PARSEOR-datatype '(1)))
(UNPARSEOR (PARSEOR-datatype '(1 OR -2)))
(UNPARSEOR (PARSEOR-datatype '(1 OR -2 OR 3 OR -4)))


;; UNPARSEAND:  and-exp -> list
;; Propósito:
;; Convierte el arbol de sintaxis abstracta basado en listas de una expresión and a una lista que cumpla con la gramática.
;; 1) Si la exp tiene un solo elemento entonces retorna la lista resultante de invocar UNPARSEOR pasandole como parametro el
;; primer elemento de exp (Sabemos que cada lista que conforma a exp es un lista, que representa una expresio or).
;; 2) En caso contrario entonces se crea una lista donde el primer elemento es el resultado de invocar UNPARSER OR pasandoles como
;; parametro el primer elemento de exp el cual es un expresion or. El segundo elemento consistira en la lista conformada por el simbolo
;; AND y la llamada recursiva de UNPARSEAND pasando como argumento la cola de exp. Este proceso continuara hasta que exp quede con un
;; solo elemento. Momento en el cual se construira la lista con los cons.

(define UNPARSEAND
  (lambda (exp)
    (if (eqv? 1 (length-of-list exp))
        (list (UNPARSEOR (car exp)))
        (cons (UNPARSEOR (car exp))
              (cons 'AND (UNPARSEAND (cdr exp)))))))

;; Pruebas
(UNPARSEAND (PARSEAND-list '((1))))
(UNPARSEAND (PARSEAND-list '((1 OR -2))))
(UNPARSEAND (PARSEAND-list '((1 OR -2) AND (3 OR -4))))
(UNPARSEAND (PARSEAND-list '((1 OR -2) AND (3 OR -4) AND (-5))))


;; UNPARSEBNF:  fnc-exp -> list
;; Propósito:
;; Convierte el arbol de sintaxis abstracta de una expresión fnc basado en listas a una lista que cumpla con la gramática.
;; 1) Crea un lista que contendra el simbolo FNC, el numero de variables (Que sera el primer elemento de exp)
;; y la lista resultado de invocar UNPARSEAND exp a la cola de exp, ya que por la gramatica sabemos que es una expresion d-and.

(define UNPARSEBNF
  (lambda (exp)
    (list 'FNC (car exp)(UNPARSEAND (cadr exp)))))

;; Pruebas
(UNPARSEBNF (PARSEBNF-list '(FNC 1 ((1)))))
(UNPARSEBNF (PARSEBNF-list '(FNC 2 ((1 OR -2)))))
(UNPARSEBNF (PARSEBNF-list '(FNC 4 ((1 OR -2) AND (3 OR -4)))))
(UNPARSEBNF (PARSEBNF-list '(FNC 5 ((1 OR -2) AND (3 OR -4) AND (-5)))))
