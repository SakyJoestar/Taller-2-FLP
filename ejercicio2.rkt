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

(require "ejercicio1.rkt")
(provide (all-defined-out))



;; ################################## Funciones Parse y Unparse ###############################

;; ****************************************** Parsers para síntaxis abstracta basada en listas ******************************************

;; or-exp?:  lista -> boolean
;; Propósito:
;; Identifica si una lista está construida usando la sintaxis concreta definida para una expresión or.
;; 1) Primero evalúa si la expresión pasada como argumento es una lista no vacía.
;; 2) Después evalúa si la lista tiene un solo elemento, en cuyo caso verifica que sea un número.
;; 3) Finalmente si la lista tiene más de un elemento, verifica que el primer elemento sea un número,
;; que el segundo sea el símbolo 'OR, y que el resto de la lista no sea vacía y sea una expresión or.

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


;; PARSEOR:  lista -> or-exp
;; Propósito:
;; Construye el árbol de síntaxs abstracta basado en listas para una expresión or a partir de una lista que cumpla con la gramática.
;; 1) Primero evalúa si la lista pasada como argumento sigue la sintaxis concreta definida para una expresión or.
;; 2) Después evalúa si la lista tiene un solo elemento, en cuyo caso retorna la lista con este número.
;; 3) Finalmente si la lista tiene más de un elemento, retorna una lista con el primer elemento y
;; el resultado de aplicar recursivamente el PARSEOR al resto de la lista.

(define PARSEOR
  (lambda (exp)
    (cond

      [(not (or-exp? exp))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una lista que siga la gramática <or> := <int> | <int> 'OR <or>" exp)]

      [(eqv? (length-of-list exp) 1)
       (cons (car exp) empty)]

      [else
       (cons (car exp) (PARSEOR (cddr exp)))])))

;; Pruebas
(PARSEOR '(1))
(PARSEOR '(1 OR -2))
(PARSEOR '(1 OR -2 OR 3 OR -4))
; Reconocimiento de entradas no válidas
;(PARSEOR  '())
;(PARSEOR '(1 OR not-a-number))
;(PARSEOR  '(1 OR))


;; and-exp?:  lista -> boolean
;; Propósito:
;; Identifica si una lista está construida usando la sintaxis concreta definida para una expresión and.
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


;; PARSEAND:  lista -> and-exp
;; Propósito:
;; Construye el árbol de síntaxs abstracta basado en listas para una expresión and a partir de una lista que cumpla con la gramática.
;; 1) Primero evalúa si la lista pasada como argumento sigue la sintaxis concreta definida para una expresión and.
;; 2) Después evalúa si la lista tiene un solo elemento, en cuyo caso retorna la lista con este elemento.
;; 3) Finalmente si la lista tiene más de un elemento, retorna una lista con el primer elemento y
;; el resultado de aplicar recursivamente el PARSEAND al resto de la lista.

(define PARSEAND
  (lambda (exp)
    (cond

      [(not (and-exp? exp))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una lista que siga la gramática <and> := (<or>) | (<or>) 'AND <and>" exp)]

      [(eqv? (length-of-list exp) 1)
       (cons (or-exp (car exp)) empty)]

      [else
       (cons (or-exp (car exp)) (PARSEAND (cddr exp)))])))

;; Pruebas
(PARSEAND '((1)))
(PARSEAND '((1 OR -2)))
(PARSEAND '((1 OR -2) AND (3 OR -4)))
(PARSEAND '((1 OR -2) AND (3 OR -4) AND (-5)))
; Reconocimiento de entradas no válidas
;(PARSEAND  '())
;(PARSEAND '(1))
;(PARSEAND '(()))
;(PARSEAND '((1 OR -2) AND (3 OR)))


;; fnc-exp?:  lista -> boolean
;; Propósito:
;; Identifica si una lista está construida usando la sintaxis concreta definida para una expresión fnc.
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


;; PARSEBNF:  lista -> fnc-exp
;; Propósito:
;; Construye el árbol de síntaxs abstracta basado en listas para una expresión fnc a partir de una lista que cumpla con la gramática.
;; 1) Primero evalúa si la lista pasada como argumento sigue la sintaxis concreta definida para una expresión fnc.
;; 2) Si la verificación es correcta, entonces retorna una lista con el segundo elemento y el resultado de aplicar
;; el PARSEAND al tercer elemento.

(define PARSEBNF
  (lambda (exp) 
    (if (not (list? exp))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una lista que siga la gramática <fnc> := 'FNC <int> (<and>)" exp)
       (cons (cadr exp) (PARSEAND (caddr exp))))))

;; Pruebas
(PARSEBNF '(FNC 1 ((1))))
(PARSEBNF '(FNC 2 ((1 OR -2))))
(PARSEBNF '(FNC 4 ((1 OR -2) AND (3 OR -4))))
(PARSEBNF '(FNC 5 ((1 OR -2) AND (3 OR -4) AND (-5))))
; Reconocimiento de entradas no válidas
;(PARSEBNF '())
;(PARSEBNF '(FNC not-a-number ((1 OR -2) AND (3 OR -4))))
;(PARSEBNF '(FNC not-a-number ((1 OR -2) AND (3 OR -4)) '4th-element))

