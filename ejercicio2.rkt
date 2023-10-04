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

;; ****************************************** Funciones propias ******************************************

;; my-map: lista -> lista
;; Propósito:
;; Implementación propia de la función map para listas.
;; Retorna una lista con los resultados de aplicar la función pasada como argumento a cada elemento de la lista pasada como argumento.
;;
;; <lista> := ( {<SchemeValue>}* )

(define my-map
  (lambda (func list-of-elements)
    (if (null? list-of-elements)
        empty
        (cons (func (car list-of-elements)) (my-map func (cdr list-of-elements))))))

;; Pruebas
(my-map (lambda (x) (* 2 x)) '(1 2 3 4))
(my-map (lambda (x) (+ 100 x)) '(1 2 3 4))
(my-map (lambda (x) (if (even? x) x (* 10 x))) '(1 2 3 4))


;; ignore-outer-symbols:  lista -> lista
;; Propósito:
;; Dado una lista escrita con la sintaxis concreta definida para expresiones or o and, 
;; retorna una lista con los elementos de la lista pasada como argumento que no son símbolos.
;; Solo ignora los símbolos que están en el nivel más externo de la lista.

(define ignore-outer-symbols
  (lambda (exp)
    (if (eqv? (length-of-list exp) 1)
        (cons (car exp) empty)
        (cons (car exp) (ignore-outer-symbols (cddr exp))))))

;; Pruebas
(ignore-outer-symbols '(1))
(ignore-outer-symbols '(1 OR -2))
(ignore-outer-symbols '((1 OR -2) AND (3 OR -4)))
(ignore-outer-symbols '((1 OR -2) AND (3 OR -4) AND (-5)))



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
;; 2) En caso de que la verificación sea correcta,  entonces ignora los símbolos 'OR de la lista, quedandose unicamnete con los números.
;; A partir de esta lista de números construye el árbol de sintaxis abstracta basado en listas, usando el constructor or-exp.

(define PARSEOR
  (lambda (exp)
    (if (not (or-exp? exp))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una lista que siga la gramática <or> := <int> | <int> 'OR <or>" exp)
       (or-exp (ignore-outer-symbols exp)))))

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
;; 2) Si la verificación es correcta,  entonces ignora los símbolos 'AND de la lista, quedandose unicamnete con las expresiones or.
;; A partir de esta lista de expresiones or realiza un parseo sobre cada una de ellas, utilizando la función PARSEOR.
;; Finalmente construye el árbol de sintaxis abstracta basado en listas, usando el constructor and-exp.

(define PARSEAND
  (lambda (exp)
    (if (not (and-exp? exp))
        (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una lista que siga la gramática <and> := (<or>) | (<or>) 'AND <and>" exp)
        (and-exp (my-map PARSEOR (ignore-outer-symbols exp))))))

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
(eqv? (fnc-exp? '()) #f)
(eqv? (fnc-exp? '(FNC 'not-a-number ((1 OR 2 OR -3) AND (2)))) #f)
(eqv? (fnc-exp? '(FNC 1 ((1 OR 2 OR -3) AND (2 OR 1) AND (2)) '4th-element)) #f)


;; PARSEBNF:  lista -> fnc-exp
;; Propósito:
;; Construye el árbol de sintaxs abstracta basado en listas para una expresión fnc a partir de una lista que cumpla con la gramática.
;; 1) Primero evalúa si la lista pasada como argumento sigue la sintaxis concreta definida para una expresión fnc.
;; 2) Si la verificación es correcta, entonces realiza un parseo sobre la expresión and, utilizando la función PARSEAND.
;; Finalmente construye el árbol de sintaxis abstracta basado en listas, usando el constructor fnc-exp, pasandole como argumentos
;; el número de variables y la expresión and parseada.

(define PARSEBNF
  (lambda (exp) 
    (if (not (fnc-exp? exp))
       (eopl:error 'IllegalArgumentError "El argumento ~s debe ser una lista que siga la gramática <fnc> := 'FNC <int> (<and>)" exp)
       (fnc-exp (cadr exp) (PARSEAND (caddr exp))))))
        
;; Pruebas
(PARSEBNF '(FNC 1 ((1))))
(PARSEBNF '(FNC 2 ((1 OR -2))))
(PARSEBNF '(FNC 4 ((1 OR -2) AND (3 OR -4))))
(PARSEBNF '(FNC 5 ((1 OR -2) AND (3 OR -4) AND (-5))))
; Reconocimiento de entradas no válidas
;(PARSEBNF '())
;(PARSEBNF '(FNC not-a-number ((1 OR -2) AND (3 OR -4))))
;(PARSEBNF '(FNC not-a-number ((1 OR -2) AND (3 OR -4)) 4th-element))



;; ****************************************** Unparsers para síntaxis abstracta basada en listas ******************************************

;; UNPARSEOR:  or-exp -> lista
;; Propósito:
;; Convierte el árbol de sintaxis abstracta basado en listas de una expresión or a una lista que cumpla con la gramática.
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
(UNPARSEOR (PARSEOR '(1)))
(UNPARSEOR (PARSEOR '(1 OR -2)))
(UNPARSEOR (PARSEOR '(1 OR -2 OR 3 OR -4)))


;; UNPARSEAND:  and-exp -> list
;; Propósito:
;; Convierte el árbol de sintaxis abstracta basado en listas de una expresión and a una lista que cumpla con la gramática.
;; 1) Si la exp tiene un solo elemento entonces retorna la lista resultante de invocar UNPARSEOR pasándole como parametro el
;; primer elemento de exp (Sabemos que cada lista que conforma a exp es un lista, que representa una expresión or).
;; 2) En caso contrario entonces se crea una lista donde el primer elemento es el resultado de invocar UNPARSER OR pasandoles como
;; parámetro el primer elemento de exp el cual es un expresión or. El segundo elemento consistirá en la lista conformada por el símbolo
;; AND y la llamada recursiva de UNPARSEAND pasando como argumento la cola de exp. Este proceso continuará hasta que exp quede con un
;; solo elemento, momento en el cual se construirá la lista con los cons.

(define UNPARSEAND
  (lambda (exp)
    (if (eqv? 1 (length-of-list exp))
        (list (UNPARSEOR (car exp)))
        (cons (UNPARSEOR (car exp))
              (cons 'AND (UNPARSEAND (cdr exp)))))))

;; Pruebas
(UNPARSEAND (PARSEAND '((1))))
(UNPARSEAND (PARSEAND '((1 OR -2))))
(UNPARSEAND (PARSEAND '((1 OR -2) AND (3 OR -4))))
(UNPARSEAND (PARSEAND '((1 OR -2) AND (3 OR -4) AND (-5))))


;; UNPARSEBNF:  fnc-exp -> list
;; Propósito:
;; Convierte el árbol de sintaxis abstracta de una expresión fnc basado en listas a una lista que cumpla con la gramática.
;; 1) Crea un lista que contendrá el símbolo FNC, el número de variables (que será el primer elemento de exp)
;; y la lista resultado de invocar UNPARSEAND exp a la cola de exp, ya que por la gramática sabemos que es una expresion and.

(define UNPARSEBNF
  (lambda (exp)
    (list 'FNC (car exp)(UNPARSEAND (cadr exp)))))

;; Pruebas
(UNPARSEBNF (PARSEBNF '(FNC 1 ((1)))))
(UNPARSEBNF (PARSEBNF '(FNC 2 ((1 OR -2)))))
(UNPARSEBNF (PARSEBNF '(FNC 4 ((1 OR -2) AND (3 OR -4)))))
(UNPARSEBNF (PARSEBNF '(FNC 5 ((1 OR -2) AND (3 OR -4) AND (-5)))))
