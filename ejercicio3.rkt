 #|
Fundamentos de Interpretación y Compilación de Lenguajes de Programación
750017C - G01
=======
Taller 2: Abstracción de datos y sintáxis abstracta
Ejercicio 3: Evaluación de Instancias SAT

Autores:
Samuel Galindo 2177491
Nicolás Herrera 2182551
Christian Vargas 2179172
|#

#lang eopl

(require "ejercicio1.rkt")
(require "ejercicio2.rkt")

;; ************** Funciones propias auxiliares **************

;; my-append:
;; Proposito:
;; lst1 x Llst2 -> L' : Retorna una lista que contiene los elementos
;;                 que pertenecen a L1 y L2.
;;
;; <lista> := () | (<SchemeValue> <lista>)

(define my-append 
  (lambda (lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (my-append (cdr lst1) lst2))))
  )

;; combinations: n -> <listaDeListasBooleana> := ( {<listaBooleana>}* )
;;                    <listaBooleana> := ( {<boolean>}* )
;; Proposito:
;; n -> 'L: retorna una lista de listas de booleans equivalente a todas las posibles combinaciones posibles
;; dependiendo de n
;;
;; Posibles combinaciones = 2^n (n > 0)

(define combinations
  (lambda (n)
  (if (= n 0)
      (list '())
      (my-append (my-map (lambda (rest) (cons #t rest)) (combinations (- n 1)))
              (my-map (lambda (rest) (cons #f rest)) (combinations (- n 1)))
              )
      )
    )
  )

(combinations 1)
(combinations 2)
(combinations 3)
(combinations 4)
(combinations 5)

;; my-or listaBooleana -> boolean
;; Propósito:
;; Implementación propia de la función or para listas.
;; Retorna un boleean tras aplicar la funcion or a una lista de booleans.
;;
;; <listaBooleana> := ( {<boolean>}* )

(define my-or
 (lambda (l1)
   (if (null? l1) #f
       (or (car l1) (my-or (cdr l1)))
       )))

(my-or '(#t #f #t #f));#t
(my-or '(#f #f #f #f));#f
(my-or '(#t #f #t #f #t #f #f));#t
(my-or '(#f #f));#f
(my-or '(#t #t #t #t));#t

;; my-and: listaBooleana -> boolean
;; Propósito:
;; Implementación propia de la función and para listas.
;; Retorna un boleean tras aplicar la funcion and a una lista de booleans.
;;
;; <listaBooleana> := ( {<boolean>}* )

(define my-and
  (lambda (l1)
    (if (null? l1) #t
        (and (car l1) (my-and (cdr l1)))
        )))

(my-and '(#t #f #t #f));#f
(my-and '(#f #f #f #f));#f
(my-and '(#t #f #t #f #t #f #f));#f
(my-and '(#f #f));#f
(my-and '(#t #t #t #t));#t

;; ************** Funciones propias prinicpales **************

;; solve-FNC: and-exp, listaBooleana -> boolean
;; Propósito:
;; Utiliza la función my-map para convertir cada numero de una orExpression a un boolean, ademas para cada expresión or 
;; aplica la función my-or retornando una lista de booleans, a la que se le aplica la función my-and para retornar el 
;; resultado de esa expresion FNC.
;; <listaBooleana> := ( {<boolean>}* )

(define solve-FNC
 (lambda (andexp lista)
   (my-and(my-map (lambda (x) ;; aplica my-and a cada boolean obtenido del sigiente map.
          (my-or (my-map (lambda (y) (if (> y 0) (get-element-at-index lista (- y 1)) ;; aplica my-or a cada orExpression.
                                                 (not (get-element-at-index lista (- (abs y) 1))))) ;; reemplaza cada elemento de la expresion or con el elemento de la posicion correspondiente de la lista de booleans
                           (or-exp->varlist x))))
                    andexp)
   )))

(solve-FNC '((1 -2 3 4) (-2 3) (-1 -2 -3) (3 4) (2))  '(#t #f #f #f)) ;#f
(solve-FNC '((1 -2 3 4) (-2 3) (-1 -2 -3) (3 4) (2))  '(#f #t #t #t)) ;#t
(solve-FNC '((1 2) (-1) (-2)) '(#t #t)) ;#f
(solve-FNC '((1 2 3) (-1 2) (-3 2) (-1 -3)) '(#t #t #t)) ;#f
(solve-FNC '((1 2 3) (-1 2) (-3 2) (-1 -3)) '(#t #t #f)) ;#t


;; travelSols listaBooleana, and-exp -> String
;; Propósito:
;; Recorre cada posible solución hasta que encuentra una satisfactible, sino encuentra una satisfactible retorna 
;; el String correspondiente

;; <and-exp> := (<or-exp>) | (<or-exp>) <and-exp> 
;; <listaBooleana> := ( {<boolean>}* )

(define travelSols
  (lambda (sols andExp)
    (cond
      [(null? sols) (cons 'insatisfactible '('()))]
      [else (if (solve-FNC andExp (car sols)) (cons 'satisfactible (list (car sols)))
        (travelSols (cdr sols) andExp))]
      )))

(travelSols (combinations 4) '((1 -2 3 4) (-2 3) (-1 -2 -3) (3 4) (2))) ;(satisfactible (#f #t #t #t))
(travelSols (combinations 2) '((1 2) (-1) (-2))) ;(insatisfactible '())
(travelSols (combinations 3) '((1 2 3) (-1 2) (-3 2) (-1 -3))) ;(satisfactible (#t #t #f))
(travelSols (combinations 4) '((1 4) (-1 2 -3 4) (1 3) (-2 -3) (-2 -3 -4))) ;(satisfactible (#t #t #f #t))
(travelSols (combinations 5) '((1 2 3 4 5) (-1 -2) (1 2 3) (4 5) (-1 -3) (1 2) (1) (-1))) ; (insatisfactible '())


;; EVALUARSAT: fnc-exp -> String
;; Propósito:
;; Utiliza la función travelSols para poder probar cada posible solución generada por la funcion combinations para una
;; expression and.
;;
;; <and-exp> := (<or-exp>) | (<or-exp>) <and-exp> 
;; <listaBooleana> := ( {<boolean>}* )

(define EVALUARSAT
 (lambda (fnc-e)
 (travelSols (combinations (fnc-exp->var fnc-e)) (fnc-exp->clauses fnc-e))
       ))

(EVALUARSAT '(4 ((1 -2 3 4) (-2 3) (-1 -2 -3) (3 4) (2)))) ;(satisfactible (#f #t #t #t))
(EVALUARSAT '(2 ((1 2) (-1) (-2)))) ;(insatisfactible '())
(EVALUARSAT '(3 ((1 2 3) (-1 2) (-3 2) (-1 -3)))) ;(satisfactible (#t #t #f))
(EVALUARSAT '(4 ((1 4) (-1 2 -3 4) (1 3) (-2 -3) (-2 -3 -4)))) ;(satisfactible (#t #t #f #t))
(EVALUARSAT '(5 ((1 2 3 4 5) (-1 -2) (1 2 3) (4 5) (-1 -3) (1 2) (1) (-1)))) ; (insatisfactible '())






