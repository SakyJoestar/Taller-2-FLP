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

;; ****************************************** Funciones propias ******************************************

;; Funcion my-append que ya sabemos que hace

(define my-append 
  (lambda (lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (my-append (cdr lst1) lst2))))
  )

;; Función map que ya sabemos que hace tambien
(define my-map
  (lambda (proc lst)
  (if (null? lst)
      '()
      (cons (proc (car lst))
            (my-map proc (cdr lst)))))
  )
;; Función que genera todas las combinaciones
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Solucion1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Función que retorna #t o #f para una posible combincación y una expresion FNC.

(define solve-FNC
  (lambda (exp lista)
  (cond
    ;[(null? exp) #t] ;; caso cuando un or queda vacio
    [(number? exp) (if (> exp 0) (get-element-at-index lista (- exp 1)) ;; caso cuando un or es un numero positivo
                                 (not (get-element-at-index lista (- (abs exp) 1)))  ;; caso cuando un or es un numero negativo
                                                  )]
    [(list? exp)
     (cond
       [(= (length-of-list exp) 1) (solve-FNC (car exp) lista)] ;; caso cuando exp es una lista de tamano 1 (cuando solo queda una expresion or dentro del and)
       [(number? (car exp)) (or (solve-FNC (car exp) lista) (solve-FNC (cdr exp) lista))] ;; cuando el primer elemento de exp es un numero (expresion or)
       [(list? (car exp)) (and (solve-FNC (car exp) lista) (solve-FNC (cdr exp) lista))] ;; cuando el primer elemento es una lista (exp es una expresion and)
       )])))

(define l1 '((1 -2) (3 -4) (-5)))

(solve-FNC l1 '(#t #t #f #f #t))

;;Función EvaluarSat que pasa los datos a solSAT para dar una respuesta
(define EVALUARSAT
 (lambda (fnc-e)
 (travelSols (combinations (car fnc-e)) (cdr fnc-e))
       ))

;;Función solSat que recorre las combinaciones
(define travelSols
  (lambda (lbs exp)
    (cond
      [(null? lbs) (cons 'insatisfactible '('()))]
      [else (if (solve-FNC exp (car lbs)) (cons 'satisfactible (list (car lbs)))
        (travelSols (cdr lbs) exp))]
      )))

;; ejemplos profesor
(EVALUARSAT '(4 ((1 -2 3 4) (-2 3) (-1 -2 -3) (3 4) (2)))) ;(satisfactible (#f #t #t #t))
(EVALUARSAT '(2 ((1 2) (-1) (-2)))) ;(insatisfactible '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Solucion2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define solve-FNC2
 (lambda (andexp lista)
  (cond
  [(null? andexp) #t] ;; si todos dan true la lista original quedara vacia
  [(list? (caar andexp)) (solve-FNC2 (car andexp) lista)] ;; reduce la lista cuando entra por primera vez
  [else ;; evalua cada or, si uno retorna #f se termina, sino evalua el siguiente.
   (letrec
       (
       (solveOr
        (lambda (orexp)
          (cond
            [(null? orexp) #f ] ;; caso base, neutro del or
            [(number? orexp) (if (> orexp 0) (get-element-at-index lista (- orexp 1)) ;; caso cuando un or es un numero positivo
                                   (not (get-element-at-index lista (- (abs orexp) 1))))] ;; caso cuando un or es un numero negativo
            
            [(list? orexp) (or (solveOr (car orexp)) (solveOr (cdr orexp)))] ;; si la expresion es un or, realiza un or por cada elemento.
            
            ))))
     (if (solveOr (car andexp)) (solve-FNC2 (cdr andexp) lista) ;; si solveOr devuelve true, llama la función con la siguiente expresion or
         #f)     
     )
   ])))

(solve-FNC2 l1 '(#t #t #f #f #t))

;;Función EvaluarSat que pasa los datos a solSAT para dar una respuesta
(define EVALUARSAT2
 (lambda (fnc-e)
 (travelSols2 (combinations (car fnc-e)) (cdr fnc-e))
       ))

;;Función solSat que recorre las combinaciones
(define travelSols2
  (lambda (lbs exp)
    (cond
      [(null? lbs) (cons 'insatisfactible '('()))]
      [else (if (solve-FNC2 exp (car lbs)) (cons 'satisfactible (list (car lbs)))
        (travelSols (cdr lbs) exp))]
      )))

;; ejemplos profesor
(EVALUARSAT2 '(4 ((1 -2 3 4) (-2 3) (-1 -2 -3) (3 4) (2)))) ;(satisfactible (#f #t #t #t))
(EVALUARSAT2 '(2 ((1 2) (-1) (-2)))) ;(insatisfactible '())


  
  
      
 