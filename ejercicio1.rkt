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

(define expr-fnc '(FNC 4 ((1 or 2 or -4) and (2 or 4 or 3) and (-2) and (1))))

(define FNC
  (lambda (int exp) (list 'FNC int exp)))

(define or
  (lambda (int or)
    (if (null? or)
        (list int)
        (append (list int 'V) or))))

(define and
  (lambda (or exp)
    (if(null? exp)
       (list or)
       (cons or(cons 'AND exp)))))

(define fnc->var
  (lambda (fnc)
    (cadr fnc)))

(define fnc->clausula
  (lambda (fnc)
    (cond
    [(null? fnc) '()]
    [(list? (car fnc))
     (cons (car fnc) (fnc->clausula (cdr fnc)))]
    [else (fnc->clausula (cdr fnc))])))

(define or->varlist
  (lambda (or)
    (if (null? or)
        '()
        (cond
          [(number? (car or)) (cons(car or)(or->varlist(cdr or)))]
          [else (or->varlist(cdr or))]))))

(FNC 4 (and (or 2 '()) (and (or 2 (or 3 '())) (or 1 (or -3 '())))))