#lang racket
;ZONA WOKSPACE : Que tiene una lista con los archivos
(define workspace (list ""))

;Constructores

;crear archivo

;Editar archivo

;Mostrar arvhivo

;Funciones de Pertenencia;

;Selectores

;Modificadores

;Otras Funciones.



;Constructores
;Funciones de Pertenencia;
;Selectores
;Modificadores
;Otras Funciones.


(define local-repository (list))
;Representación

;Constructores
;Funciones de Pertenencia;
;Selectores
;Modificadores
;Otras Funciones.
(define remote-repository '())
;Representación

;Constructores
;Funciones de Pertenencia;
;Selectores
;Modificadores
;Otras Funciones.
  
;ZONA INDEX
(define index (list ""))

;Constructores
;Funciones de Pertenencia;
;Selectores
;Modificadores
;Otras Funciones.

;Función : ZONAS , consta de 4 zonas
;Dominio : zona con la cual se va a ingresar
;Recorrido: lista de nombre s de archivos
;Tipo de Recursión:

(define (zonas)
  (lambda (n)
    (cond
      ;caso pull
      [(= n 1) remote-repository ]
      ;
      [(= n 2) workspace]
      ;
      [(= n 3) index]
      ;
      [(= n 4) local-repository]
      ;else
      )
  ))
;ZONAS TO STRING
;DEVUELVE TODAS LAZ ZONAS EN FORMA DE STRING
;append string
(define (zonas->string)
  (define (string-salida )
    (cond
        ;pregunto si acaso no estan vacías cada una de las zonas
        [(not(null? remote-repository )) "kk" ]
        [(not(null? local-repository )) "hola"] 
      ) )(null) )  
(provide zonas)
(provide zonas->string)