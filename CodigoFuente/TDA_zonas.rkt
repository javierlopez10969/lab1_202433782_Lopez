#lang racket
;Constructores
;Funciones de Pertenencia;
;Selectores
;Modificadores
;Otras Funciones.
;ZONA WOKSPACE
(define workspace (list ""))
;Representación

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
(define remote-repository '("caca"))
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
      [(= n 1) (remote-repository) ]
      ;
      [(= n 2) (workspace)]
      ;
      [(= n 3) (index)]
      ;
      [(= n 4) (local-repository)]
      ;else
      )
  ))
  
(provide zonas)
(provide index)
(provide remote-repository)
(provide local-repository)
(provide workspace)