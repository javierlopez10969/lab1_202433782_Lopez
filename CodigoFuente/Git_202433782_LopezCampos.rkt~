;Modulo de importaciones
#lang racket
; Zona de modulación.
;(require "TDA_add.rkt")
;(require "TDA_commit.rkt")
;(require "TDA_push.rkt")
;(require "TDA_pull.rkt")
(require "TDA_zonas.rkt") 

;Función : add , añade los cambios locales al index 
;Dominio :
;Recorrido: lista de nombre s de archivos
;Tipo de Recursión: recursion natural o de cola , siendo disntinto
;al de pull
(define (commit string)
  (lambda (zonas)
    (list ("\n" ) )))

;Función : push
;Dominio :
;Recorrido: lista de nombre s de archivos
;Tipo de Recursión: recursion natural o de cola , siendo disntinto
;al de pull
(define (push zona)
  (
   ;mover de remote a local repository
   display("buenardo")))


;Función : add , añade los cambios locales al index 
;Dominio :
;Recorrido: lista de nombre s de archivos
;Tipo de Recursión: recursion natural o de cola , siendo disntinto
;al de pull
(define add
  (lambda (zonas)
    ((zonas)1)))




;Función pull que recibe zonas y mueve de remote a local repository     
;Dominio :
;Recorrido: 
;Tipo de Recursión: recursion natural o de cola , siendo disntinto
;al de add
; Ej (pull zonas)
         ; ((git pull)zonas)
(define (pull)
  (lambda (funcion)
    (funcion)
    ))



;Función git que recibe comandos y devuelve estos mismos
;Dominio :
;Recorrido: Un comando dentro del programa
;Tipo de Recursión:
(define (git comando)
  (cond
    [(equal? comando pull)
    (lambda (zonas)
    ((zonas)1))]

    [(equal? comando push)
    (lambda (zonas)
    ((zonas)1))]

    [(equal? comando add)
    (lambda (zonas)
    ((zonas)2))]

    [(equal? comando commit)
    (lambda (zonas)
    ((zonas)2))]
    ;else
    [else (display "malardo")])
)