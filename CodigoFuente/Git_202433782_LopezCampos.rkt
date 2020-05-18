;Modulo de importaciones
#lang racket
; Zona de modulación.
;(require "TDA_add.rkt")
;(require "TDA_commit.rkt")
;(require "TDA_push.rkt")
;(require "TDA_pull.rkt")
(require "TDA_zonas.rkt") 

  ;Función : Commit que añade los commit del index al local repository repository
;Dominio : Dos variables "string" y zonas
;Recorrido: lista de nombre s de archivos
;Tipo de Recursión: recursion natural o de cola , siendo disntinto
;al de pull
(define (commit)
  (lambda (string)
    (if (string? string)
        ;True case
        (lambda (funcion)
          (if (equal? funcion zonas)
              ;true case
              "Movemos hacia el index"
              ;else
              (null) )
          ;Movemos los commit de index a Local-Repository
          )
        ;Else
        ((null) ) ) ) )

;Función : push
;Dominio :
;Recorrido: lista de nombre s de archivos
;Tipo de Recursión: recursion natural o de cola , siendo disntinto
;al de pull
(define (push)
  (lambda (funcion)
    (if (equal? funcion zonas)
        ;True Case
        ;mover desde local repository a remote repository
        ("Movemos los commit al remote repository")
        ;else
        (display "No ha ingresado el parametro correcto") ) ) )


;Función : add , añade los cambios locales al index 
;Dominio :
;Recorrido: lista de nombre s de archivos
;Tipo de Recursión: recursion natural o de cola , siendo disntinto
;al de pull
(define add
  (lambda (lista)
    (if (list? lista)
        ;True case
        (lambda (funcion)
          (if (equal? funcion zonas)
              ;añadimos
              "Añadir"
              "Ignroamos ") )
        ;False
        ("ingrese una lista") )
    ) )

(define add-all
  (lambda (funcion)
    ((zonas)1)))




;Función pull que recibe zonas y mueve de remote a local repository     
;Dominio :
;Recorrido: 
;Tipo de Recursión: recursion natural o de cola , siendo disntinto al de add
; Ej (pull zonas)
; ((git pull)zonas)
(define (pull)
  (lambda (funcion)
    (if (equal? funcion zonas)
        ;true case , pregunto si acaso la lista se encuentra vacía 
        (if(empty?((zonas)1) )
           ;en caso de estarlo digo que el remote repository esta vacío
           
           "El remote repository se encuentra vacío"
            
           ;else , de caso contrario
           ;RETORNO UNA LISTA CON TODOS LOS COMMITS A TRAVES DE UNA RECURSIÓN NATURAL
           ;cada commit en una lista y lo paso al local repository
          (reconstruir-lista ((zonas)1) ((zonas)4) )  )
        ;else
        (null))
    )
  )

;Función git que recibe comandos y devuelve estos mismos
;Dominio :
;Recorrido: Un comando dentro del programa
;Tipo de Recursión:
(define git
  (lambda (comando)
  (cond
    [(equal? comando pull)
    (pull)]

    [(equal? comando push)
     (push)]

    [(equal? comando add)
     (add)]

    [(equal? comando commit)
     (commit)]
    ;else
    [else (display "Comando Inválido")])
    )
  )