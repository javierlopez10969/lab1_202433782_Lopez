#lang racket

;ZONA WOKSPACE : Que tiene una lista con los archivos
;representados con una lista de listas donde el primer elemento de cada lsita
;es el nombre y el resto sus lineas de código
(define workspace (list  (list "main.c"
                           "#include <stdio.h> \n"
                            "int main (){ \n"
                            "printf('Hola') }"
                            )
                         (list "busqueda.h"
                               ) ) )

(define local-repository (list))

;Representación

(define remote-repository (list
                           "#include <stdio.h> \n"
                            "int main (){ \n"
                            "printf('Hola') }"
                            ))
;Representación
  
;ZONA INDEX
(define index (list ""))

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

 ;FUNCIONES NECESARIAS
 ;calcular longitud o length
( define (longitud lista)
   ( cond
      [( null? lista) 0]
      [ else (+ 1 (longitud (rest lista) ) ) ]) )
  ;Recorrer lista
  ( define recorrer
     (lambda (lista)
       (cond
         ;Caso borde
         [(= 1 (longitud lista)) (first lista) ]
         ;obtengo el primero de la lista
         [else (first lista) (recorrer (cdr lista))]
         ) ) )
;ZONAS TO STRING
;DEVUELVE TODAS LAZ ZONAS EN FORMA DE STRING
(define zonas->string
  (lambda ()
    (cond
      ;pregunto si acaso no estan vacías cada una de las zonas
      ;IDEA : filter string ?  (rest remote-repository)
      [(not(null? remote-repository ))
       (recorrer remote-repository)]
      [(not(null? local-repository )) "hola"]
      [(not(null? workspace )) "hola"]
      [(not(null? index )) "hola"]
      [else "nothing"]
        
      ) ) )

(provide zonas)
(provide zonas->string)