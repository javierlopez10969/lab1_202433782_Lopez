#lang racket

;ZONA WOKSPACE : Que tiene una lista con los archivos
;representados con una lista de listas donde el primer elemento de cada lsita
;es el nombre y el resto sus lineas de código
(define workspace (list  (list "main.c\n"
                               "#include <stdio.h> \n"
                               "int main (){ \n"
                               "printf('Hola') }\n"
                               )
                         (list "busqueda.h\n"
                               "#include <stdio.h>\n"
                               "//funcion que realiza una busqueda") ) )
;Representación
(define index (list)
  )
;Representación

(define local-repository (list))

;Representación:
;Una lista de listas que contiene todos los commits realizados
(define remote-repository (list
                           (list
                            "Mi primer commit ")
                            ))

;Función : ZONAS , consta de 4 zonas
;Dominio : Entero que representa zona con la cual se va a ingresar
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

(define stringSalida "")
;FUNCIONES NECESARIAS
;calcular longitud o length
;Recursion Natural  , ya que deja espacios pendientes
( define (longitud lista)
   ( cond
      [( null? lista) 0]
      [ else (+ 1 (longitud (cdr lista) ) ) ]) )
;Función : Que recorre lista de listas y muestra cada elemento como un string
;Dominio : Lista , N total de listas Entero , I Iterador Entero que se va moviendo
;entre las listas particulares de la lista general
;Recorrido: String x String 
;Tipo de Recursión: recursión de cola , ya que no deja espacios 
(define recorrer-workspace
      (lambda (listaG listaP n i string)
        ;Casos borde
        ;Caso de Lista Particular pregunto si acaso queda un único elemento en la lista que estoy analizando
        (cond [(= 1 (longitud listaP) )
               ;True Case  :
               ;Devuelve el único elemento de la lista y pregunto si puedo seguir avanzando
               ;pregunto si el iterador sumado con uno es menor que el largo total
               (cond [ (< (+ 1 i) n)
                       ;en caso de serlo retorno el workspace con el iterador sumandole uno
                       ;y paso a la siguiente lista particular
                       (recorrer-workspace listaG (list-ref listaG (+ 1 i)) n (+ i 1) (string-append string (car listaP))) ]
                     ;Else
                     [else (string-append string (car listaP)) ])]
              ;Else :
              ;en caso de que el largo de la lista particular no se encuentre con un solo elemento
              ;Obtengo el primer elemento de la lista que estoy viendo
              [ else
                ;Ahora hago recursión con lo que queda de la lista
                (recorrer-workspace listaG (cdr listaP) n i (string-append string (car listaP) " ") ) ]
              )
        )
  )
(define string-workspace
  (lambda ()
    ;pregunto si acaso no estan vacías cada una de las zonas
    (if (not (null? workspace))
        (recorrer-workspace workspace (car workspace) (longitud workspace) 0 stringSalida)
        #f))
  )

;ZONAS TO STRING
;DEVUELVE TODAS LAZ ZONAS EN FORMA DE STRING
;IDEA : filter string ?  (rest remote-repository)
(define zonas->string
  (lambda ()
    (string-workspace)
    
    )
  )

;Función que cambia elemento dentro de una lista
;DOMINIO : LISTA x ENTERO x ELEMENTO
;RECORRIDO : LISTA
;RECURSIÓN : NATURAL
(define cambiar-elemento
  (lambda (lista posicion nuevo-elemento)
  ;Pregunto si la lista es nula 
  (if (null? lista)
      ;Caso base , cuando ya termine de de recorrer toda la lista
      lista
      ;Else recursivo
      ;CREO UNA NUEVA LISTA reconstruyendola toda
      (cons
       ;pregunto si ya llege a mi posición deseada
       ;en caso de serlo devuelvo el elemento
       (if (zero? posicion) nuevo-elemento (car lista))
       ;En caso contrario sigo recorriendo la lista recursivamente 
     (cambiar-elemento (cdr lista) (- posicion 1) nuevo-elemento) ) ) ) )

;Función que cambia elemento dentro de una lista
;DOMINIO : LISTA x LISTA
;RECORRIDO : LISTA
;RECURSIÓN : NATURAL
(define reconstruir-lista
  (lambda (lista lista-nueva)
    ;Pregunto si la lista es nula 
    (if (null? lista)
        ;Caso base , cuando ya termine de de recorrer toda la lista
        lista-nueva
        ;Else recursivo
        ;CREO UNA NUEVA LISTA reconstruyendola toda
        (reconstruir-lista (cdr lista) lista-nueva)
        ) ) )


(provide zonas)
(provide zonas->string)