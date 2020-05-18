#lang racket
(define index (list)
  )
;Representación

(define local-repository (list))

;Representación:
;Una lista de listas que contiene todos los commits realizados
(define remote-repository
  (list
   (list
    "Mi primer commit ")
   (list
    "nombre del commit"
    ;Archivos cambiados
    (list "archivo.c" )
    ;acciones realizadas dentro de los archivos
    "+ 59 insertions"
    "+ 60 dele"
    "\n"
    "create mode CodigoFuente.rkt"
    "delete mode")
   )
  )
;---------------------------------------------------------------------------------------------------------------------------
;TDA ZONAS
;Función : ZONAS , FUNCION CONSTANTES , consta de 4 zonas que se obtienen por su ref
; 0 : WORKSPACE ; 1 :INDEX ; 2 : LOCAL-REPOSITORY ; 3 :
;Dominio : LISTA DE LISTAS
;Recorrido: lista de nombre s de archivos
;Tipo de Recursión:
(define zonas
  (list
   ;WORKSPACE ref 0 , car , first
   ;Representación: tiene una lista con los archivos
   ;representados con una lista de listas donde el primer elemento de cada lsita
   ;es el nombre y el resto sus lineas de código
   (list
    (list "main.c\n"
          "#include <stdio.h> \n"
          "int main (){ \n"
          "printf('Hola') }\n"
          )
    (list "busqueda.h\n"
          "#include <stdio.h>\n"
          "//funcion que realiza una busqueda")
    )
   )
  )
;SELECTOR
;GET-ZONA , obtengo una zona especifica requerida segun el momento
;DOMINIO : STRING
;RECORRIDO :LISTA X LISTAS || STRING
;RECURSION
(define get-zona
  (lambda (string-zona)
    ;Primero de aseguro que la entrada sea string
    (if (string? string-zona)
        ;TRUE CASE
        (cond
          [(equal? string-zona "workspace")
           (if (not (null? (car zonas)))
               ;true case
               (car zonas)
               ;else
               "Workspace vacío")]
          [(equal? string-zona "index") (list-ref zonas 1)]
          [(equal? string-zona "local-repository") (list-ref zonas 2)]
          [(equal? string-zona "remote-repository") (list-ref zonas 3)]
          [else "No existe esta zona"]
          )
           ;FALSE CASE
           "Entrada incorrecta\n")))

;---------------------------------------------------------------------------------------------------------------------------

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
        (cond [( null? listaP )
               ;True Case  :
               ;Devuelve el único elemento de la lista y pregunto si puedo seguir avanzando
               ;pregunto si el iterador sumado con uno es menor que el largo total
               (cond [ (< (+ 1 i) n)
                       ;en caso de serlo retorno el workspace con el iterador sumandole uno
                       ;y paso a la siguiente lista particular
                       (recorrer-workspace listaG (list-ref listaG (+ 1 i)) n (+ i 1) (string-append string "Archivo número "
                                     (~v (+ i 2)) "\n\n"   (car (list-ref listaG (+ 1 i))) )) ]
                     ;Else
                     [else (string-append string ) ])]
              ;Else :
              ;en caso de que el largo de la lista particular no se encuentre con un solo elemento
              ;Obtengo el primer elemento de la lista que estoy viendo
              [ else
                ;Ahora hago recursión con lo que queda de la lista
                (recorrer-workspace listaG (cdr listaP) n i (string-append string (car listaP)) ) ]
              )
        )
  )

(define string-workspace
  (lambda ()
    ;pregunto si acaso no estan vacías cada una de las zonas
    (if (list? (get-zona "workspace"))
        (recorrer-workspace (get-zona "workspace") (car (get-zona "workspace")) (longitud (get-zona "workspace") ) 0
                            "WORKSPACE : \n\nArchivo número 1:\n\n")
        #f)))

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
  (lambda (lista lista-nueva primero cola )
    ;Pregunto si la lista es nula 
    (if (null? lista-nueva)
        ;Caso base , cuando ya termine de de recorrer toda la lista
        lista-nueva
        ;Else recursivo
        ;CREO UNA NUEVA LISTA reconstruyendola toda para eso llego hasta el final de la lista
        ;una vez en el final agrego la nueva lista
        (cons (car lista-nueva) (reconstruir-lista lista (cdr lista-nueva)))
        ) ) )
;(provide work-space)
;(provide index )

(provide reconstruir-lista)
(provide zonas)
(provide zonas->string)