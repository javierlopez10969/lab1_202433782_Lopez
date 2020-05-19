#lang racket
;FUNCIONES ANEXAS
(define listaTiempo
  (lambda () (list
    (date-second (seconds->date (current-seconds)))
    (date-minute (seconds->date (current-seconds)))
    (date-hour (seconds->date (current-seconds)))
    (date-day (seconds->date (current-seconds)))
    (date-month (seconds->date (current-seconds)))
    (date-year (seconds->date (current-seconds)))
    )
   )
  )

;FUNCION : calcular longitud o length , su implmentación es solo para saber como funcionaba 
;Dominio: lista x lista
;Recorrido : Entero x Entero
;Recursion Natural  , ya que deja espacios pendientes
(define longitud
   (lambda (lista)
     ;Pregunto si acaso el elemento entregado acaso es una lista
     (if (list? lista)
         ;Y la recorro recurisvamente
         ( cond
            ;Hasta que llege a ser nulo
            [( null? lista) 0]
            ;Si no es nulo añado uno más a la cuenta del longitud y sigo buscando el ultimo elemento (null) de la lista
            [ else (+ 1 (longitud (cdr lista) ) ) ] )
         #f) ) )

;Función que cambia elemento dentro de una lista
;DOMINIO : LISTA x ENTERO x ELEMENTO
;RECORRIDO : LISTA
;RECURSIÓN : NATURAL
(define cambiar-elemento
  (lambda (lista posicion nuevo-elemento)
  ;Pregunto si la lista es nula 
       ;pregunto si ya llege a mi posición deseada
       ;en caso de serlo devuelvo el elemento
       (if (zero? posicion) nuevo-elemento (car lista))
       ;En caso contr
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

(provide listaTiempo)
(provide longitud)