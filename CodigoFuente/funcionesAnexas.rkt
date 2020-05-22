#lang racket
;FUNCIONES ANEXAS
;Get primero = sinonimo de car
(define get-primero car)
;Get resto = sinonimo de cdr
(define get-resto cdr)
;Selector de Zonas segun indice sinonimo de list-ref
(define selectorIndice list-ref)

;Función la cual crea en ese instante una lista con losdatos de la fecha de ese instante
(define get-lista-tiempo
  (lambda ()
    (list
     (date-second (seconds->date (current-seconds)))
     (date-minute (seconds->date (current-seconds)))
     (date-hour (seconds->date (current-seconds)))
     (date-day (seconds->date (current-seconds)))
     (date-month (seconds->date (current-seconds)))
     (date-year (seconds->date (current-seconds))))))
;(define lista (get-lista-tiempo))
;Funcion traductora a string para que se más comprensible para el ususario
;Dominio: Lista
(define tiempo->string
  (lambda (listaTiempo)
    (string-append (~v (selectorIndice listaTiempo 0))":"
                   (~v (selectorIndice listaTiempo 1))":"
                   (~v (selectorIndice listaTiempo 2))" "
                   (~v (selectorIndice listaTiempo 3))"/"
                   (~v (selectorIndice listaTiempo 4))"/"
                   (~v (selectorIndice listaTiempo 5))"\n")))
;(display(tiempo->string lista))


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


(provide selectorIndice)
(provide get-primero)
(provide get-resto)
(provide tiempo->string)
(provide get-lista-tiempo)
(provide longitud)