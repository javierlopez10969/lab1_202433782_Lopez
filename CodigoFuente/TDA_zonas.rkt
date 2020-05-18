#lang racket
;----------------------------------------------------------------------------------------------------------------
;Funcion Auxiliar
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
;----------------------------------------------------------------------------------------------------------------


;---------------------------------------------------------------------------------------------------------------------------
;TDA ZONAS
;Función : ZONAS , FUNCION CONSTANTES , consta de 4 zonas (4 listas) que se obtienen por su ref
; 0 : WORKSPACE ; 1 :INDEX ; 2 : LOCAL-REPOSITORY ; 3 :
(define zonas
  (list
   ;WORKSPACE ref 0 , car , first
   ;Representación: tiene una lista con los archivos
   ;representados con una lista de listas donde el primer elemento de cada lsita
   ;es el nombre y el resto sus lineas de código
   ;BEGIN of WORKSPACE---------------------------------------------------------------------------------------
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
   ;END of WORKSPACE ---------------------------------------------------------------------------------------------
   
   ;BEGIN of INDEX -----------------------------------------------------------------------------------------------
   
   (list
    ;La lista index contiene en primer lugar todos los archivos dados de una carpeta
    ;La primera lista es una copia del workspace actual , con las respectivos cambios hechos en los archivos

    ;COPIA DE WORKSPACE ACTUAL---------------------------------------------------------------------------------
    (list
     (list
      "nombre de archivo\n"
      "linea1\n")
     )
     ;---------------------------------------------------------------------------------------------------------

     ;DELTA CAMBIOS----------------------------------------------------------------------------------------------
    
     ;Lista que contiene los archivos que han sido cambiados
     ;FILES CHANGED , lista de elementos que representan para saber que fue lo que se hizo en cada archivo
     ;El largo de esta lista indica cuantos archivos fueron cambiados
     ; INSERTIONS   | DELETIONS |  RENAMES   | DELETE mode | CREATEMODE |
     ; entero con   |entero con |boleano que |boleano que  |boleano que |
     ; la cantidad  |la cantidad|indica si se|indica si se |indica si se|  
     ; de           |de lineas  |cambio el   |borro el     |creo el     |
     ; inserciones  |borradas   |nombre o no |archivo o no |archivo o no|
     (list
      (list "nombre de archivo" 1 0 #f #f #t )
      
      ;DELTA CAMBIOS----------------------------------------------------------------------------------------------
      )
     )
   
   ;END of INDEX -----------------------------------------------------------------------------------------------

   ;BEGIN of LOCAL-REPOSITORY--------------------------------------------------------------------------------------
   
   (list)

   ;END of INDEX -----------------------------------------------------------------------------------------------

   ;BEGINof REMOTE-REPOSITORY -----------------------------------------------------------------------------------------------
   
   (list)
   
   ;END of REMOTE-REPOSITORY -----------------------------------------------------------------------------------------------
   
   )
  )
;DE PERETENECIA
;Función zona? : verifica si acaso el parametro entregado pertenece a un elemento zona
;Dominio : Elemento
;Salida : bool
(define zonas?
  (lambda (funcion)
    ;Para que sea zona , el elemento debe ser una lista de la largo 4 donde cada elemento dentro de el
    ;Tambien sea un lista
    (if (and (list? funcion) (= 4 (length funcion)))
        ;True Case ahora que se que es una lista hay que verificar si cada elemento dentro corresponde a una lista
        (if (and (list?(car funcion)) (list? (car(cdr funcion))) (list? (car (cdr (cdr funcion))))
                 (list?(car(car(car(cdr funcion)))) ))
            #t
            #f)
        ;Else
        #f)))

;SELECTOR
;GET-ZONA , obtengo una zona especifica requerida segun el momento
;DOMINIO : STRING X ZONAS
;RECORRIDO :LISTA X LISTAS || STRING
;RECURSION
(define get-zona
  (lambda (string-zona zonas)
    ;Primero de aseguro que las entrada sea un string
    ;junto a unas zonas
    (if (and (string? string-zona) (zonas? zonas))
        ;TRUE CASE
        (cond
          [(equal? string-zona "workspace")
           (if (not (null? (car zonas)))
               ;true case
               (car zonas)
               ;else
               "Workspace vacío")]
          [(equal? string-zona "index")
           (if (not (null? (car (cdr zonas))))
               ;true case
               (car(cdr zonas))
               ;else
               "Index vacío")]
          [(equal? string-zona "local-repository") (list-ref zonas 2)]
          [(equal? string-zona "remote-repository") (list-ref zonas 3)]
          [else "No existe esta zona"]
          )
           ;FALSE CASE
           "Entrada incorrecta\n")))
;SELECTOR BUSCAR ARCHIVO en un workspace


;---------------------------------------------------------------------------------------------------------------------------

;FUNCIONES NECESARIAS

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
                       (recorrer-workspace listaG (cdr(list-ref listaG (+ 1 i))) n (+ i 1)
                                           (string-append string "\nArchivo número "
                                                          (~v (+ i 2)) ": " (car (list-ref listaG (+ 1 i))) "\n" )) ]
                     ;Else
                     [else (string-append string ) ])]
              ;Else :
              ;en caso de que el largo de la lista particular no se encuentre con un solo elemento
              ;Obtengo el primer elemento de la lista que estoy viendo
              [ else
                ;Ahora hago recursión con lo que queda de la lista
                (recorrer-workspace listaG (cdr listaP) n i (string-append string (car listaP)))]
              )
        )
  )
                                                                                                                                              
;Funcion que funciona de accionar el traspaso de workspace a string
;Dominio : Funcion para ver si verificada si corresponde a una zonas
;Recorrido : String x String
(define string-workspace
  (lambda (funcion)
    ;pregunto si corresponde a una zona
    (if (zonas? funcion)
        (recorrer-workspace (get-zona "workspace" funcion) (cdr(car (get-zona "workspace" funcion)))
                            (longitud (get-zona "workspace" funcion)) 0
                            (string-append "WORKSPACE : \n\nArchivo número 1: "
                            (car (car (get-zona "workspace" funcion)))"\n"))
        "No corresponde a una zona el paramtero entregado")))


;Funcion que sirve de accionar el traspaso de de local-repository a string
;Dominio : Funcion para ver si verificada si corresponde a una zonas
;Recorrido : String x String
(define string-local-repository
  (lambda ()
    (if (list? ("local-repository"))
        #t
        #f) ) )


;ZONAS TO STRING
;DEVUELVE TODAS LAZ ZONAS EN FORMA DE STRING
;Dominio : Funciones
;Salida :StringxString
(define zonas->string
  (lambda (funcion)
    (string-workspace funcion)
    
    )
  )

;(provide reconstruir-lista)
(provide zonas)
(provide zonas?)
(provide zonas->string)