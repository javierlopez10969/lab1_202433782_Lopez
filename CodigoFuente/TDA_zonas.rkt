;Modulo de importaciones
#lang racket
(require "funcionesAnexas.rkt")

;---------------------------------------------------------------------------------------------------------------------------
;TDA ZONAS
;Función : ZONAS , FUNCION CONSTANTES , consta de 4 zonas (4 listas) que se obtienen por su indice

;ZONAS : Zonas X Workspace X Index X Local-repository X Remote-repository
;Indice 0
;workspace = directorio X archivo
;Directorio = null | archivo x archivo
;archivo = nombre-archivo x contenido

;Indice 1
;index = workspace x delta-cambios
;delta cambios = insertions X deletions X  rename X delete-mode X create-mode
;insertions = integer ; deletions = integer ; rename string x string ; delete.mode = boolean ; create-mode = boolean

;Indice 2
;remote-repository = commit x commit
;commit = nombre-commit x lista-tiempo x workspace x delta-cambios
;nombre-commit = string
;lista-tiempo = segundos x minutos x horas x dia x mes x año

;Indice 3
;remote-repository = commit x commit

; Funcion constante zonas
(define zonas
  (list
   ;WORKSPACE ref 0 , car , first
   ;Representación: tiene una lista con los archivos
   ;representados con una lista de listas donde el primer elemento de cada lsita
   ;es el nombre y el resto sus lineas de código
   ;BEGIN of WORKSPACE---------------------------------------------------------------------------------------
   (list
    (list "main.c"
          "\n#include <stdio.h> \n"
          "int main (){ \n"
          "printf('Hola') }\n"
          )
    (list "busqueda.h"
          "\n#include <stdio.h>\n"
          "//funcion que realiza una busqueda\n")
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
    ;DELTA CAMBIOS QUE SE COMPARAN
    ;Lista que contiene los archivos que han sido cambiados
    ;FILES CHANGED , con codigos para saber que fue lo que se hizo en cada archivo
    ;El largo de esta lista indica cuantos archivos fueron cambiados
    (list
     (list "nombre de archivo" 1 0 (list "archivo.c" "nombre archivo") #f #t ))
      
      ;DELTA CAMBIOS----------------------------------------------------------------------------------------------
      
     )
   ;END of INDEX -----------------------------------------------------------------------------------------------

   ;BEGIN of LOCAL-REPOSITORY--------------------------------------------------------------------------------------
   
   (list
    ;El list-ref numero 0 muestra el commit mas reciente
    (list "primer commit" ;nombre del commit
          (list 10 35 22 18 5 2020) ;Fecha realizada
          ;Copia del worpksace ;Workspace en ese momento
          (list)
          ;Delta cambios ;Cambios realizados
          )
    )

   ;END of INDEX -----------------------------------------------------------------------------------------------

   ;BEGINof REMOTE-REPOSITORY ---------------------------------------------------------------------------------
   
   (list)
   
   ;END of REMOTE-REPOSITORY ----------------------------------------------------------------------------------
   
   )
  )

;SELECTORES zonas

;GET-ZONA , obtengo una zona especifica requerida segun el momento
;DOMINIO : STRING X ZONAS
;RECORRIDO :ZONAS|| STRING
(define get-zona
  (lambda (string-zona zonas)
    ;Primero de aseguro que las entrada sea un string
    (if (and (string? string-zona))
        ;TRUE CASE
        (cond
          ;get-workspace
          [(equal? string-zona "workspace")
           (if (not (null? (selectorIndice zonas 0) ))
               ;true case
               (selectorIndice zonas 0)
               ;else
               "workspace vacío\n")]
          ;get-index
          [(equal? string-zona "index")
           (if (not (null? (selectorIndice zonas 1)))
               ;true case
               (selectorIndice zonas 1)
               ;else
               "index vacío\n")]
          ;get-local-repositoy
          [(equal? string-zona "local-repository")
           (if (not (null? (selectorIndice zonas 2)))
               ;true case
               (selectorIndice zonas 2)
               ;else
               "local-repository vacío\n")]
          ;get-remote-repository
          [(equal? string-zona "remote-repository")
           (if (not (null? (selectorIndice zonas 3)))
               ;true case
               (selectorIndice zonas 3)
               ;else
               "remote-repository vacío\n")]
          [else "No existe esta zona"]
          )
           ;FALSE CASE
           "Entrada incorrecta\n")))
;sinonimos de funciones de indice
(define (get-workspace zonas)
    (get-zona "workspace" zonas))
(define (get-index zonas)
    (get-zona "index" zonas))
(define (get-local-repository  zonas)
    (get-zona "local-repository" zonas))
(define (get-remote-repository zonas)
    (get-zona "remote-repository" zonas))

;FUNCIONES DE PERETENECIA
;Función zona? : verifica si acaso el parametro entregado pertenece a un elemento zona
;Dominio : Cualquier clase de elemento
;Recorrido : boolean
(define (zonas? funcion)
  (and
   (list? funcion)
   (= 4 (longitud funcion))
   (list? (selectorIndice funcion 0))
   (list? (selectorIndice funcion 1))
   (list? (selectorIndice funcion 2))
   (list? (selectorIndice funcion 3))))



;SELECTOR BUSCAR ARCHIVO en un workspace
;Dominio : Workspace x String
;Recorrido : Integer
;Recursión de cola
;Funcion Envoltorio
(define buscar-archivo
      (lambda (workspace archivo-buscado)
        ;Funcion Inside
        (define buscador(lambda (workspace archivo n i string)
        ;Casos borde
        (cond
          ;Pregunto si acaso el nombre del archivo es igual al nombre que busco
          [(equal? (get-primero archivo) string) i]
          ;De caso contrario pregunto si puedo seguir buscando
          [(< (+ 1 i) n) (buscador workspace (selectorIndice workspace (+ 1 i)) n (+ 1 i) string) ]
          ;De caso que no pueda seguir buscando retorno un indice -1
          [else -1])))
        ;Activo la función de adentro
        (buscador workspace (get-primero workspace) (longitud workspace) 0 archivo-buscado)))
;Ejemplo de uso : (buscar-archivo (get-workspace zonas) "busqueda.h")

;Funcion accionar buscador
;---------------------------------------------------------------------------------------------------------------------------

;MODULO zonas->string

;Funcion que funciona de accionar el traspaso de workspace a string
;Dominio : workspace
;Recorrido : String x String
;Tipo de Recursión: recursión de cola
(define string-workspace
  (lambda (funcion)
    (define recorrer-workspace
      (lambda (workspace archivo n i stringSalida)
        ;Casos borde
        ;Caso de Lista Particular pregunto si acaso queda un único elemento en la lista que estoy analizando
        (cond [( null? archivo )
               ;True Case  :
               ;Devuelve el único elemento de la lista y pregunto si puedo seguir avanzando
               ;pregunto si el iterador sumado con uno es menor que el largo total
               (cond [ (< (+ 1 i) n)
                       ;en caso de serlo retorno el workspace con el iterador sumandole uno
                       ;y paso a la siguiente lista particular
                       (recorrer-workspace workspace (get-resto(list-ref workspace (+ 1 i))) n (+ i 1)
                                           (string-append stringSalida "\nArchivo número "
                                                          (~v (+ i 2)) ": "
                                                          (get-primero (list-ref workspace (+ 1 i))) "\n" )) ]
                     ;Else
                     [else (string-append stringSalida)])]
              ;Else :
              ;en caso de que el largo de la lista particular no se encuentre con un solo elemento
              ;Obtengo el primer elemento de la lista que estoy viendo
              [ else
                ;Ahora hago recursión con lo que queda de la lista
                (recorrer-workspace workspace (get-resto archivo) n i (string-append stringSalida (get-primero archivo)))])))
    ;pregunto si corresponde a una zona
    (if (zonas? funcion)
        ;Pregunto acaso si se encuentra vacío o no , en caso de estarlo devuelvo workspace vacío
        (if (string? (get-zona "workspace" funcion) )
            ;True case:
            (get-zona "workspace" funcion)
            (recorrer-workspace (get-zona "workspace" funcion)
                                (get-resto (get-primero (get-zona "workspace" funcion)))
                                (longitud (get-zona "workspace" funcion))
                                0 ; Indice
                                (string-append "WORKSPACE : \n\nArchivo número 1: "
                                               (car (car (get-zona "workspace" funcion)))"\n")))
        "No corresponde a una zona el paramtero entregado")))

;Funcion que traspasa el local-repository a string mostrando solo los nombres de los commit y su fecha de modificación
;Como git log
;Dominio : Zonas
;Recorrido : String x String
(define string-local-repository
  (lambda (zonas)
    (define recorrer-local-repository
      (lambda (local-repository stringSalida)
        ;Pregunto acaso si mi puedo seguir recorriendo mi local repository
        (if (null? local-repository)
            stringSalida
            ;En caso de que aún no nos encontremos al final del local-repository
            (recorrer-local-repository
             (get-resto local-repository)
             ;StringSalida
             (string-append
              stringSalida
              "Commit: "
              (get-primero (get-primero local-repository)) "\n"
              (tiempo->string (get-primero(get-resto (get-primero local-repository))))"\n")))))
    (if (string? (get-local-repository zonas))
        ;Si es sting get-local-repository significa que esta vacío
        (get-local-repository)
        ;Si no lo es
        (recorrer-local-repository
         (get-local-repository zonas)
         "\nLocal-repository:\n\n")
        )))
;Funcion que traspasa el index a string mostrando solo los nombres de los commit y su fecha de modificación
;Como git log
;Dominio : Zonas
;Recorrido : String x String

;ZONAS TO STRING
;DEVUELVE TODAS LAZ ZONAS EN FORMA DE STRING
;Dominio : Zonas
;Salida :String x String
(define zonas->string
  (lambda (zonas)
    (string-append (string-workspace zonas) (string-local-repository zonas))))
;------------------------------------------------------------------------------------------------------
;(provide reconstruir-lista)
;(buscar "busqueda.c" (car zonas))

;MODULO PROVIDE
(provide zonas)
;Selectores
(provide get-zona)
(provide get-workspace)
(provide get-index)
(provide get-local-repository)
(provide get-remote-repository)
(provide selectorIndice)
;pertenencia
(provide zonas?)
;zonas->string
(provide zonas->string)