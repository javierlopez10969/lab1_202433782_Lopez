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
   ;Representación:  lista de listas donde el primer elemento de cada lista es el nombre de archivo
   ;y el resto su contenido . Cada linea tiene un salto de linea excepto el nombre de archivo
   ;BEGIN of WORKSPACE---------------------------------------------------------------------------------------
   (list
    (list "main.c"
          "\n#include <stdio.h> \n"
          "int main (){ \n"
          "printf('Hola') }\n"
          )
    (list "busqueda.h"
          "\n#include <stdio.h>\n"
          "//funcion que realiza una busqueda\n"))
   ;END of WORKSPACE ---------------------------------------------------------------------------------------------

   ;La lista index contiene como primer paramtro una copia del workspace actual y como segundo parametro una lista
   ;Con cada cambio de los archivos con respecto al ultimo commit
   
   ;BEGIN of index-----------------------------------------------------------------------------------------------
   (list
    ;COPIA DE WORKSPACE ACTUAL
    (list
     (list
      "BEE.c"
      "linea1\n"))
    ;DeltaCambios : lista que contiene los archivos que han sido cambiados
    ;El largo de esta lista indica cuantos archivos fueron cambiados
    (list
     ;Estas listas contienen listas con codigos para saber que fue lo que se hizo en cada archivo
     (list "nombre de archivo" 1 0 (list "archivo.c" "BEE.c") #f #t )))
   ;END of index -----------------------------------------------------------------------------------------------

   ;BEGIN of local-repository--------------------------------------------------------------------------------------
   (list
    ;El list-ref numero 0 muestra el commit mas reciente
    (list "primer commit" ;nombre del commit
          (list 10 35 22 18 5 2020) ;Fecha realizada
          ;Copia del worpksace ;Workspace en ese momento
          (list)
          ;Delta cambios ;Cambios realizados
          (list)))
   ;END of local-repository -----------------------------------------------------------------------------------------------

   ;BEGINof REMOTE-REPOSITORY ---------------------------------------------------------------------------------
   
   (list)
   
   ;END of REMOTE-REPOSITORY ----------------------------------------------------------------------------------
   ))

;SELECTORES zonas

;GET-ZONA , obtengo una zona especifica requerida segun el momento
;DOMINIO : STRING X ZONAS
;RECORRIDO :ZONA || STRING
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

;selector de nombre y fecha de un commit
;Dominio : commit
;salida : nombre x fecha
(define selector-nombre-fecha
  (lambda (commit)
    (define selector
      (lambda(commit listaSalida)
        ;Devuelvo una lista con el nombre y la fecha del commit entregado
        (añadir-elemento (añadir-elemento listaSalida
                         (selectorIndice commit 0)) (selectorIndice commit 1))))
    (selector commit null)))

;Función está en cierta zonas , función que revisa si un commit esta en cierta zona
;Dominio  : commit x zona
;Recorrido : boolean
(define esta?
  (lambda (commit zona)
    ;Si llego al final de la zona devuelvo falso , porque no ha sido encontrado
    (if (null? zona)
        #f
        ;Pregunto si los commits son iguales
        (if (equal? commit (get-primero zona))
            #t
           ;sino sigo buscando
            (esta? commit (get-resto zona))))))

;(esta? (get-primero(selectorIndice zonas 2)) (selectorIndice zonas 2))
;(esta? (get-primero(selectorIndice zonas 2)) (selectorIndice zonas 3))

;PUSH
;Otra función que compara los commits dentro de un local con respecto a los del remote
;Dominio : local-repository y remote-repository
;Salida : Una lista con un entero que indica si las diferencias están en el local o en el remote , 1 local , 2 remote
;Recorrido  : entero x commit x commit
(define commit->remote
  (lambda (local remote zonas)
    ;Función que devuelve una lista con todos los commits que no se encuentren en la otra zona
    (define salida (lambda (zona1 zona2 listaSalida)
                    (if (null? zona1)
                        listaSalida
                        ;Pregunto si acaso mi commit se encuentra en la segunda zona
                        (if (esta? (get-primero zona1) zona2)
                            ;en caso que se encuentre no añado nada a la lista de salida
                            (salida (get-resto zona1) zona2  listaSalida)
                            ;en caso de que no se encuentre añado ese commit a la lista de Salida
                            (salida (get-resto zona1) zona2 (añadir-elemento listaSalida (get-primero zona1)))))))
    (cond
      ;En caso de que las longitudes de local y remote sean iguales
     [(= (longitud local) (longitud remote))
      (cond
        ;Pregunto si hay commit del local en el remote , si no hay ninguno se hace el push
        [(null? (salida local remote null)) "push"]
        ;si hay la misma cantidad que la longitud del local signfica que está actualizado el local
        [(= (longitud (salida local remote null)) (longitud local)) "El local repository se encuentra actualizado"]
        ;si hay por lo menos uno
        [(not (null? (salida local remote null))) ])]
     ;ahora en caso de que la longitud de local sea mayor a la de 
     [(> (longitud local) (longitud remote))
      (cambiar-elemento zonas 3 (añadir-al-inicio (selectorIndice zonas 3) (get-primero(salida local remote null))))]
     [(< (longitud local) (longitud remote))])))




;Función Seleccionadora del workspace de un commit , list-ref 2
;Dominio: Commit
;Recorrido: Workspace
(define get-workspace-of-commit
  (lambda (commit)
    (selectorIndice commit 2)))
;Función seleccionadora del ultimo commit del local-repository
;Dominio zonas
;Recorrido : workspace
(define get-last-workspace
  (lambda (zonas)
    (get-workspace-of-commit(get-primero(get-local-repository zonas)))))
;Ejemplo de uso :(get-last-workspace zonas)


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

;Funciones constructoras----------------------------------------------------------------------------------------------------


;Función que genera una lista con TODOS los nombres de archivos dentro de un workspace
;Dominio : Workspace ; Recorrido : lista
;Recursión : Natural
(define get-namesN
      (lambda (workspace)
        (if (null? workspace)
            null
            (cons
             ;Recursión natural
             (get-primero(get-primero workspace))
             (get-namesN (get-resto workspace))))))
;(get-namesN (get-workspace zonas))
;Recursión : De cola , pero añadir elemento es natural
(define get-namesC
  (lambda(workspace)
    (define get-lista-salida
      (lambda (workspace listaSalida)
        (if (null? workspace)
            listaSalida
            (get-lista-salida (get-resto workspace) (añadir-elemento listaSalida (get-primero(get-primero workspace)))))))
    (get-lista-salida workspace null)))
;(get-names (get-workspace zonas))


;Función constructora delta cambios según el último commit realizado dentro del local repository

;Funcion que genera cambios de una archivo con respecto al ultimo commit
;Entrada : archivo , workspace de el ultimo commit
;Dominio : archivo x workspace
;Recorrido: delta-cambios
(define generar-cambios-dos-archivos
  (lambda (archivo1 workspace)
    ;veo si acaso existe el mismo nombre dentro del commit o un partial match
    "kk"))

;se añaden todos los archivos en que se registran cambios locales
;Función que añade todos los archivos al index 
;Dominio : Lista de Archivos X zonas
;Recorrido = Zonas
(define generar-index
  (lambda (lista zonas)
    (cambiar-elemento zonas 1 )))

;Funcion que busca las coincidencias de en los archivos del workspace dentro del commit mas reciente
;dominio = lista de nombres de archivos X zonas
;Recorrido = zonas
;Funcion que busca archivo por archivo los cambios realizados
(define delta-cambios
  (lambda (lista zonas)
    ;En primer lugar nos aseguramos que los nombres otorgados si se encuentran dentro del workspace
    (if (null? lista)
        ;Si ya me encuentro al final de la lista signfica que si se encontraron todos los archivos dentro del workspace
        ;cambio el index por uno nuevo
        (generar-index lista zonas)
        ;pregunto si se encuentra dentro del workspace
        (if (>= (buscar-archivo (get-workspace zonas) (get-primero lista)) 0)
            ;True Case
            (delta-cambios (get-resto lista) zonas)
            ;Sino se encuentran dentro del workspace
            "Fatal error , los archivos entregados no existen en el workspace"))))

;Creador de commit
(define crear-commit
  (lambda (zonas comentario)
    (list comentario (get-lista-tiempo) (get-primero(get-index zonas)) (get-primero(get-resto(get-index zonas))))))

;Nueva-zona-commit
(define nuevo-commit-local
  (lambda (zonas comentario)    
    (cambiar-elemento 
     ;Creo una nueva zona con el local-repository con un nuevo commit
     (cambiar-elemento zonas 2
    ;añado al inicio el nuevo commit en el local-repository
    (añadir-al-inicio (get-local-repository zonas) (crear-commit zonas comentario)))
    ;Creo una nueva zona con un index en limpio indice 1 , index . nuevo index
    1 (list))))


;Pull arrastra todo el contenido en el ultimo commit de remote-repository y lo actualiza en el workspace y combina commits
;con el local-repository


;---------------------------------------------------------------------------------------------------------------------------

;MODULO zonas->string

;Funcion que funciona de accionar el traspaso de workspace a string
;Dominio : workspace
;Recorrido : String x String
;Tipo de Recursión: recursión de cola
(define string->workspace
  (lambda (workspace)
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
        ;Pregunto acaso si se encuentra vacío o no , en caso de estarlo devuelvo workspace vacío
    (recorrer-workspace workspace
                        (get-resto (get-primero workspace))
                        (longitud workspace)
                        0 ; Indice
                        (string-append "WORKSPACE : \n\nArchivo número 1: "
                                       (get-primero (get-primero workspace))"\n"))))

(define verificar-string-workspace
  (lambda (zonas)
    (if (string? (get-zona "workspace" zonas) )
            ;True case:
            (string-append (get-zona "workspace" zonas) "\n")
            (string->workspace (get-zona "workspace" zonas)))))

;Funcion que traspasa el local-repository a string mostrando solo los nombres de los commit y su fecha de modificación
;Dominio : local-repository | remote-repository X mensaje , el mensaje es si es un local o un remote
;Recorrido : String x String
(define repository->string
  (lambda (repository mensaje)
    (define recorrer-repository
      (lambda (repository stringSalida)
        ;Pregunto acaso si mi puedo seguir recorriendo mi local repository
        (if (null? repository)
            stringSalida
            ;En caso de que aún no nos encontremos al final del local-repository
            (recorrer-repository
             (get-resto repository)
             ;StringSalida
             (string-append
              stringSalida
              "Nombre del Commit: "
              (get-primero (get-primero repository)) "\n"
              (tiempo->string (get-primero(get-resto (get-primero repository))))
              "\n\n")))))
        (recorrer-repository repository mensaje)))

;git log , muestra los ultimos 5 commits realizados
;Dominio : reositorio x string-previo
;Recorrido : String
;Recursión : De cola
(define repository->stringLog
  (lambda (repository mensaje)
    (define recorrer-repository
      (lambda (repository stringSalida indice)
        ;Pregunto acaso si mi puedo seguir recorriendo mi local repository
        (if (or (null? repository) (= 5 indice))
            stringSalida
            ;En caso de que aún no nos encontremos al final del local-repository
            (recorrer-repository
             (get-resto repository)
             ;StringSalida
             (string-append
              stringSalida
              "Nombre del Commit: "
              (get-primero (get-primero repository)) "\n"
              (tiempo->string (get-primero(get-resto (get-primero repository))))
              "\n\n")))))
        (recorrer-repository repository mensaje 0)))

(define verificar-log
  (lambda (zonas)
    (if (string? (get-local-repository zonas))
        ;Si es sting get-local-repository significa que esta vacío
        (string-append (get-local-repository zonas)"\n\n")
        ;Si no lo es
        (repository->stringLog (get-local-repository zonas) "\nLos Ultimos commits realizados fueron : \n\n"))))


(define verificar-local-repository-string
  (lambda (zonas)
    (if (string? (get-local-repository zonas))
        ;Si es sting get-local-repository significa que esta vacío
        (string-append (get-local-repository zonas)"\n\n")
        ;Si no lo es
        (repository->string (get-local-repository zonas) "\nLocal-repository:\n\n"))))

(define verificar-remote-repository-string
  (lambda (zonas)
    (if (string? (get-remote-repository zonas))
        ;Si es sting get-local-repository significa que esta vacío
        (string-append (get-remote-repository zonas)"\n\n")
        ;Si no lo es
        (repository->string (get-remote-repository zonas) "\nRemote-repository:\n\n"))))
;Función que traduce los cambios ocurridos 
;Dominio : delta-cambios x stringSalida
;Recorrido: String
(define translate-delta
  (lambda (deltas stringSalida)
    (define contar-insertions
      (lambda (deltas insertions)
        (if (null? deltas)
            insertions
            (contar-insertions (get-resto deltas) (+ insertions (selectorIndice (get-primero deltas) 1)) ))))
    (define contar-deletions
      (lambda (deltas deletions)
        (if (null? deltas)
            deletions
            (contar-deletions (get-resto deltas) (+ deletions (selectorIndice (get-primero deltas) 2))))))
    (string-append stringSalida
                   (~v (longitud deltas) ) " archivos cambiados , "
                   "Insertions : " (~v (contar-insertions deltas 0) ) " ,"
                   "Deletions : " (~v (contar-deletions deltas 0) ) " .\n")))

;delete , create 
(define index->string
  (lambda (index)
    (translate-delta (get-primero(get-resto index))
    (string-append "INDEX : \nWorkspace actual de index :" (string->workspace (get-primero(get-index zonas))) "\n" ))))

;Funcion que traspasa el index a string mostrando el workspace guardado en ese index + los delta cambios
;Dominio : index
;Recorrido : String x String
(define verificar-index-string
  (lambda (zonas)
    (if (string? (get-index zonas))
        ;Si es sting get-index significa que esta vacío
        (string-append (get-index zonas)"\n\n")
        (index->string (get-index zonas) ))))

;MAIN  zonas->string
;ZONAS TO STRING
;DEVUELVE TODAS LAZ ZONAS EN FORMA DE STRING
;Dominio : Zonas
;Salida :String x String
(define zonas->string
  (lambda (zonas)
    (if (zonas? zonas)
    (string-append (verificar-string-workspace zonas)
                   (verificar-index-string zonas)
                   (verificar-local-repository-string zonas)
                   (verificar-remote-repository-string zonas))
    "No es una zona el parametro entregado")))

;Ejemplo correcto de uso :
;(display (zonas->string zonas))
;push
;(display(zonas->string(comparar-commit (selectorIndice zonas 2) (selectorIndice zonas 3) zonas)))
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
(provide buscar-archivo)
;Pull
(provide selector-nombre-fecha)
;pertenencia
(provide zonas?)
;constructores
(provide get-namesN)
;pull
(provide commit->remote)
;add
(provide generar-index)
(provide delta-cambios)
;commit
(provide nuevo-commit-local)
;zonas->string
(provide zonas->string)
;log
(provide verificar-log)