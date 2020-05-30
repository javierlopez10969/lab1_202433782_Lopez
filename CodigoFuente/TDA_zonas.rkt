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
     (list "BEE.c" 1 0 (list "BEE.c") #f #t )))
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
   ;Registro Hisorico
   (list)
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
               "\nworkspace vacío\n")]
          ;get-index
          [(equal? string-zona "index")
           (if (not (null? (selectorIndice zonas 1)))
               ;true case
               (selectorIndice zonas 1)
               ;else
               "\nindex vacío\n")]
          ;get-local-repositoy
          [(equal? string-zona "local-repository")
           (if (not (null? (selectorIndice zonas 2)))
               ;true case
               (selectorIndice zonas 2)
               ;else
               "\nlocal-repository vacío\n")]
          ;get-remote-repository
          [(equal? string-zona "remote-repository")
           (if (not (null? (selectorIndice zonas 3)))
               ;true case
               (selectorIndice zonas 3)
               ;else
               "\nremote-repository vacío\n")]
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


;SELECTOR BUSCAR nombre de archivo en un workspace
;Dominio : Workspace x Nombre de Archivo
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
    (get-workspace-of-commit(get-primero(selectorIndice zonas 2)))))
;Ejemplo de uso :(get-last-workspace zonas)

;Función que revisa si un commit esta en cierta zona
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
;Ejemplos de uso :
;(esta? (get-primero(selectorIndice zonas 2)) (selectorIndice zonas 2))
;(esta? (get-primero(selectorIndice zonas 2)) (selectorIndice zonas 3))

;Función que devuelve una lista con todos los commits que no se encuentren en la otra zona
;Dominio zona1 x zona2 x null
;Recorrido : commit x commit
;Recursión : de cola
(define estan? (lambda (zona1 zona2 listaSalida)
                 (if (null? zona1)
                     ;Devuelvo los commits desde el más viejo al más reciente
                     (reverse listaSalida)
                     ;Pregunto si acaso mi commit se encuentra en la segunda zona
                     (if (esta? (get-primero zona1) zona2)
                         ;en caso que se encuentre no añado nada a la lista de salida
                         (estan? (get-resto zona1) zona2  listaSalida)
                         ;en caso de que no se encuentre añado ese commit a la lista de Salida
                         (estan? (get-resto zona1) zona2 (añadir-elemento listaSalida (get-primero zona1)))))))

;PUSH-----------------------------------------------------------------------------------------------------------------------
;Otra función que compara los commits dentro de un local con respecto a los del remote
;Dominio : local-repository y remote-repository
;Salida : Una lista con un entero que indica si las diferencias están en el local o en el remote , 1 local , 2 remote
;Recorrido  : Zonas | string
;Recursión : De cola , ya que no queremos hacer un mal uso de memoria , además que cambiar elemento , de por si , ya usa
;una recursión natural
(define commit->remote
  (lambda (local remote zonas)
    (define actualizar-remote
      (lambda (commits zonas)
        (if (null? commits)
            zonas
            (actualizar-remote
             (get-resto commits)
             ;Añado al incio el commit
             (cambiar-elemento zonas 3 (añadir-al-inicio (selectorIndice zonas 3) (get-primero commits)))))))
    (cond
      ;estan? los commits del local  en remote
      [(not (null? (estan? local remote null)))
       (actualizar-remote (estan? local remote null) zonas )]
      ;no estan? los commits de local en remote
      [(null? (estan? local remote null))
       "El remote repository se encuentra actualizado , no se procederá a hacer push"])))
;---------------------------------------------------------------------------------------------------------------------------

;PULL--------------------------------------------------------------------------------------------------------------------
;Función que actualiza el workspace según el último workspace
;Dominio : Zonas X Ultimo-worskpace X Workspace
;Recorrido : Zonas
;Recursión de cola
(define actualizar-workspace
  (lambda (zonas last-workspace workspace)
    (cond
      ;pregunto si mi workspace esta vacío
      [(not(string? (get-workspace zonas)))
       ;Acá actualizo mi workspace con los cambios en el último workspace
       (if (null? last-workspace)
           zonas
           (actualizar-workspace
            ;Añado los archivos del ultimo workspace
            ;Pregunto si acaso mi archivo en el workspace se encuentra en el workspace actual
            (if (>= (buscar-archivo workspace (get-primero last-workspace)) 0) 
                ;Si se encuentra lo actualizo
                (cambiar-elemento zonas 0 (cambiar-elemento (selectorIndice zonas 0)
                                                            (buscar-archivo workspace (get-primero last-workspace))
                                                            (get-primero last-workspace)))
                ;Si no se encuentra lo añado al inicio del workspace
                (cambiar-elemento zonas 0 (añadir-al-inicio (selectorIndice zonas 0)
                                                            (get-primero last-workspace))))
            
            (get-resto last-workspace) workspace))]
      [(string? (get-workspace zonas))
       (cambiar-elemento zonas 0 (get-last-workspace zonas))])))

;Función que mueve los commits del remote repository al local-repository
;Dominio : local-repository , remote-repository ,zonas
;Recorrido : Zonas || string
;Recursión : De cola ,porque asi lo solicita el enunciado
(define commit->local
  (lambda (local remote zonas)
    ;Función que actualiza local repository junto al con los commits de remote-repository
    ;Dominio : commits x zonas
    ;Recorrido : zonas
    ;Actualizar local de una manera muy básica solo pensando que el local esta vacío
    (define actualizar-local
      (lambda (commits zonas)
        (if (null? commits)
            ;Aquí actualizo el workspace , porque ya agrege todos los commits
            (actualizar-workspace zonas (get-last-workspace zonas) (selectorIndice zonas 0))
            ;Hago la llamada recursiva de cola 
            (actualizar-local
             (get-resto commits)
             ;Actualizo el local-repository con los commits que no se encuentren en el local
             (cambiar-elemento zonas 2 (añadir-al-inicio (selectorIndice zonas 2) (get-primero commits)))))))
    (cond
      ;Acá pregunto si están actualizados los commits del remote en el local-repository
      [(not (null? (estan? remote local null)))
       (actualizar-local (estan? remote local null) zonas )]
      ;En caso de que el local se encuentre actualizado , se lo informo al usuario
      [(null? (estan? remote local null))
       "El local repository se encuentra actualizado , no se procederá a hacer pull"])))
;---------------------------------------------------------------------------------------------------------------------------

;FUNCIONES DE PERETENECIA
;Función zona? : verifica si acaso el parametro entregado pertenece a un elemento zona
;Dominio : Cualquier clase de elemento
;Recorrido : boolean
(define (zonas? funcion)
  (and
   (list? funcion)
   (= 5 (longitud funcion))
   (list? (selectorIndice funcion 0))
   (list? (selectorIndice funcion 1))
   (list? (selectorIndice funcion 2))
   (list? (selectorIndice funcion 3))))

;Funciones constructoras----------------------------------------------------------------------------------------------------


;Commit------------------------------------------------------------------------------------------------------------------
;Creador de commit
;Dominio : Zonas x Comentario
;Recorrido:Commit
(define crear-commit
  (lambda (zonas comentario)
    ;                                         Guardar Workspace        Guardar cambios
    (list comentario (get-lista-tiempo) (get-primero(get-index zonas)) (get-primero(get-resto(get-index zonas))))))

;Nueva-zona-commit , función que recibe un comentario para luego el ultimo index efectuado se lleve a un commit
;Dominio : Zonas X Comentario
;Recorrido  : Zonas
(define nuevo-commit-local
  (lambda (zonas comentario)    
    (cambiar-elemento 
     ;Creo una nueva zona con el local-repository con un nuevo commit
     (cambiar-elemento zonas 2
    ;añado al inicio el nuevo commit en el local-repository
    (añadir-al-inicio (selectorIndice zonas 2) (crear-commit zonas comentario)))
    ;Creo una nueva zona con un index en limpio indice 1 , index . nuevo index
    1 (list))))
;Commit---------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------------


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
(provide commit->local)
;pertenencia
(provide zonas?)
;push
(provide commit->remote)
;add
(provide get-last-workspace)
;commit
(provide nuevo-commit-local)