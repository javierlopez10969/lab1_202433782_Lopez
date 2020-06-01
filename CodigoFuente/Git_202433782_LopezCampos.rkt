;Modulo de importaciones
#lang racket
; Zona de modulación.
(require "zonasEjemplos.rkt")
(require "add.rkt")
(require "funcionesAnexas.rkt")
(require "TDA_zonas.rkt")(require "add.rkt")
(require "zonastostring.rkt")

;Funcón que se activa cuando ocurre una invadilación de tipos
(define invalidacion
  (lambda ()
  (display "Invalid terms")))
;commit---------------------------------------------------------------------------------------------------------------------
;Función : Commit que añade los commit del index al local repository repository
;Dominio : string x zonas
;Recorrido: lista de nombre s de archivos
;Tipo de Recursión: recursion natural o de cola , siendo disntinto
;al de pull
(define commit
  (lambda (string)
    (if (string? string)
        ;True case
        (lambda (zonas)
          (if (zonas? zonas)
              ;true case
              (if (not (null? (get-index zonas)))
                  (nuevo-commit-local zonas string)
                  ;else
                  "No se han registrado cambios en el index")
              ;else
              "no corresponde a una zona")
          ;Movemos los commit de index a Local-Repository
          )
        ;Else
        "No es un string el comentario entregado") ) )
;(((git commit)"mi commit")zonas)
;(display(zonas->string (((git commit)"mi commit")zonas)))
;(display(zonas->string ((git commit)2)zonas))
;-----------------------------------------------------------------------------------------------------------------------------
;Función : push
;Dominio : zonas
;Recorrido: lista de nombre s de archivos
;Tipo de Recursión: recursion natural o de cola , siendo disntinto al de pull
(define push
  (lambda (zonas)
    (if (zonas? zonas)
        ;True Case
        (commit->remote (selectorIndice zonas 2) (selectorIndice zonas 3) zonas)
        ;else
        (display "No ha ingresado el parametro correcto\n"))))
;(display(zonas->string ((git push)(((git commit)"my commit")((git push)zonas)))))
;((git push)(((git commit)"my commit")((git push)zonas)))
;Ejemplo de uso
;(display(zonas->string((git push)zonas)))
;((git push)(((git commit)"my commit")zonas))
;((git push)((git push)(((git commit)"my commit")zonas)))
;-----------------------------------------------------------------------------------------------------------------------------
;Función pull que recibe zonas y mueve de remote a local repository     
;Dominio : zonas
;Recorrido: zonas
;Tipo de Recursión: recursion natural o de cola , siendo disntinto al de add
; Ej (pull zonas)
; ((git pull)zonas)
(define pull
  (lambda (zonas)
    (if (zonas? zonas)
        (commit->local (selectorIndice zonas 2) (selectorIndice zonas 3) zonas)
        ;else
        #f)))
;-----------------------------------------------------------------------------------------------------------------------------
;Función log , que muestra por pantalla un string con los últimos commits realizados en el local-repository actual
;Dominio : zonas
;Recorrido : string
(define log
  (lambda (zonas)
    (verificar-log zonas)))
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
              "Date : " (tiempo->string (get-primero(get-resto (get-primero repository))))
              "\n\n        "
              (get-primero (get-primero repository)) "\n\n")
             (+ indice 1)))))
        (recorrer-repository repository mensaje 0)))

(define verificar-log
  (lambda (zonas)
    (if (string? (get-local-repository zonas))
        ;Si es sting get-local-repository significa que esta vacío
        (string-append (get-local-repository zonas)"\n\n")
        ;Si no lo es
        (repository->stringLog (get-local-repository zonas) "\nLos Ultimos commits realizados fueron : \n\n"))))
;(display((git log)((git push)(((git commit)"my commit")zonas))))
;LOG-----------------------------------------------------------------------------------------------------------------------
;STATUS---------------------------------------------------------------------------------------------------------------------
(define archivos-index->string
  (lambda (index)
    (define delta-cambios->string
      (lambda (stringSalida delta-cambios)
        (if (null? delta-cambios)
            (string-append stringSalida "\n")
            (delta-cambios->string (string-append stringSalida (get-primero(get-primero delta-cambios)) "\n")
                                   (get-resto delta-cambios) ))))
    (delta-cambios->string "\n" (get-primero(get-resto index)))))
;Función status , la cuál retorna un string con :
; - Archivos agregados al index
; - la cantidad de commits en el local-repository
; -La rama actual en la que se encuentra el Local Repository (predeterminado: “master”  )
;Dominio : Zonas
;Recorrido : String
(define status
  (lambda (zonas)
    (string-append
     ;Archivos en el index , pregunto si acaso esta vacío el index , si esta devuelvo un mensaje , si no esta vacío 
     "Archivos en el index: \n" (if (string? (get-index zonas)) (get-index zonas) (archivos-index->string (get-index zonas)))
     "Cantidad de commits en el local-repository: " (~v (longitud (selectorIndice zonas 2))) "\n"
     "La rama actual donde se encuentra el local-repository : master \n\n")))
;STATUS---------------------------------------------------------------------------------------------------------------------
;Función que muestra registro historico
;
(define mostrar-registro
  (lambda (zonas)
    (define recorrer-registro
      (lambda (registro stringSalida )
        (if (null? registro)
            stringSalida
            (recorrer-registro
             (get-resto registro)
             (string-append stringSalida (get-primero registro) "\n")))))
    (recorrer-registro (selectorIndice zonas 4) "EL registro de comandos efectuados en esta zona fue:\n" )))
;Función git que recibe comandos y devuelve estos mismos aplicados a las zonas correspondientes
;Dominio : Funcion X ZONAS
;Recorrido: Zonas || String
(define git
  (lambda (comando)
    (lambda (zonas)
      (cond
        [(equal? comando pull)
         (pull (cambiar-elemento zonas 4 (añadir-al-inicio (selectorIndice zonas 4) "pull")))]
        [(equal? comando push)
         (push (cambiar-elemento zonas 4 (añadir-al-inicio (selectorIndice zonas 4) "push")))]
        [(equal? comando add)
         (lambda (real-zonas)
           ((add zonas)(cambiar-elemento real-zonas 4 (añadir-al-inicio (selectorIndice real-zonas 4) "add"))))]
        [(equal? comando commit)
         (lambda (real-zonas)
         ((commit zonas)(cambiar-elemento real-zonas 4 (añadir-al-inicio (selectorIndice real-zonas 4) "commit"))))]
        [(equal? comando log)
         (log zonas)]
        [(equal? comando status)
         (status zonas)]
        ;else
        [else (invalidacion)]))))

;EJEMPLOS DE USO-------------------------------------------------------------------------------------------------------------
;EJEMPLO BASE
;Ejemplos de add
;1.-Ejemplo de Add
;Zonas inicialmente sin ningún cambio

;(display(zonas->string zonas))

;Add

;(((git add)(list "main.c"))zonas)
;(display(zonas->string (((git add)(list "main.c"))zonas) ))

;commit

;(zonas->string  (((git commit)"New workspace")(((git add)null)zonas)))

;Push

;((git push) (((git commit)"New workspace")(((git add)null)zonas)))

;Otro ejemplo de add

;(((git commit)"my commit")zonas)
;(display(zonas->string (((git commit)"my commit")zonas)))

;((git push)(((git commit)"my commit")zonas))
;(display(zonas->string ((git push)(((git commit)"my commit")zonas))))

;mostrar registro historico
;(display(mostrar-registro ((git push)(((git commit)"my commit")zonas))))

;Otros ejemplos
;Add
;(((git add)null)(((git commit)"my commit")zonas))
;(display(zonas->string (((git add)null)(((git commit)"my commit")zonas))))

;((git push)(((git commit)"New workspace")(((git add)null)(((git commit)"my commit")zonas))))
;(display (zonas->string ((git push)(((git commit)"New workspace")(((git add)null)(((git commit)"my commit")zonas))))))
;(display(mostrar-registro ((git push)(((git commit)"New workspace")(((git add)null)(((git commit)"my commit")zonas)))))

;LOG

;(display((git log)((git push)(((git commit)"New workspace")(((git add)null)(((git commit)"my commit")zonas))))))

;STATUS

;(display((git status)zonas))
;(display((git status)(((git add)null)zonas)))
;(display((git status)(((git add)null)(((git commit)"my commit")zonas))))
;-------------------------------------------------------------------------------------------------------------------------
;Ejemplo PULL PULL : Trae todo lo del remote repository al local y al workspace
;Zonas 1 inicialmente

;zonas1
;(display (zonas->string zonas1))

;Zonas 1 Despues de aplicarle el pull

;(display "PULL APLICADO\n\n\n")
;((git pull)zonas1)
;(display (zonas->string ((git pull)zonas1)))

;Si le intento hacer un add será rechazado ya que no hay cambios dentro del workspace

;(((git add)null)((git pull)zonas1))
;(((git add)(list "main.c"))((git pull)zonas1))

;STATUS

;(display((git status)((git pull)zonas1)))

;LOG
;(display((git log)((git pull)zonas1)))

;------------------------------------------------------------------------------------------------------------------------
;Zonas 2 antes que haga cualquier cambio

;(display(zonas->string zonas2))

;add Función a la cual se realiza un add del pdf actual

;(display(zonas->string(((git add) (list "como salvar el semestre online.pdf"))zonas2)))

;commit -add

;(((git commit)"Mi PDF") (((git add) (list "como salvar el semestre online.pdf"))zonas2))
;(display(zonas->string(((git commit)"Mi PDF") (((git add) (list "como salvar el semestre online.pdf"))zonas2))))

;Registro : pull-commit-add

;(display(mostrar-registro (((git commit)"Mi PDF") (((git add) (list "como salvar el semestre online.pdf"))zonas2))))

;commit sin cambios en index
;(((git add)null)((git pull)zonas2))
;(display(zonas->string(((git add)null)((git pull)zonas2))))


;(display(zonas->string (((git add)null)((git pull)zonas2))))

;((git pull)zonas2)
;(display(zonas->string((git pull)zonas2)))
;STATUS:
;(display((git status)(((git add)null)zonas2)))
;(display((git status)(((git add)null)((git pull)zonas2))))
;---------------------------------------------------------------------------------------------------------------------------
;Zonas 3
;Si intento hacer un pull , no procederá a pasar nada , ya que se encuentrá actualizado

;((git pull)zonas3)

;Y add , actualizar los archivos con el mismo nombre , reconocer cuantas lineas se agregaron y que cambios se realizaron

;(((git add)null)zonas3)
;(display(zonas->string(((git add)null)zonas3)))

;Archivos que se le reconocieron cambios

;(display((git status)(((git add)null)zonas3)))
;(((git commit)"Nuevas lineas en mi código")(((git add)null)zonas3))
;(display(zonas->string (((git commit)"Nuevas lineas en mi código")(((git add)null)zonas3))))
;Index limpiado , ya que se realizo un commit y un commit más

;(display((git status)(((git commit)"Nuevas lineas en mi código")(((git add)null)zonas3))))
;((git push)(((git commit)"Nuevas lineas en mi código")(((git add)null)zonas3)))
;--------------------------------------------------------------------------------------------------------------------------