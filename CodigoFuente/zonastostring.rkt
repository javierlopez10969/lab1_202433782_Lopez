#lang racket
(require "TDA_zonas.rkt")
(require "funcionesAnexas.rkt")
;Zonas->string

;MODULO zonas->string y log

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

;zonas->string
(provide zonas->string)
;log
(provide verificar-log)
