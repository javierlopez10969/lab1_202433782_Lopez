#lang racket
;Zona de modulación
(require "funcionesAnexas.rkt")
(require "TDA_zonas.rkt")

;Archivo donde se encuentra la función add
;----------------------------------------------------------------------------------------------------------------
;Función : add , añade los cambios locales al index 
;Dominio : zonas
;Recorrido: lista de nombre s de archivos
;Tipo de Recursión: recursion natural , siendo disntinto al de pull
(define add
  (lambda (lista)
    (if (list? lista)
        ;True case
        (lambda (zonas)
          ;Pregunto, es una zona? , y si el workspace no esta vacío
          (if (and (zonas? zonas) (not (null? (selectorIndice zonas 0))))
              (if (verificar-nulos (delta-cambios lista zonas))
              "No se procederá a realizar add , no hay cambios registrados" 
              (cambiar-elemento zonas 1 (borrar-nulos (delta-cambios lista zonas))))
              "Ingrese una zona válida"))
        ;False
        "ingrese una lista")))
;ejemplo de uso
;(((git add)null)zonas)
;(((git add)(list "main.c" "busqueda.h"))zonas)

;Add-----------------------------------------------------------------------------------------------------------------------
;En esta Sección se implementa la función add , la cual se trata de usar recursión natural en todas sus funciones
;Ya que eso se indica en los requerimientos

;Función que verifica que no haya solamente delta-cambios nulos
;Dominio:Index
;Recorrido: Boolean
;Recursión de cola
(define verificar-nulos
  (lambda (index)
    (define recorrer-delta
      (lambda (delta)
        (if (null? delta)
            #t
            (if (null? (get-primero delta))
                (recorrer-delta (get-resto delta))
                #f))))(recorrer-delta (selectorIndice index 1) )))


;Función que borre los elementos nulos dentro de index que no presenten cambios
;Dominio:index
;Recorrido : index 
;Recursión : de cola
(define borrar-nulos
  (lambda (index)
    (define borrar-delta
      (lambda (delta)
        ;Si el quitar un nulo hace que me devuelva el mismo delta , devuelvo el delta
        (if (equal?  (remove null delta) delta)
            delta
            ;si no sigo borrando
            (borrar-delta (remove null delta)))))
    ;Llamo a la función para que borre los distintos nulos
    (cambiar-elemento index 1 (borrar-delta (selectorIndice index 1)))))

;Función que genera una lista con TODOS los nombres de archivos dentro de un workspace
;Dominio : Workspace ; Recorrido : lista
;Recursión : Natural 
(define get-namesN
      (lambda (workspace)
        ;Me encuentro al final del workspace?
        (if (null? workspace)
            null
            ;No? devuelvo un par y dentro hago una llamada recursiva hasta llegar a null
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

;Función que solo guarda los archivos que se indiquen en lista
;Dominio : Lista Con nombres de archivos X Workspace actual donde se buscan los nombres de los archivos
;Recorrido : Workspace
;Recursión : Natural
(define guardar-workspace-actual
  (lambda (lista actual-workspace)
    ;Estoy al final de mi lista?
    (if (null? lista)
        null
        (cons
         ;Guardo el archivo segun su nombre
         (selectorIndice actual-workspace (buscar-archivo actual-workspace (get-primero lista)))
         ;sigo recorriendo la lista
         (guardar-workspace-actual (get-resto lista) actual-workspace)))
    ))
;Función que fusiona el workspace actual con archivos especificos con el workspace del último commit realizado
;Dominio : Lista con nombres de los archivos X Workspace-actual X last-workspace
;Recorrido : Workspace
;Recursión : Natural
(define guardar-workspace-actual-y-viejo
  (lambda (lista actual-workspace last-workspace)
    ;Me encuentro al final de la lista del actual workspace?
    (if (null? lista)
        ;Ahora sigo recorriendo el veijo workspace sin los archivos que ya agrege;no se logro a implementar
        null
        (cons
         ;Guardo el archivo segun su nombre
         (selectorIndice actual-workspace (buscar-archivo actual-workspace (get-primero lista)))
         ;sigo recorriendo la lista
         (guardar-workspace-actual (get-resto lista) actual-workspace)))))

;Función que compara las inseciones y deletions ocurridas en un archivo con respecto al último commit en el local-repository
;Dominio: Archivo x Arcħivo X Entero X Entero
;Recorrido : Delta-Cambio
;Recursión de Cola
(define comparar-lineas
  (lambda (archivo1 archivo2 deletions insertions name )
    (cond
      ;Si ambos archivos son nulos
      [(and (null? archivo1) (null? archivo2))
           (list name insertions deletions (list name) #f #f)]
      ;Si mi archivo1 ya es nulo , empiezo a contar , deletions
      [(and (null? archivo1) (not (null? archivo2)))
       (comparar-lineas archivo1 (get-resto archivo2) (+ 1 deletions) insertions name)]
      ;Si mi archivo2 ya es nulo , empiezo a contar , insertions
      [(and (not(null? archivo1)) (null? archivo2))
       (comparar-lineas (get-resto archivo1) archivo2 deletions (+ 1 insertions) name)]
      ;Si ambas lineas son iguales , no agrego nada
      [(equal? (get-primero archivo1) (get-primero archivo2))
       (comparar-lineas (get-resto archivo1) (get-resto archivo2) deletions insertions name )]
      ;Si no son iguales agrego deletions como insertions
      [(not (equal? (get-primero archivo1) (get-primero archivo2)))
       (comparar-lineas (get-resto archivo1) (get-resto archivo2) (+ 1 deletions) (+ 1 insertions) name )]
      
    )))

;Función constructora delta cambios según el último commit realizado dentro del local repository
;Funcion que genera cambios de una archivo con respecto al ultimo commit
;Entrada : archivo , workspace de el ultimo commit
;Dominio : archivo x workspace
;Recorrido: delta-cambios de un archivo
(define generar-cambios-dos-archivos
  (lambda (archivo workspace)
    ;veo si acaso existe el mismo nombre dentro del commit o un partial match
    (cond
      ;CASO MUY ESPECIAL , workspace vacío
      [(null? workspace) (list (get-primero archivo) (- (longitud archivo) 1) 0 (list (get-primero archivo))   #f #t )]
      ;Caso 1 que tengan el mismo nombre
      [(>= (buscar-archivo workspace (get-primero archivo)) 0 )
       (cond
         ;Si encuentro el nombre del archivo , ahora pregunto si tienen el mismo contenido
         [(equal? (selectorIndice workspace (buscar-archivo workspace (get-primero archivo)))archivo)
          ;Si tienen el mismo contenido y nombre devuelvo null
          null]
         ;si no tienen el mismo contenido voy comparando linea por linea que se cambio
         ;deletions and insertions
         [else
          (comparar-lineas archivo (selectorIndice workspace (buscar-archivo workspace (get-primero archivo)) ) 0 0
                                (get-primero archivo))])
       ]
      ;Caso 2 que tengan el mismo contenido    
      ;Caso de que ya no existe
      [(< (buscar-archivo workspace (get-primero archivo)) 0 )
       (list (get-primero archivo) 0 0 (list (get-primero archivo))   #t #f )]
      ;Caso de que no existía anteriormente
      [else (list (get-primero archivo) (- (longitud archivo) 1) 0 (list (get-primero archivo))   #f #t )]
      )
    ))

;Función que genera la lista con todos los index
;Dominio: Workspace con elq ue se quiere trabajar X last-workspace
;Recorrido : Lista de delta cambios
;Recursión : De cola , porque no funcionaba natural
(define generar-lista-delta-cambios
  (lambda (workspace-guardado last-workspace)
    (define get-lista
      (lambda (workspace-guardado last-workspace lista-delta-cambios)
    (if (null? workspace-guardado)
        ;Si nos encontramos al final del workspace que guardamos entregamos la lista de deltacambios
        lista-delta-cambios
       (get-lista (get-resto workspace-guardado) last-workspace
(añadir-elemento lista-delta-cambios (generar-cambios-dos-archivos (get-primero workspace-guardado) last-workspace))))))
    (get-lista workspace-guardado last-workspace null)))

;se añaden todos los archivos en que se registran cambios locales
;Función que añade todos los archivos requerridos al index
;Dominio : Lista con los nombres de los archivos X Actual-Workspace X Ultimo-workspace X zonas
;Recorrido = index
;Recursión Natural
(define generar-index
  (lambda (lista actual-workspace last-workspace zonas)
    ;Pegunto si acaso las longitudes de los nombres de los archivos son iguales con la del workspace actual
    ;Se asume que es un add all , en caso contrario se combinan los archivos del last commit
    (if (= (longitud lista) (longitud actual-workspace))
        ;Si es asi solo guardo el actual workspace
        (cons
         (guardar-workspace-actual lista actual-workspace)
         (list(generar-lista-delta-cambios (guardar-workspace-actual lista actual-workspace) last-workspace)))
         
        ;si no es así guardo los archivos del workspace actual en conjunto de los archivos viejos
        (cons
         ;Actual+ viejo
         (guardar-workspace-actual-y-viejo lista actual-workspace last-workspace )
         ;Lista delta-cambios
         (append (list(generar-lista-delta-cambios
          (guardar-workspace-actual-y-viejo lista actual-workspace last-workspace ) last-workspace)))))
    ;Delta cambios
    ;Ahora los cambios realizados se compararan con los viejos
    ))

;Funcion que busca las coincidencias de en los archivos del workspace dentro del commit mas reciente
;dominio = lista de nombres de archivos X zonas
;Recorrido = zonas
;Funcion que busca archivo por archivo los cambios realizados
(define delta-cambios
  (lambda (lista zonas)
    ;Función que verifica que los archivos entregados si se encuentran dentro del workspace
    (define verificar-lista
      (lambda (lista workspace)
        (if (null? lista)
            ;Si ya me encuentro al final de la lista signfica que si se encontraron todos los archivos dentro del workspace
            #t
            ;pregunto si se encuentra dentro del workspace
            (if (>= (buscar-archivo workspace (get-primero lista)) 0)
                ;True Case
                (verificar-lista (get-resto lista) zonas)
                ;Sino se encuentran dentro del workspace
                "Fatal error , los archivos entregados no existen en el workspace"))))
    ;si no es nula la lista entregada , verifico que los nombres entregados si corresponden a los entregados.
    (if (not(null? lista))
        (if (equal? #t (verificar-lista lista (selectorIndice zonas 0)))
            ;Caso de que no haya ningún local-repository
            (if (null? (selectorIndice zonas 2))
                (generar-index lista (selectorIndice zonas 0) null zonas)
            (generar-index lista (selectorIndice zonas 0) (get-last-workspace zonas) zonas))
            "Fatal error , los archivos entregados no existen en el workspace"
            )
        ;Creo una lista con todos los nombres de las funciones
        ;Caso de que no haya ningún local-repository
        (if (null? (selectorIndice zonas 2))
        (generar-index (get-namesN (selectorIndice zonas 0)) (selectorIndice zonas 0) null zonas)
        (generar-index (get-namesN (selectorIndice zonas 0)) (selectorIndice zonas 0) (get-last-workspace zonas) zonas)) )))

;(delta-cambios (list) zonas)
;(get-last-workspace zonas)

;Zona de provide
(provide add)