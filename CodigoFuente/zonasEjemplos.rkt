#lang racket
;Ejemplo número 1 de zonas para efectuar la función pull de manera sencilla
(define zonas1
  (list
   ;Workspace
   (list)
   ;Index
   (list)
   ;Local-repository
   (list)
   ;Remote-repository
   (list
    (list"New workspace"
         (list 9 46 11 28 5 2020)
         ;Copia del workspace
         (list(list"main.c" "\n#include <stdio.h> \n" "int main (){ \n" "printf('Hola') }\n")
              (list"busqueda.h" "\n#include <stdio.h>\n" "//funcion que realiza una busqueda\n"))
         (list (list"main.c" 0 0 (list"main.c") #t #f)))
    (list"my commit" (list 9 46 11 28 5 2020) (list(list"BEE.c" "linea1\n")) (list(list"BEE.c" 1 0 (list"BEE.c") #f #t)))
    (list"primer commit" (list 10 35 22 18 5 2020) (list) (list)))
   ;Registro Hisorico
   (list)))
;Ejemplo de pull con un archivo dentro y como se combinan
(define zonas2
  (list
   ;Workspace
   (list
    (list "como salvar el semestre online.pdf"
          "Paso número 1 : Estudiar más de lo que se estudia en un semestre normal\n"
          "Paso número 2 : Si el paso número uno no dio frutos , irse a paro\n"))
   ;Index
   (list)
   ;Local-repository
   (list)
   ;Remote-repository
   (list
    (list"New workspace"
         (list 9 46 11 28 5 2020)
         ;Copia del workspace
         (list(list"main.c" "\n#include <stdio.h> \n" "int main (){ \n" "printf('Hola') }\n")
              (list"busqueda.h" "\n#include <stdio.h>\n" "//funcion que realiza una busqueda\n"))
         (list (list"main.c" 0 0 (list"main.c") #t #f)))
    (list"my commit" (list 9 46 11 28 5 2020) (list(list"BEE.c" "linea1\n")) (list(list"BEE.c" 1 0 (list"BEE.c") #f #t)))
    (list"primer commit" (list 10 35 22 18 5 2020) (list) (list)))
   ;Registro Hisorico
   (list)))
;Ejemplo número 3 para la función add , un cambio de una linea extra , y un nuevo archivo , además que pasa si se hace pull
(define zonas3
  (list
   ;Workspace
   (list(list"main.c"
             "\n#include <stdio.h> \n"
             "int main (){ \n"
             "int arreglo [50] = {1,2,4,5,6,7} ;\n"
             "//Realizamos la búsqueda\n")
        (list"busqueda.h"
             "\n#include <stdio.h>\n"
             "//funcion que realiza una busqueda\n"
             "void busqueda (int * arreglo , elemento){\n"))
   ;Index
   (list)
   ;Local-repository
   (list
    (list"New workspace"
         (list 9 46 11 28 5 2020)
         ;Copia del workspace
         (list(list"main.c" "\n#include <stdio.h> \n" "int main (){ \n" "printf('Hola') }\n")
              (list"busqueda.h" "\n#include <stdio.h>\n" "//funcion que realiza una busqueda\n"))
         (list (list"main.c" 0 0 (list"main.c") #t #f)))
    (list"my commit" (list 9 46 11 28 5 2020) (list(list"BEE.c" "linea1\n")) (list(list"BEE.c" 1 0 (list"BEE.c") #f #t)))
    (list"primer commit" (list 10 35 22 18 5 2020) (list) (list)))
   ;Remote-repository
   (list
    (list"New workspace"
         (list 9 46 11 28 5 2020)
         ;Copia del workspace
         (list(list"main.c" "\n#include <stdio.h> \n" "int main (){ \n" "printf('Hola') }\n")
              (list"busqueda.h" "\n#include <stdio.h>\n" "//funcion que realiza una busqueda\n"))
         (list (list"main.c" 0 0 (list"main.c") #t #f)))
    (list"my commit" (list 9 46 11 28 5 2020) (list(list"BEE.c" "linea1\n")) (list(list"BEE.c" 1 0 (list"BEE.c") #f #t)))
    (list"primer commit" (list 10 35 22 18 5 2020) (list) (list)))
   ;Registro Hisorico
   (list)))

(provide zonas1)
(provide zonas2)
(provide zonas3)