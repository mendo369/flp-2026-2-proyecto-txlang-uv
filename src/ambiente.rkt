#lang eopl
;Autores: Santiago Criollo 2380661, Luis David Mendoza Manzano 2067621

#|
  TAD ambiente: liga identificadores a valores.

  Interfaz
    constructores   amb-vacio, amb-extendido, amb-extendido-rec
    observadores    ambiente?, aplicar-amb
    verificación    ambiente?

  El ambiente no conoce el almacén: un identificador nunca se reasigna, lo
  que cambia es el contenido de la celda a la que apunta.

  Un ambiente se crea en tres momentos: después del in de un let, después del
  in de un letrec y al invocar un procedimiento. El if y el begin no crean
  ambientes.

  Dependencias: este módulo requiere a valores.rkt porque aplicar-amb arma la
  clausura de una función de letrec con el constructor de procedimientos.
|#

(require "sintaxis.rkt")
(require "valores.rkt")

(define-datatype ambiente ambiente?
  ;; El ambiente vacío.
  (amb-vacio)
  ;; Una ligadura simultánea de varios identificadores a valores.
  (amb-extendido
   (ids (list-of symbol?))
   (vals (list-of valor?))
   (amb ambiente?))
  ;; El marco de un letrec. No guarda clausuras sino con qué fabricarlas:
  ;; el nombre de cada función, sus parámetros y su cuerpo. La clausura se
  ;; arma al buscar el nombre, con este mismo marco como ambiente, y así la
  ;; función se ve a sí misma y a las demás del grupo (recursión mutua).
  (amb-extendido-rec
   (nombres (list-of symbol?))
   (idss (list-of (list-of symbol?)))
   (cuerpos (list-of expresion?))
   (amb ambiente?)))

;; Busca el valor ligado a un identificador; aborta si no está ligado. Al
;; encontrar una función de letrec fabrica su clausura con el marco del letrec
;; completo (amb) como ambiente, que es lo que rompe la circularidad.
(define aplicar-amb
  (lambda (amb id)
    (cases ambiente amb
      (amb-vacio ()
        (eopl:error 'aplicar-amb "variable no ligada: ~s" id))
      (amb-extendido (ids vals resto)
        (let ((pos (posicion id ids)))
          (if pos
              (list-ref vals pos)
              (aplicar-amb resto id))))
      (amb-extendido-rec (nombres idss cuerpos resto)
        (let ((pos (posicion id nombres)))
          (if pos
              (proc-val (clausura (list-ref idss pos) (list-ref cuerpos pos) amb))
              (aplicar-amb resto id)))))))

;; Posición de un símbolo en una lista, o #f si no está.
(define posicion
  (lambda (id lista)
    (letrec ((buscar (lambda (resto n)
                       (cond ((null? resto) #f)
                             ((eqv? (car resto) id) n)
                             (else (buscar (cdr resto) (+ n 1)))))))
      (buscar lista 0))))

(provide (all-defined-out))
