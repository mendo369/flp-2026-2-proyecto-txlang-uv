#lang eopl
;Autores: Luis David Mendoza Manzano 2067621; Santiago Alexander criollo Ordoñez 2380661

#|
  Chequeador de tipos (segunda entrega).

  Recibirá el programa y lo devolverá si está bien tipado; si no, abortará
  con eopl:error antes de que se evalúe, diciendo qué tipo esperaba y cuál
  encontró. El tipo, su ambiente y el chequeo de cada expresión viven aquí.

  Mientras no exista, chequear-programa deja pasar todo.
|#

(require "sintaxis.rkt")

(define chequear-programa
  (lambda (pgm) pgm))

(provide (all-defined-out))