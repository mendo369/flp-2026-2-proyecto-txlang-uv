#lang eopl
;Autores: Luis David Mendoza Manzano 2067621; Santiago Alexander criollo Ordoñez 2380661

#|
  Interfaz del interpretador: lo que usan las baterías de pruebas.

    interpretar        texto de un programa  ->  valor expresado
    correr             texto de un programa  ->  el valor como texto
    tipo-del-programa  texto de un programa  ->  su tipo como texto
|#

(require "sintaxis.rkt")
(require "valores.rkt")
(require "almacen.rkt")
(require "tipos.rkt")
(require "interprete.rkt")

;; Analiza, chequea y evalúa. Cada programa corre sobre un almacén nuevo.
(define interpretar
  (lambda (texto)
    (let ((pgm (scan&parse texto)))
      (begin
        (chequear-programa pgm)
        (inicializar-almacen!)
        (value-of-programa pgm)))))

;; Lo que usan las baterías: un texto de TxLang-UV y el valor como texto.
(define correr
  (lambda (texto) (valor->texto (interpretar texto))))

;; Tipo del programa, escrito como texto: "int", "ref(bool)", "(int -> int)".
(define tipo-del-programa
  (lambda (texto)
    (eopl:error 'tipo-del-programa "el chequeador de tipos llega en la segunda entrega")))

(provide (all-defined-out))