#lang eopl
;Autores: Santiago Criollo 2380661, Nombre2 Codigo2, ...

#|
  TAD almacén: celdas que se reservan con ref, se leen con deref y se
  escriben con set.

  Interfaz
    inicializar-almacen!         deja el almacén vacío
    nueva-ref! v                 reserva una celda con el valor v y devuelve su referencia
    leer-ref r                   contenido de la celda r
    escribir-ref! r v            escribe v en la celda r
    tamano-almacen               cantidad de celdas reservadas

  Una referencia es la posición de la celda. El almacén es único para todo
  el programa (estado global del módulo, como en EOPL 4.2) y se guarda en un
  vector que crece al doble cuando se llena.

  Diseño pensando en las transacciones. En esta entrega las escrituras son
  definitivas, pero la interfaz ya está trazada para lo que viene:

    - escribir-ref! es la ÚNICA operación que modifica una celda existente, y
      nueva-ref! la única que reserva una. Para llevar el registro de una
      transacción basta, entonces, tocar esas dos funciones: antes de
      sobrescribir se apunta el par (referencia, valor anterior) en la
      transacción abierta, y al reservar se apunta la referencia nueva.
    - Reservar es siempre agregar al final, así que descartar las celdas
      creadas dentro de un bloque que aborta es devolver tamano-almacen al
      que tenía al abrirlo.
    - La pila de transacciones se agrega como un segundo estado del módulo,
      con abrir-transaccion!, confirmar-transaccion! y revertir-transaccion!,
      sin cambiar la interfaz de arriba.

  Este módulo no necesita conocer qué guarda cada celda, por lo que no
  depende de valores.rkt.
|#

(define capacidad-inicial 16)

(define the-store 'almacen-sin-inicializar)
(define siguiente-ref 0)

(define inicializar-almacen!
  (lambda ()
    (begin
      (set! the-store (make-vector capacidad-inicial 0))
      (set! siguiente-ref 0))))

(define tamano-almacen
  (lambda () siguiente-ref))

;; Duplica la capacidad del vector conservando el contenido.
(define crecer-almacen!
  (lambda ()
    (let* ((viejo the-store)
           (nuevo (make-vector (* 2 (vector-length viejo)) 0)))
      (letrec ((copiar (lambda (i)
                         (if (< i (vector-length viejo))
                             (begin (vector-set! nuevo i (vector-ref viejo i))
                                    (copiar (+ i 1)))
                             'listo))))
        (begin (copiar 0)
               (set! the-store nuevo))))))

(define nueva-ref!
  (lambda (v)
    (begin
      (if (= siguiente-ref (vector-length the-store))
          (crecer-almacen!)
          'sin-cambio)
      (let ((r siguiente-ref))
        (begin
          (vector-set! the-store r v)
          (set! siguiente-ref (+ siguiente-ref 1))
          r)))))

(define verificar-ref
  (lambda (quien r)
    (if (and (integer? r) (>= r 0) (< r siguiente-ref))
        r
        (eopl:error quien "referencia inválida: ~s" r))))

(define leer-ref
  (lambda (r)
    (begin
      (verificar-ref 'leer-ref r)
      (vector-ref the-store r))))

(define escribir-ref!
  (lambda (r v)
    (begin
      (verificar-ref 'escribir-ref! r)
      (vector-set! the-store r v))))

(provide (all-defined-out))
