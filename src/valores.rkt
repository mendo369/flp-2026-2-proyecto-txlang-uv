#lang eopl
;Autores: Luis David Mendoza Manzano 2067621; Santiago Alexander criollo Ordoñez 2380661

#|
  Valores expresados de TxLang-UV y sus extractores.

  Los valores expresados y los denotados son los mismos: el ambiente liga
  identificadores directamente a valores, y la mutación solo ocurre en las
  celdas del almacén, a las que se llega por una referencia.
|#

(require "sintaxis.rkt")

;; Un entero es un número exacto; un flotante, uno inexacto. Así 3 y 3.0 son
;; valores de clases distintas, como exige la aritmética del lenguaje.
(define entero?
  (lambda (x) (and (integer? x) (exact? x))))

(define flotante?
  (lambda (x) (and (real? x) (not (exact? x)))))

;; Una referencia es la posición de una celda en el almacén.
(define referencia?
  (lambda (x) (and (entero? x) (>= x 0))))

;; Acepta cualquier cosa. Se usa en el campo donde la clausura guarda su
;; ambiente: ambiente.rkt requiere a este módulo, así que aquí no se puede
;; nombrar ambiente?. Quien verifica que llegue un ambiente es amb-extendido
;; cuando se aplica el procedimiento.
(define cualquier-cosa?
  (lambda (x) #t))

;; Una clausura captura los parámetros, el cuerpo sin evaluar y el ambiente
;; de definición (alcance léxico).
(define-datatype procval procval?
  (clausura
   (ids (list-of symbol?))
   (cuerpo expresion?)
   (amb cualquier-cosa?)))

(define-datatype valor valor?
  (entero-val (n entero?))
  (flotante-val (f flotante?))
  (cadena-val (s string?))
  (caracter-val (c char?))
  (booleano-val (b boolean?))
  (unit-val)
  (ref-val (r referencia?))
  (proc-val (p procval?))
  (registro-val
   (nombre symbol?)
   (campos (list-of symbol?))
   (valores (list-of valor?))))

;;; Extractores: devuelven el contenido o abortan diciendo qué se esperaba.

(define valor->entero
  (lambda (v)
    (cases valor v
      (entero-val (n) n)
      (else (eopl:error 'valor->entero "se esperaba un entero y llegó ~a" (valor->texto v))))))

(define valor->flotante
  (lambda (v)
    (cases valor v
      (flotante-val (f) f)
      (else (eopl:error 'valor->flotante "se esperaba un flotante y llegó ~a" (valor->texto v))))))

(define valor->cadena
  (lambda (v)
    (cases valor v
      (cadena-val (s) s)
      (else (eopl:error 'valor->cadena "se esperaba una cadena y llegó ~a" (valor->texto v))))))

(define valor->caracter
  (lambda (v)
    (cases valor v
      (caracter-val (c) c)
      (else (eopl:error 'valor->caracter "se esperaba un carácter y llegó ~a" (valor->texto v))))))

(define valor->booleano
  (lambda (v)
    (cases valor v
      (booleano-val (b) b)
      (else (eopl:error 'valor->booleano "se esperaba un booleano y llegó ~a" (valor->texto v))))))

(define valor->ref
  (lambda (v)
    (cases valor v
      (ref-val (r) r)
      (else (eopl:error 'valor->ref "se esperaba una referencia y llegó ~a" (valor->texto v))))))

(define valor->proc
  (lambda (v)
    (cases valor v
      (proc-val (p) p)
      (else (eopl:error 'valor->proc "se esperaba un procedimiento y llegó ~a" (valor->texto v))))))

;;; Texto de un valor.

;; Lo que devuelve correr: las cadenas y los caracteres llevan sus comillas,
;; como en el programa fuente.
(define valor->texto
  (lambda (v)
    (cases valor v
      (entero-val (n) (number->string n))
      (flotante-val (f) (number->string f))
      (cadena-val (s) (string-append "\"" s "\""))
      (caracter-val (c) (string-append "'" (string c) "'"))
      (booleano-val (b) (if b "true" "false"))
      (unit-val () "unit")
      (ref-val (r) (string-append "ref(#" (number->string r) ")"))
      (proc-val (p) "<procedimiento>")
      (registro-val (nombre campos valores)
        (string-append (symbol->string nombre) " {"
                       (campos->texto campos valores) "}")))))

;; Lo que escribe print: igual que valor->texto, salvo que las cadenas y los
;; caracteres salen sin comillas.
(define valor->salida
  (lambda (v)
    (cases valor v
      (cadena-val (s) s)
      (caracter-val (c) (string c))
      (else (valor->texto v)))))

(define campos->texto
  (lambda (campos valores)
    (cond ((null? campos) "")
          ((null? (cdr campos))
           (string-append " " (symbol->string (car campos)) " = " (valor->texto (car valores)) " "))
          (else
           (string-append " " (symbol->string (car campos)) " = " (valor->texto (car valores)) ","
                          (campos->texto (cdr campos) (cdr valores)))))))

(provide (all-defined-out))