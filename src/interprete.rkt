#lang eopl
;Autores: Luis David Mendoza Manzano 2067621; Santiago Alexander criollo Ordoñez 2380661

#|
  Interpretador de TxLang-UV: value-of y las primitivas.

  value-of recibe una expresión y un ambiente y devuelve un valor expresado.
  El almacén es el único del módulo almacen.rkt: value-of lo modifica solo en
  ref, set y las primitivas que lo requieran, nunca el ambiente.

  Primera entrega: está todo lo del alcance menos transacciones, registros
  y chequeo de tipos. atomic, abort, new y field se analizan y se escriben de
  vuelta, pero evaluarlos aborta con un mensaje que lo dice; llegan en la
  segunda entrega. Las declaraciones de registro tampoco se usan todavía.
|#

(require "sintaxis.rkt")
(require "valores.rkt")
(require "ambiente.rkt")
(require "almacen.rkt")

;;; ==========================================================
;;; Programa
;;; ==========================================================

(define value-of-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (decls cuerpo)
        (value-of cuerpo (amb-vacio))))))

;;; ==========================================================
;;; Expresiones
;;; ==========================================================

(define value-of
  (lambda (exp amb)
    (cases expresion exp
      (entero-exp (n) (entero-val n))
      (flotante-exp (f) (flotante-val f))
      (cadena-exp (lexema) (cadena-val (token->cadena lexema)))
      (caracter-exp (lexema) (caracter-val (token->caracter lexema)))
      (true-exp () (booleano-val #t))
      (false-exp () (booleano-val #f))
      (unit-exp () (unit-val))
      (var-exp (id) (aplicar-amb amb id))

      ;; Las ligaduras se evalúan en el ambiente exterior: no se ven entre sí.
      (let-exp (ids exps cuerpo)
        (value-of cuerpo (amb-extendido ids (evaluar-lista exps amb) amb)))

      (ref-exp (e)
        (ref-val (nueva-ref! (value-of e amb))))
      (deref-exp (e)
        (leer-ref (valor->ref (value-of e amb))))
      (set-exp (destino valor-exp)
        (let* ((r (valor->ref (value-of destino amb)))
               (v (value-of valor-exp amb)))
          (begin (escribir-ref! r v)
                 (unit-val))))

      (begin-exp (primera resto)
        (evaluar-secuencia (cons primera resto) amb))

      (if-exp (prueba entonces pruebas-elif entonces-elif sino)
        (evaluar-if (cons prueba pruebas-elif)
                    (cons entonces entonces-elif)
                    sino amb))

      (while-exp (prueba cuerpo)
        (letrec ((ciclo (lambda ()
                          (if (condicion->booleano 'while (value-of prueba amb))
                              (begin (value-of cuerpo amb)
                                     (ciclo))
                              (unit-val)))))
          (ciclo)))

      (prim-exp (prim args)
        (aplicar-primitiva prim (evaluar-lista args amb)))

      ;; proc solo captura: parámetros, cuerpo sin evaluar y ambiente de definición.
      (proc-exp (ids tipos cuerpo)
        (proc-val (clausura ids cuerpo amb)))

      (apply-exp (operador operandos)
        (let* ((f (value-of operador amb))
               (args (evaluar-lista operandos amb)))
          (aplicar-procedimiento f args)))

      ;; Los tipos declarados no se usan al evaluar: son asunto del chequeador.
      (letrec-exp (tipos-ret nombres idss tiposs cuerpos cuerpo)
        (value-of cuerpo (amb-extendido-rec nombres idss cuerpos amb)))

      (atomic-exp (cuerpo id manejador)
        (eopl:error 'value-of "atomic llega en la segunda entrega"))
      (abort-exp (e)
        (eopl:error 'value-of "abort llega en la segunda entrega"))
      (new-exp (nombre campos exps)
        (eopl:error 'value-of "new llega en la segunda entrega"))
      (field-exp (e campo)
        (eopl:error 'value-of "field llega en la segunda entrega")))))

;; Evalúa de izquierda a derecha y devuelve la lista de valores.
(define evaluar-lista
  (lambda (exps amb)
    (if (null? exps)
        '()
        (let ((v (value-of (car exps) amb)))
          (cons v (evaluar-lista (cdr exps) amb))))))

;; Evalúa de izquierda a derecha y devuelve el último valor.
(define evaluar-secuencia
  (lambda (exps amb)
    (let ((v (value-of (car exps) amb)))
      (if (null? (cdr exps))
          v
          (evaluar-secuencia (cdr exps) amb)))))

;; Recorre las pruebas del if y los elif; la primera verdadera decide.
(define evaluar-if
  (lambda (pruebas ramas sino amb)
    (if (null? pruebas)
        (value-of sino amb)
        (if (condicion->booleano 'if (value-of (car pruebas) amb))
            (value-of (car ramas) amb)
            (evaluar-if (cdr pruebas) (cdr ramas) sino amb)))))

;; Exige un booleano; si no lo es, reporta el error con eopl:error.
(define condicion->booleano
  (lambda (quien v)
    (cases valor v
      (booleano-val (b) b)
      (else (eopl:error quien "la condición debe ser booleana y llegó ~a"
                        (valor->texto v))))))

;; Aplica un procedimiento a sus argumentos ya evaluados. El cuerpo se
;; evalúa en el ambiente de la clausura extendido con los parámetros.
(define aplicar-procedimiento
  (lambda (f args)
    (cases valor f
      (proc-val (p)
        (cases procval p
          (clausura (ids cuerpo amb-clausura)
            (if (= (length ids) (length args))
                (value-of cuerpo (amb-extendido ids args amb-clausura))
                (eopl:error 'apply "el procedimiento espera ~s argumento(s) y recibió ~s"
                            (length ids) (length args))))))
      (else (eopl:error 'apply "se esperaba un procedimiento y llegó ~a"
                        (valor->texto f))))))

;;; ==========================================================
;;; Primitivas
;;; ==========================================================
;; Reciben la lista de operandos ya evaluados.

(define aplicar-primitiva
  (lambda (prim vals)
    (cases primitiva prim
      (suma-prim () (aritmetica "+" + + vals))
      (resta-prim () (aritmetica "-" - - vals))
      (mult-prim () (aritmetica "*" * * vals))
      (div-prim ()
        (aritmetica "/"
                    (lambda (x y) (if (= y 0) (eopl:error '/ "división por cero") (quotient x y)))
                    (lambda (x y) (if (= y 0) (eopl:error '/ "división por cero") (/ x y)))
                    vals))
      (mod-prim ()
        (let ((ab (numeros-del-mismo-tipo "%" vals)))
          (if (entero? (car ab))
              (if (= (cadr ab) 0)
                  (eopl:error '% "módulo por cero")
                  (entero-val (remainder (car ab) (cadr ab))))
              (eopl:error '% "% solo acepta enteros y llegaron flotantes"))))
      (mayor-prim () (comparacion ">" > vals))
      (menor-prim () (comparacion "<" < vals))
      (mayor-igual-prim () (comparacion ">=" >= vals))
      (menor-igual-prim () (comparacion "<=" <= vals))
      (igual-prim ()
        (begin (aridad! "==" 2 vals)
               (booleano-val (valores-iguales? "==" (car vals) (cadr vals)))))
      (distinto-prim ()
        (begin (aridad! "!=" 2 vals)
               (booleano-val (not (valores-iguales? "!=" (car vals) (cadr vals))))))
      (and-prim ()
        (begin (aridad! "and" 2 vals)
               (booleano-val (and (operando-booleano "and" (car vals))
                                  (operando-booleano "and" (cadr vals))))))
      (or-prim ()
        (begin (aridad! "or" 2 vals)
               (booleano-val (or (operando-booleano "or" (car vals))
                                 (operando-booleano "or" (cadr vals))))))
      (not-prim ()
        (begin (aridad! "not" 1 vals)
               (booleano-val (not (operando-booleano "not" (car vals))))))
      (concat-prim ()
        (begin (aridad! "concat" 2 vals)
               (cadena-val (string-append (operando-cadena "concat" (car vals))
                                          (operando-cadena "concat" (cadr vals))))))
      (length-prim ()
        (begin (aridad! "length" 1 vals)
               (entero-val (string-length (operando-cadena "length" (car vals))))))
      (to-string-prim ()
        (begin (aridad! "to-string" 1 vals)
               (cadena-val (valor->cadena-conversion (car vals)))))
      (to-number-prim ()
        (begin (aridad! "to-number" 1 vals)
               (cadena->numero (operando-cadena "to-number" (car vals)))))
      (print-prim ()
        (begin (aridad! "print" 1 vals)
               (display (valor->salida (car vals)))
               (newline)
               (unit-val))))))

(define aridad!
  (lambda (nombre n vals)
    (if (= (length vals) n)
        'ok
        (eopl:error 'primitiva "~a espera ~s operando(s) y recibió ~s"
                    nombre n (length vals)))))

;; El número que hay dentro de un entero o un flotante.
(define numero-crudo
  (lambda (nombre v)
    (cases valor v
      (entero-val (n) n)
      (flotante-val (f) f)
      (else (eopl:error 'primitiva "~a espera operandos numéricos y llegó ~a"
                        nombre (valor->texto v))))))

;; Verifica la aridad y que los dos operandos sean números de la misma clase
;; (los dos enteros o los dos flotantes). Devuelve los dos números crudos.
(define numeros-del-mismo-tipo
  (lambda (nombre vals)
    (begin
      (aridad! nombre 2 vals)
      (let ((a (numero-crudo nombre (car vals)))
            (b (numero-crudo nombre (cadr vals))))
        (if (eq? (entero? a) (entero? b))
            (list a b)
            (eopl:error 'primitiva
                        "~a exige operandos del mismo tipo numérico y llegaron ~a y ~a"
                        nombre (valor->texto (car vals)) (valor->texto (cadr vals))))))))

(define aritmetica
  (lambda (nombre op-entero op-flotante vals)
    (let* ((ab (numeros-del-mismo-tipo nombre vals))
           (x (car ab))
           (y (cadr ab)))
      (if (entero? x)
          (entero-val (op-entero x y))
          (flotante-val (op-flotante x y))))))

(define comparacion
  (lambda (nombre op vals)
    (let ((ab (numeros-del-mismo-tipo nombre vals)))
      (booleano-val (op (car ab) (cadr ab))))))

(define operando-booleano
  (lambda (nombre v)
    (cases valor v
      (booleano-val (b) b)
      (else (eopl:error 'primitiva "~a espera booleanos y llegó ~a"
                        nombre (valor->texto v))))))

(define operando-cadena
  (lambda (nombre v)
    (cases valor v
      (cadena-val (s) s)
      (else (eopl:error 'primitiva "~a espera una cadena y llegó ~a"
                        nombre (valor->texto v))))))

;; == y != comparan valores de la misma clase.
(define valores-iguales?
  (lambda (nombre a b)
    (let ((incompatibles
           (lambda ()
             (eopl:error 'primitiva "~a no puede comparar ~a con ~a"
                         nombre (valor->texto a) (valor->texto b)))))
      (cases valor a
        (entero-val (x)
          (cases valor b (entero-val (y) (= x y)) (else (incompatibles))))
        (flotante-val (x)
          (cases valor b (flotante-val (y) (= x y)) (else (incompatibles))))
        (cadena-val (x)
          (cases valor b (cadena-val (y) (string=? x y)) (else (incompatibles))))
        (caracter-val (x)
          (cases valor b (caracter-val (y) (char=? x y)) (else (incompatibles))))
        (booleano-val (x)
          (cases valor b (booleano-val (y) (eq? x y)) (else (incompatibles))))
        (unit-val ()
          (cases valor b (unit-val () #t) (else (incompatibles))))
        (ref-val (x)
          (cases valor b (ref-val (y) (= x y)) (else (incompatibles))))
        (else (incompatibles))))))

;; to-string: el texto de un número, booleano, carácter, cadena o unit.
(define valor->cadena-conversion
  (lambda (v)
    (cases valor v
      (proc-val (p) (eopl:error 'to-string "no se puede convertir un procedimiento"))
      (registro-val (n c vs) (eopl:error 'to-string "no se puede convertir un registro"))
      (else (valor->salida v)))))

;; to-number: un entero si el texto lo es, un flotante si trae punto decimal.
(define cadena->numero
  (lambda (texto)
    (let ((n (string->number texto)))
      (cond ((and n (real? n) (exact? n) (integer? n)) (entero-val n))
            ((and n (real? n) (not (exact? n))) (flotante-val n))
            (else (eopl:error 'to-number "la cadena ~s no es un número" texto))))))

(provide (all-defined-out))