#lang eopl
;Autores: Santiago Criollo 2380661, Luis David Mendoza Manzano 2067621

#|
  Unparse: del árbol de sintaxis abstracta de vuelta al texto.

  El texto que produce es un programa válido de TxLang-UV, escrito en una
  sola línea: volver a analizarlo da un árbol igual al de partida. Eso es lo
  que comprueba la prueba ida-y-vuelta de pruebas-txlang.rkt.
|#

(require "sintaxis.rkt")

;; Une una lista de textos con un separador: (unir-con ", " '("a" "b")) => "a, b".
(define unir-con
  (lambda (separador textos)
    (cond ((null? textos) "")
          ((null? (cdr textos)) (car textos))
          (else (string-append (car textos) separador (unir-con separador (cdr textos)))))))

(define unparse
  (lambda (pgm)
    (cases programa pgm
      (un-programa (decls cuerpo)
        (unir-con " "
                  (append (map unparse-decl-registro decls)
                          (list (unparse-expresion cuerpo))))))))

(define unparse-decl-registro
  (lambda (decl)
    (cases decl-registro decl
      (una-decl-registro (nombre campos tipos)
        (string-append "record " (symbol->string nombre)
                       " { " (unparse-pares-tipados campos tipos) " }")))))

;; "x: int, y: bool" a partir de la lista de nombres y la de tipos.
(define unparse-pares-tipados
  (lambda (ids tipos)
    (unir-con ", "
              (map (lambda (id tipo)
                     (string-append (symbol->string id) ": " (unparse-tipo tipo)))
                   ids tipos))))

;; "x = e1, y = e2" a partir de la lista de nombres y la de expresiones.
(define unparse-ligaduras
  (lambda (ids exps)
    (unir-con ", "
              (map (lambda (id exp)
                     (string-append (symbol->string id) " = " (unparse-expresion exp)))
                   ids exps))))

(define unparse-tipo
  (lambda (tipo-sintactico)
    (cases tipo tipo-sintactico
      (int-tipo () "int")
      (float-tipo () "float")
      (bool-tipo () "bool")
      (string-tipo () "string")
      (char-tipo () "char")
      (unit-tipo () "unit")
      (ref-tipo (t) (string-append "ref(" (unparse-tipo t) ")"))
      (proc-tipo (args ret)
        (string-append "(" (unir-con ", " (map unparse-tipo args))
                       " -> " (unparse-tipo ret) ")"))
      (registro-tipo (nombre) (symbol->string nombre)))))

(define unparse-expresion
  (lambda (exp)
    (cases expresion exp
      (entero-exp (n) (number->string n))
      (flotante-exp (f) (number->string f))
      (cadena-exp (lexema) (cadena->token (token->cadena lexema)))
      (caracter-exp (lexema) (caracter->token (token->caracter lexema)))
      (true-exp () "true")
      (false-exp () "false")
      (unit-exp () "unit")
      (var-exp (id) (symbol->string id))
      (let-exp (ids exps cuerpo)
        (string-append "let " (unparse-ligaduras ids exps)
                       " in " (unparse-expresion cuerpo) " end"))
      (ref-exp (e) (string-append "ref(" (unparse-expresion e) ")"))
      (deref-exp (e) (string-append "deref(" (unparse-expresion e) ")"))
      (set-exp (destino valor)
        (string-append "set " (unparse-expresion destino)
                       " := " (unparse-expresion valor)))
      (begin-exp (primera resto)
        (string-append "begin "
                       (unir-con "; " (map unparse-expresion (cons primera resto)))
                       " end"))
      (if-exp (prueba entonces pruebas-elif entonces-elif sino)
        (string-append "if " (unparse-expresion prueba)
                       " then " (unparse-expresion entonces)
                       (apply string-append
                              (map (lambda (p e)
                                     (string-append " elif " (unparse-expresion p)
                                                    " then " (unparse-expresion e)))
                                   pruebas-elif entonces-elif))
                       " else " (unparse-expresion sino) " end"))
      (while-exp (prueba cuerpo)
        (string-append "while " (unparse-expresion prueba)
                       " do " (unparse-expresion cuerpo) " end"))
      (prim-exp (prim args)
        (string-append (unparse-primitiva prim)
                       "(" (unir-con ", " (map unparse-expresion args)) ")"))
      (proc-exp (ids tipos cuerpo)
        (string-append "proc (" (unparse-pares-tipados ids tipos) ") "
                       (unparse-expresion cuerpo) " end"))
      (apply-exp (operador operandos)
        (string-append "apply " (unparse-expresion operador)
                       " (" (unir-con ", " (map unparse-expresion operandos)) ")"))
      (letrec-exp (tipos-ret nombres idss tiposs cuerpos cuerpo)
        (string-append "letrec "
                       (unir-con " "
                                 (map (lambda (tr nombre ids tipos c)
                                        (string-append (unparse-tipo tr) " "
                                                       (symbol->string nombre)
                                                       " (" (unparse-pares-tipados ids tipos) ")"
                                                       " = " (unparse-expresion c)))
                                      tipos-ret nombres idss tiposs cuerpos))
                       " in " (unparse-expresion cuerpo) " end"))
      (atomic-exp (cuerpo id manejador)
        (string-append "atomic " (unparse-expresion cuerpo)
                       " onfail (" (symbol->string id) ") "
                       (unparse-expresion manejador) " end"))
      (abort-exp (e) (string-append "abort(" (unparse-expresion e) ")"))
      (new-exp (nombre campos exps)
        (string-append "new " (symbol->string nombre)
                       " { " (unparse-ligaduras campos exps) " }"))
      (field-exp (e campo)
        (string-append "field(" (unparse-expresion e) ", " (symbol->string campo) ")")))))

(define unparse-primitiva
  (lambda (prim)
    (cases primitiva prim
      (suma-prim () "+")
      (resta-prim () "-")
      (mult-prim () "*")
      (div-prim () "/")
      (mod-prim () "%")
      (mayor-prim () ">")
      (menor-prim () "<")
      (mayor-igual-prim () ">=")
      (menor-igual-prim () "<=")
      (igual-prim () "==")
      (distinto-prim () "!=")
      (and-prim () "and")
      (or-prim () "or")
      (not-prim () "not")
      (concat-prim () "concat")
      (length-prim () "length")
      (to-string-prim () "to-string")
      (to-number-prim () "to-number")
      (print-prim () "print"))))

(provide (all-defined-out))
