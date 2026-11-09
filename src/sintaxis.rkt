#lang eopl
;Autores: Luis David Mendoza Manzano 2067621; Santiago Alexander criollo Ordoñez 2380661

#|
  Sintaxis de TxLang-UV: especificación léxica, especificación gramatical,
  los datatypes del árbol de sintaxis abstracta y el analizador scan&parse.

  Los datatypes los genera SLLGEN a partir de la gramática; los nombres de
  las variantes son los que aparecen al final de cada producción. Los
  separated-list y arbno con varios elementos producen listas paralelas: por
  ejemplo, en let-exp los identificadores van en una lista y las expresiones
  asociadas en otra, en el mismo orden.
|#

;;; ==========================================================
;;; 1. Especificación léxica
;;; ==========================================================
;; Dos clases de comentario (// hasta el fin de línea y #| ... |#, que no
;; admite | adentro), enteros con signo opcional, flotantes, cadenas entre
;; comillas dobles, caracteres entre comillas simples e identificadores.
;; Las palabras reservadas y los símbolos de la gramática los agrega SLLGEN
;; como terminales; ante un empate de longitud con un identificador (por
;; ejemplo to-string) gana la palabra reservada.

(define especificacion-lexica
  '((espacio (whitespace) skip)
    (comentario-linea ("//" (arbno (not #\newline))) skip)
    (comentario-bloque ("#|" (arbno (not #\|)) "|#") skip)
    (identificador (letter (arbno (or letter digit "-" "_" "?"))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (flotante (digit (arbno digit) "." digit (arbno digit)) number)
    (flotante ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (cadena ("\"" (arbno (not #\")) "\"") string)
    (caracter ("'" any "'") string)))

;;; ==========================================================
;;; 2. Especificación gramatical
;;; ==========================================================
;; Un datatype por no terminal: programa, declaración de registro, tipo,
;; expresión y primitiva.

(define especificacion-gramatical
  '((programa ((arbno decl-registro) expresion) un-programa)

    (decl-registro
     ("record" identificador "{" (separated-list identificador ":" tipo ",") "}")
     una-decl-registro)

    (tipo ("int") int-tipo)
    (tipo ("float") float-tipo)
    (tipo ("bool") bool-tipo)
    (tipo ("string") string-tipo)
    (tipo ("char") char-tipo)
    (tipo ("unit") unit-tipo)
    (tipo ("ref" "(" tipo ")") ref-tipo)
    (tipo ("(" (separated-list tipo ",") "->" tipo ")") proc-tipo)
    (tipo (identificador) registro-tipo)

    (expresion (numero) entero-exp)
    (expresion (flotante) flotante-exp)
    (expresion (cadena) cadena-exp)
    (expresion (caracter) caracter-exp)
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)
    (expresion ("unit") unit-exp)
    (expresion (identificador) var-exp)
    (expresion
     ("let" (separated-list identificador "=" expresion ",") "in" expresion "end")
     let-exp)
    (expresion ("ref" "(" expresion ")") ref-exp)
    (expresion ("deref" "(" expresion ")") deref-exp)
    (expresion ("set" expresion ":=" expresion) set-exp)
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion
     ("if" expresion "then" expresion
      (arbno "elif" expresion "then" expresion)
      "else" expresion "end")
     if-exp)
    (expresion ("while" expresion "do" expresion "end") while-exp)
    (expresion (primitiva "(" (separated-list expresion ",") ")") prim-exp)
    (expresion
     ("proc" "(" (separated-list identificador ":" tipo ",") ")" expresion "end")
     proc-exp)
    (expresion ("apply" expresion "(" (separated-list expresion ",") ")") apply-exp)
    (expresion
     ("letrec"
      (arbno tipo identificador
             "(" (separated-list identificador ":" tipo ",") ")"
             "=" expresion)
      "in" expresion "end")
     letrec-exp)
    (expresion
     ("atomic" expresion "onfail" "(" identificador ")" expresion "end")
     atomic-exp)
    (expresion ("abort" "(" expresion ")") abort-exp)
    (expresion
     ("new" identificador "{" (separated-list identificador "=" expresion ",") "}")
     new-exp)
    (expresion ("field" "(" expresion "," identificador ")") field-exp)

    (primitiva ("+") suma-prim)
    (primitiva ("-") resta-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("/") div-prim)
    (primitiva ("%") mod-prim)
    (primitiva (">") mayor-prim)
    (primitiva ("<") menor-prim)
    (primitiva (">=") mayor-igual-prim)
    (primitiva ("<=") menor-igual-prim)
    (primitiva ("==") igual-prim)
    (primitiva ("!=") distinto-prim)
    (primitiva ("and") and-prim)
    (primitiva ("or") or-prim)
    (primitiva ("not") not-prim)
    (primitiva ("concat") concat-prim)
    (primitiva ("length") length-prim)
    (primitiva ("to-string") to-string-prim)
    (primitiva ("to-number") to-number-prim)
    (primitiva ("print") print-prim)))

(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

(define scan&parse
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

;; Muestra los define-datatype que SLLGEN generó a partir de la gramática.
(define mostrar-datatypes
  (lambda ()
    (sllgen:list-define-datatypes especificacion-lexica especificacion-gramatical)))

;; Las cadenas y los caracteres llegan del analizador léxico como el lexema
;; completo, con sus comillas ("hola", 'x'). El árbol guarda ese lexema tal
;; cual; estos ayudantes sacan el contenido para evaluar y vuelven a poner las
;; comillas para el unparse. Toleran que las comillas ya vengan quitadas, para
;; no depender de ese detalle de SLLGEN.

(define entre-comillas?
  (lambda (texto comilla)
    (and (>= (string-length texto) 2)
         (char=? (string-ref texto 0) comilla)
         (char=? (string-ref texto (- (string-length texto) 1)) comilla))))

(define token->cadena
  (lambda (texto)
    (if (entre-comillas? texto #\")
        (substring texto 1 (- (string-length texto) 1))
        texto)))

(define cadena->token
  (lambda (contenido)
    (string-append "\"" contenido "\"")))

(define token->caracter
  (lambda (texto)
    (if (entre-comillas? texto #\')
        (string-ref texto 1)
        (string-ref texto 0))))

(define caracter->token
  (lambda (c)
    (string-append "'" (string c) "'")))

(provide (all-defined-out))