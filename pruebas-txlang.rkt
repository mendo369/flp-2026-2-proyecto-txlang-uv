#lang eopl
;Autores: Santiago Criollo 2380661, Luis David Mendoza Manzano 2067621

#|
  Batería de pruebas del interpretador de TxLang-UV.

  Cada prueba compara el texto que devuelve `correr` con el que se espera, o
  comprueba que un programa se rechace. La primera entrega deja aquí al
  menos 15 programas que cubran todo su alcance; la segunda, al menos 20 que
  cubran el proyecto completo y al menos 5 programas mal tipados con el
  error que el chequeador reporta en cada uno.

  Se ejecuta con:  racket pruebas-txlang.rkt
|#

(require (only-in racket/base with-handlers exn:fail? exn-message))
(require "interprete-txlang.rkt")

(define pasadas 0)
(define fallidas 0)

;; Corre el programa y, si aborta, devuelve el mensaje precedido de ERROR.
(define intentar
  (lambda (texto)
    (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
      (correr texto))))

(define empieza-con-error?
  (lambda (texto)
    (and (>= (string-length texto) 5) (string=? (substring texto 0 5) "ERROR"))))

(define verificar
  (lambda (etiqueta obtenido esperado)
    (if (string=? obtenido esperado)
        (begin (set! pasadas (+ pasadas 1))
               (eopl:printf "  ok    ~a~%" etiqueta))
        (begin (set! fallidas (+ fallidas 1))
               (eopl:printf "  FALLA ~a: se esperaba ~s y llegó ~s~%"
                            etiqueta esperado obtenido)))))

;; El programa devuelve el texto esperado.
(define prueba
  (lambda (etiqueta texto esperado)
    (verificar etiqueta (intentar texto) esperado)))

;; El chequeador asigna al programa el tipo esperado.
(define prueba-tipo
  (lambda (etiqueta texto esperado)
    (verificar etiqueta
               (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
                 (tipo-del-programa texto))
               esperado)))

;; El programa se rechaza, por el chequeador o por el intérprete.
(define prueba-rechazo
  (lambda (etiqueta texto)
    (let ((r (intentar texto)))
      (if (empieza-con-error? r)
          (begin (set! pasadas (+ pasadas 1))
                 (eopl:printf "  ok    ~a  [~a]~%" etiqueta r))
          (begin (set! fallidas (+ fallidas 1))
                 (eopl:printf "  FALLA ~a: se esperaba un rechazo y llegó ~s~%" etiqueta r))))))

;; unparse del árbol de un texto vuelve a parsear al mismo árbol.
(define ida-y-vuelta
  (lambda (etiqueta texto)
    (let ((una (unparse (scan&parse texto))))
      (verificar etiqueta (unparse (scan&parse una)) una))))

;;; ==========================================================
;;; Primera entrega: léxico, gramática, unparse y núcleo
;;; ==========================================================

(eopl:printf "~%== Primera entrega ==~%")

;; unparse y volver a analizar da un árbol igual al de partida.
(define ida-y-vuelta-arbol
  (lambda (etiqueta texto)
    (verificar etiqueta
               (if (equal? (scan&parse texto) (scan&parse (unparse (scan&parse texto))))
                   "arbol igual"
                   "arbol distinto")
               "arbol igual")))

;; --- Literales y comentarios
(prueba "literal entero" "42" "42")
(prueba "entero negativo" "-7" "-7")
(prueba "flotante" "3.14" "3.14")
(prueba "cadena" "\"hola\"" "\"hola\"")
(prueba "caracter" "'a'" "'a'")
(prueba "booleano y unit" "begin true; false; unit end" "unit")
(prueba "comentario de linea" "// un comentario\n42" "42")
(prueba "comentario de bloque" "#| varias\n lineas |# 42" "42")

;; --- unparse
(ida-y-vuelta "unparse de un literal" "42")
(verificar "unparse escribe el texto canonico"
           (unparse (scan&parse "let a=1,b=2 in +(a,b) end"))
           "let a = 1, b = 2 in +(a, b) end")
(ida-y-vuelta-arbol "arbol estable con if y while"
  "if true then while false do 1 end elif false then 2 else 3 end")
(ida-y-vuelta-arbol "arbol estable con letrec y proc"
  "letrec int f (n: int) = apply f (n) in apply proc (g: (int, bool -> ref(int))) g end (f) end")
(ida-y-vuelta-arbol "arbol estable con atomic, record y field"
  "record Cuenta { titular: string, saldo: ref(int) }
   let c = new Cuenta { titular = \"Ana\", saldo = ref(1) } in
     atomic set field(c, saldo) := 2 onfail (m) print(m) end
   end")

;; --- let, ref/deref/set y begin
(prueba "let y suma" "let a = 1, b = 2 in +(a, b) end" "3")
(prueba "las ligaduras de un let no se ven entre si"
        "let a = 1 in let a = 2, b = a in b end end" "1")
(prueba "una celda" "let c = ref(0) in begin set c := 5; deref(c) end end" "5")
(prueba "dos nombres para una celda"
        "let c = ref(1) in let d = c in begin set d := 9; deref(c) end end end" "9")
(prueba "begin devuelve el ultimo" "begin 1; 2; 3 end" "3")

;; --- Condicional y ciclo
(prueba "if con elif" "if false then 1 elif false then 2 elif true then 3 else 4 end" "3")
(prueba-rechazo "if con prueba no booleana" "if 1 then 2 else 3 end")
(prueba-rechazo "if sin else" "if true then 1 end")
(prueba "while suma 0..4"
        "let i = ref(0), s = ref(0) in
           begin
             while <(deref(i), 5) do
               begin set s := +(deref(s), deref(i)); set i := +(deref(i), 1) end
             end;
             deref(s)
           end
         end"
        "10")

;; --- Primitivas
(prueba "aritmetica entera" "-(10, *(2, 3))" "4")
(prueba "division entera" "/(7, 2)" "3")
(prueba "modulo" "%(7, 2)" "1")
(prueba "aritmetica flotante" "+(1.5, 2.25)" "3.75")
(prueba-rechazo "suma de entero y flotante" "+(1, 2.0)")
(prueba-rechazo "modulo de flotantes" "%(7.0, 2.0)")
(prueba-rechazo "division por cero" "/(1, 0)")
(prueba "relacionales" "begin <(1, 2); >=(2, 3) end" "false")
(prueba "igualdad de cadenas" "==(\"a\", \"a\")" "true")
(prueba "desigualdad" "!=(1, 2)" "true")
(prueba "booleanas" "and(true, not(false))" "true")
(prueba-rechazo "and con un entero" "and(true, 1)")
(prueba "concat" "concat(\"ab\", \"cd\")" "\"abcd\"")
(prueba "length" "length(\"hola\")" "4")
(prueba "to-string" "to-string(12)" "\"12\"")
(prueba "to-number entero" "to-number(\"41\")" "41")
(prueba "to-number flotante" "to-number(\"2.5\")" "2.5")
(prueba-rechazo "to-number de texto no numerico" "to-number(\"abc\")")
(prueba "print devuelve unit" "print(\"hola desde TxLang-UV\")" "unit")

;; --- Procedimientos y clausuras
(prueba "proc y apply" "apply proc (x: int, y: int) +(x, y) end (3, 4)" "7")
(prueba "alcance lexico"
        "let x = 10 in
           let f = proc (y: int) +(x, y) end in
             let x = 100 in apply f (1) end
           end
         end"
        "11")
(prueba "procedimiento de orden superior"
        "let dos-veces = proc (f: (int -> int), v: int) apply f (apply f (v)) end
         in apply dos-veces (proc (n: int) *(n, 2) end, 5) end"
        "20")
(prueba "referencia como argumento"
        "let c = ref(10), inc = proc (r: ref(int)) set r := +(deref(r), 1) end
         in begin apply inc (c); apply inc (c); deref(c) end end"
        "12")
(prueba-rechazo "aridad equivocada" "apply proc (x: int) x end (1, 2)")
(prueba-rechazo "apply de un no procedimiento" "apply 3 (1)")

;; --- letrec y recursión mutua
(prueba "factorial con letrec"
        "letrec int fact (n: int) =
                  if ==(n, 0) then 1 else *(n, apply fact (-(n, 1))) end
         in apply fact (6) end"
        "720")
(prueba "recursion mutua par/impar"
        "letrec
           bool par (n: int) = if ==(n, 0) then true else apply impar (-(n, 1)) end
           bool impar (n: int) = if ==(n, 0) then false else apply par (-(n, 1)) end
         in apply par (10) end"
        "true")

;; --- Errores de ejecución
(prueba-rechazo "variable no ligada" "x")
(prueba-rechazo "deref de un no referencia" "deref(3)")
(prueba-rechazo "set sobre un no referencia" "set 3 := 4")

;; --- Lo que llega en la segunda entrega se analiza pero no se evalúa todavía
(prueba-rechazo "atomic aun no se evalua" "atomic 1 onfail (m) 2 end")

;;; ==========================================================
;;; Segunda entrega: transacciones, registros y tipos
;;; ==========================================================

(eopl:printf "~%== Segunda entrega ==~%")

;; Agregue aquí las pruebas de la segunda entrega: bloques atomic que
;; confirman y que revierten, anidamiento, registros, y al menos cinco
;; programas mal tipados con prueba-rechazo o prueba-tipo. Por ejemplo:
;;
;; (prueba-tipo "tipo de un proc" "proc (x: int) x end" "(int -> int)")
;; (prueba-rechazo "suma de int y bool" "+(1, true)")

;;; ==========================================================
;;; Resumen
;;; ==========================================================

(eopl:printf "~%==========================================~%")
(eopl:printf "  pasadas ~s, fallidas ~s~%" pasadas fallidas)
(eopl:printf "==========================================~%")
