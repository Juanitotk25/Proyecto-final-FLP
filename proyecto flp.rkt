#lang eopl
;Diego Armando Espinosa Ossa 201942206
Juan David Lopez Vanegas 
;******************************************************************************************

;; ACLARACIONES IMPORTANTES
;; -En este lenguaje se declaran variables simbolos y constantes (sentencias)
;; antes de cualquier expression debido a que se produacian conflictos con los ambientes
;; que se extienden y que usa eval-expression.
;; -Solo se puede usar un print en cada ejecucion
;;;;; Interpretador para el lenguaje del proyecto

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;; <sentence> :: = var <identificador> = <exp>
;; | const <identificador> = <exp>
;; <exp> ::= <identificador>
;; |<numero>
;; | <cadena>
;; | <bool>
;; | null # representación de valor faltante

;; | <identificador> = <exp> # actualización
;; | func <identificador>({<identificador>}*) {
;; {<exp>}*
;; return <exp> }
;; | <identificador>(<args>) # invocacion
;; | begin {<exp>}+(;) end # secuenciación
;; | if <exp> then <exp> else <exp> end
;; | switch ....
;; | while <exp> do <exp> done
;; | for <id> in <exp> do <exp> done
;; | [ {<exp>} *(,) ] # listas
;; | { {<identificador>:<exp>} +(,) }#diccionarios

;; | symbol <id>
;;  <exp-bool>
;;  ::= <pred-prim>(<exp> , <exp>)
;;  ::= <oper-bin-bool>(<exp-bool>, <exp-bool>)
;;  ::= <bool>
;;  ::= <oper-un-bool>(<exp-bool>)

;;  <primitive> ::=
;; | simplificar(<exp>)
;; | evaluar(<exp>, {<id>=<exp>}*(,))
;; | +, -, *, %, /, add1, sub
;; | vacio?, vacio, crear-lista, lista?,cabeza, cola,append,ref-list,set-list. #primitivas sobre listas
;; |diccionario?, crear-diccionario, ref-diccionario, set-diccionario, claves, valores #primitivas sobre diccionarios

;;
;; <pred-prim> ::= < | > | <= | >= | == | <>
;; 
;******************************************************************************************

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("#" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit ))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  (number
    (digit (arbno digit) "." digit (arbno digit) )number)
   (number
    ("-" digit (arbno digit) "." digit (arbno digit) )number)
   (text
    ("\"" (arbno (not #\")) "\"") string)
   ))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (main-exp) a-program)
    (expression (number) numero)
    (expression (identifier) variable)
    (expression ("set" identifier "=" expression)
            assign-exp)
    (expression ("print" expression) print-exp)
    (expression (text) texto-exp)
    (expression ("true") true-exp)
    (expression ("false") false-exp)
    (expression ("null") null-exp)
    (expression (exp-bool) bool-oper-exp)
    (expression
     (primitivaArit "("expression")")
     primapp-exp)
    (expression ("if" expression "then" expression "else" expression "end")
                if-exp)
    (expression ("switch" expression "{" (arbno "case" expression ":" expression) "default" ":" expression "}")
                switch-exp)
    (expression ("proc" "(" (arbno identifier) ")" expression)
                proc-exp)
    (expression ( "(" expression (arbno expression) ")")
                app-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" expression) 
                letrec-exp)
    (main-exp ("$" (arbno sentence) expression (arbno ";" expression) "end")
                begin-exp)
    (sentence ("var" identifier "=" expression (arbno ";" identifier "=" expression))
                define-var)
    (sentence ("const" identifier "=" expression (arbno ";" identifier "=" expression))
                const)
    
    (bool ( "true"  )true-val)
    (bool ( "false" )false-val)
    (exp-bool (pred-prim "(" expression "," expression ")") pred-prim-exp)
    (exp-bool ( bool ) valor-verdad)
    (exp-bool (oper-bin-bool "("exp-bool "," exp-bool")" ) oper-bin-bool-exp)
    (exp-bool (oper-un-bool "("exp-bool")" ) oper-un-bool-exp)
    (pred-prim ("<")menor)
    (pred-prim (">")mayor)
    (pred-prim ("<=")menorIgual)
    (pred-prim (">=")mayorIgual)
    (pred-prim ("==")igual)
    (pred-prim ("<>")diferente)
    (oper-bin-bool ("and") and-exp )
    (oper-bin-bool ("or") or-exp )
    (oper-un-bool ("not") negacion )
    (primitivaArit ("+") add-prim)
    (primitivaArit ("-") substract-prim)
    (primitivaArit ("*") mult-prim)
    (primitivaArit ("/") div-prim)
    (primitivaArit ("%") mod-prim)
    (primitivaArit ("add1") incr-prim)
    (primitivaArit ("sub1") decr-prim)))

;Tipos de datos para la sintaxis abstracta de la gramática

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (main body (init-env))))))

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))

(define init-env
  (lambda ()
     (empty-env)))

;(define init-env
;  (lambda ()
;    (
;     '(x y z f)
;     (list 4 2 5 (closure '(y) (primapp-exp (mult-prim) (cons (var-exp 'y) (cons (primapp-exp (decr-prim) (cons (var-exp 'y) ())) ())))
;                      (empty-env)))
;     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada

;**************************************************************************************
;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;**************************************************************************************

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (numero (datum) datum)
      (variable (id)
                (let ((ref (apply-env-ref env id)))
                  (deref ref)))
      (assign-exp (id rhs)
                  (let ((lab (apply-env-label env id)))
                    (cond
                      ((eqv? lab 'const)
                       (eopl:error 'set
                                   "No se puede modificar constante ~s" id))
                      
                      (else
                       (let ((ref (apply-env-ref env id))
                             (val (eval-expression rhs env)))
                         (setref! ref val)
                         'ok)))))
      (primapp-exp (prim rands)
                   (let ((args (eval-primapp-exp-rands rands env)))
                     (apply-primitive prim args)))
      (print-exp (arg)
                 (begin
                   (mathflow-display (eval-expression arg env))
                   '<--))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (switch-exp (test-exp cases-exps bodies-exps default-exp)
                  (let ((val (eval-expression test-exp env)))
                    (let loop ((cases cases-exps)
                               (bodies bodies-exps))
                      (if (null? cases)
                          (eval-expression default-exp env)
                          (if (equal? val (eval-expression (car cases) env))
                              (eval-expression (car bodies) env)
                              (loop (cdr cases) (cdr bodies)))))))
      (proc-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      (texto-exp (txt)
                 (substring txt 1 (- (string-length txt) 1)))
      (true-exp () #t)
      (false-exp () #f)
      (null-exp () 'null-val)
      (bool-oper-exp (expB) (eval-exp-bool expB env))
      )
    ))

;eval-exp-bool: <exp-bool> <enviroment> -> boolean
(define eval-exp-bool
  (lambda (expB env)
    (cases exp-bool expB
      (pred-prim-exp (prim exp1 exp2)
        (let ((val1 (eval-expression exp1 env))
              (val2 (eval-expression exp2 env)))
          (apply-pred-prim prim val1 val2)))
      (valor-verdad (b)
        (cases bool b
          (true-val () #t)
          (false-val () #f)))
      (oper-bin-bool-exp (oper expB1 expB2)
        (let ((val1 (eval-exp-bool expB1 env))
              (val2 (eval-exp-bool expB2 env)))
          (apply-oper-bin-bool oper val1 val2)))
      (oper-un-bool-exp (oper expB1)
        (let ((val1 (eval-exp-bool expB1 env)))
          (apply-oper-un-bool oper val1))))))

;apply-pred-prim: <pred-prim> <val> <val> -> boolean
(define apply-pred-prim
  (lambda (prim val1 val2)
    (cases pred-prim prim
      (menor () (< val1 val2))
      (mayor () (> val1 val2))
      (menorIgual () (<= val1 val2))
      (mayorIgual () (>= val1 val2))
      (igual () (equal? val1 val2))
      (diferente () (not (equal? val1 val2))))))

;apply-oper-bin-bool: <oper-bin-bool> <val> <val> -> boolean
(define apply-oper-bin-bool
  (lambda (oper val1 val2)
    (cases oper-bin-bool oper
      (and-exp () (and val1 val2))
      (or-exp () (or val1 val2)))))

;apply-oper-un-bool: <oper-un-bool> <val> -> boolean
(define apply-oper-un-bool
  (lambda (oper val)
    (cases oper-un-bool oper
      (negacion () (not val)))))

;  $ var asa = 123; x = 345; y = 567 print asa end;
(define main
  (lambda (exp env)
    (cases main-exp exp
      (begin-exp (sen exp exps)
        (let ((newenv (save-sen sen env)))
          (let ((env3 (begin
                        (eval-expression exp newenv)
                        newenv)))

            (let loop ((es exps)
                       (env2 env3)
                       (last-val 'ok))
              (if (null? es)
                  last-val
                  (let ((v (eval-expression (car es) env2)))
                    (loop (cdr es) env2 v))))))))))

(define save-sen
  (lambda (sen env)
    (cases sentence (car sen)

      (define-var (id rhs ids rhss)
        (let ((new-env
               (extend-env
                (cons id ids)
                (eval-def-exp-rands (cons rhs rhss) env)
                (make-list-of-n-smthing (length (cons rhs rhss)) 'var)
                env)))
          (if (null? (cdr sen))
              new-env
              (save-sen (cdr sen) new-env))))

      (const (id rhs ids rhss)
        (let ((new-env
               (extend-env
                (cons id ids)
                (eval-def-exp-rands (cons rhs rhss) env)
                (make-list-of-n-smthing (length (cons rhs rhss)) 'const)
                env)))
          (if (null? (cdr sen))
              new-env
              (save-sen (cdr sen) new-env)))))))

(define make-list-of-n-smthing
  (lambda(n smthing)
    (if (eqv? n 1)
        (list smthing)
        (cons smthing (make-list-of-n-smthing  (- n 1)smthing )))
    )
  )


; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (cases expression rand
      (variable (id)
               (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (eval-expression rand env))))))

(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expression x env)) rands)))

(define eval-def-exp-rands
  (lambda (rands env)
    (map (lambda (x)
           (direct-target (eval-expression x env)))
         rands)))


;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitivaArit prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (div-prim () (/ (car args) (cadr args)))
      (mod-prim () (modulo  (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1)))))

;mathflow-display: muestra valores de MathFlow en formato adecuado
(define mathflow-display
  (lambda (val)
    (cond
      ((boolean? val) (display (if val "true" "false")))
      ((eqv? val 'null-val) (display "null"))
      (else (display val)))))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
; Semantica dinamica: false, 0, "", null son falsos. Todo lo demas es verdadero.
(define true-value?
  (lambda (x)
    (cond
      ((boolean? x) x)
      ((number? x) (not (zero? x)))
      ((string? x) (not (string=? x "")))
      ((eqv? x 'null-val) #f)
      (else #t))))

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env
                                      ids
                                      args
                                      (make-list-of-n-smthing(length args) 'var)
                                      env ))))))

;*******************************************************************************************
;Ambientes
(define apply-env-label
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env-label
                    "Variable no definida: ~s"
                    sym))

      (extended-env-record (syms vals labels old-env)
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
              (list-ref labels pos)
              (apply-env-label old-env sym)))))))
;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (label (list-of symbol?))
   (env environment?)
   ))
(define scheme-value? (lambda (v) #t))
;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals labs env)
    (extended-env-record
     syms
     (list->vector vals)
     labs
     env)))

;-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (direct-target (closure ids body env))))
            (iota len) idss bodies)
          env)))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    ;(begin
     ; (display env)
      ;(display "jajajaj ")
      (deref (apply-env-ref env sym))))
    ;)
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals labs env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;*******************************************************************************************
;Blancos y Referencias

(define expval?
  (lambda (x)
    (or (number? x) (procval? x) (boolean? x) (string? x) (eqv? x 'null-val))))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1)))))))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))
(interpretador)
;$ var x = 10 set x = 20; print x end
;$ const x = 10 set x = 20; print x end
;$ var x = 10; y = 234; asa = 159 set asa = 20; print asa end

;; ========== Ejemplos Paso 1: Tipos base (cadenas, booleanos, null) ==========

;; --- Cadenas de texto ---
;; (scan&parse "$ var nombre = \"Robinson\" print nombre end")
;; (scan&parse "$ var saludo = \"Hola mundo\" print saludo end")

;; --- Booleanos ---
;; (scan&parse "$ var activo = true print activo end")
;; (scan&parse "$ var inactivo = false print inactivo end")

;; --- Null ---
;; (scan&parse "$ var vacio = null print vacio end")

;; --- Combinacion de tipos con var y const ---
;; (scan&parse "$ var x = 42; nombre = \"Juan\"; activo = true print nombre end")
;; (scan&parse "$ const pi = 3.14; mensaje = \"hola\" print mensaje end")

;; --- Semantica dinamica de true-value? ---
;; false, 0, "", null son falsos. Todo lo demas es verdadero.
;; (scan&parse "$ var x = 0 if x then print 1 else print 0 end")
;; (scan&parse "$ var x = false if x then print 1 else print 0 end")
;; (scan&parse "$ var x = null if x then print 1 else print 0 end")
;; (scan&parse "$ var x = 5 if x then print 1 else print 0 end")

;; ========== Ejemplos Paso 2: Expresiones Booleanas ==========

;; --- Predicados primitivos ---
;; (scan&parse "$ var a = <(3, 5) print a end")
;; (scan&parse "$ var a = >(10, 5) print a end")
;; (scan&parse "$ var a = <=(5, 5) print a end")
;; (scan&parse "$ var a = ==(4, 4) print a end")
;; (scan&parse "$ var a = <>(4, 5) print a end")

;; --- Operadores booleanos compuestos ---
;; (scan&parse "$ var a = and(<(3,5), >(10,2)) print a end")
;; (scan&parse "$ var a = or(<(5,3), >(10,2)) print a end")
;; (scan&parse "$ var a = not(==(3,3)) print a end")

;; --- Uso en condicionales ---
;; (scan&parse "$ var x = 10; y = 20 if <(x, y) then print \"Menor\" else print \"Mayor\" end end")

;; ========== Ejemplos Paso 3: Condicionales y Switch ==========

;; --- Condicional IF con end ---
;; (scan&parse "$ var edad = 20 if >=(edad, 18) then print \"Mayor\" else print \"Menor\" end end")

;; --- Switch ---
;; (scan&parse "$ var color = \"verde\" switch color { case \"rojo\": print \"Detente\" case \"verde\": print \"Sigue\" default: print \"Desconocido\" } end")