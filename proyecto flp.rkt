#lang eopl
;Diego Armando Espinosa Ossa 201942206
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
   (letter (arbno (or letter digit "?" "-"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  (number
    (digit (arbno digit) "." digit (arbno digit) )number)
   (number
    ("-" digit (arbno digit) "." digit (arbno digit) )number)
   (string
    ("\"" (arbno (not #\")) "\"") string)
   ))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (main-exp) a-program)
    (expression (number) numero)
    (expression (identifier call-or-var) var-or-call-exp)
    (call-or-var ("(" (separated-list expression ",") ")") call-suffix)
    (call-or-var () var-suffix)
    (expression ("set" identifier "=" expression)
            assign-exp)
    (expression ("print" expression) print-exp)
    (expression
     (primitivaArit "(" (separated-list expression ",") ")")
     primapp-exp)
    (expression
     (primitivaLista "(" (separated-list expression ",") ")")
     listapp-exp)
    (expression ("[" (separated-list expression ",") "]") list-literal-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression (exp-bool) bool-exp)



    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" expression) 
                letrec-exp)
    (main-exp ("$" (arbno sentence) expression (arbno ";" expression) "end")
                begin-exp)
    (sentence ("var" identifier "=" expression (arbno ";" identifier "=" expression))
                define-var)
    (sentence ("const" identifier "=" expression (arbno ";" identifier "=" expression))
                const)
    (sentence ("func" identifier "(" (separated-list identifier ",") ")" "{" (arbno expression ";") "return" expression "}")
                func-decl)
    
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
    (primitivaArit ("sub1") decr-prim)
    (primitivaLista ("vacio?") vacio?-prim)
    (primitivaLista ("vacio") vacio-prim)
    (primitivaLista ("crear-lista") crear-lista-prim)
    (primitivaLista ("lista?") lista?-prim)
    (primitivaLista ("cabeza") cabeza-prim)
    (primitivaLista ("cola") cola-prim)
    (primitivaLista ("append") append-prim)
    (primitivaLista ("ref-list") ref-list-prim)
    (primitivaLista ("set-list") set-list-prim)))

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
      (var-or-call-exp (id suffix)
                       (cases call-or-var suffix
                         (var-suffix ()
                                     (let ((ref (apply-env-ref env id)))
                                       (deref ref)))
                         (call-suffix (rands)
                                      (let ((proc (let ((ref (apply-env-ref env id)))
                                                    (deref ref)))
                                            (args (eval-rands rands env)))
                                        (if (procval? proc)
                                            (apply-procedure proc args)
                                            (eopl:error 'eval-expression
                                                        "Attempt to apply non-procedure ~s" proc))))))
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
                 (let ((val (eval-expression arg env)))
                   (begin
                     (display (cond
                                ((procval? val) "<funcion>")
                                ((mutable-list? val) (mutable-list->string val))
                                (else val)))
                     '<--)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (bool-exp (bool-exp1)
                (eval-exp-bool bool-exp1 env))
      (listapp-exp (prim rands)
                   (let ((args (eval-primapp-exp-rands rands env)))
                     (apply-list-primitive prim args)))
      (list-literal-exp (exps)
                        (make-mutable-list (map (lambda (e) (eval-expression e env)) exps)))


      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))

      )
    ))

;; eval-exp-bool: <exp-bool> <env> -> boolean
;; Evalúa expresiones booleanas
(define eval-exp-bool
  (lambda (exp env)
    (cases exp-bool exp
      (pred-prim-exp (prim exp1 exp2)
                     (let ((val1 (eval-expression exp1 env))
                           (val2 (eval-expression exp2 env)))
                       (apply-pred-prim prim val1 val2)))
      (valor-verdad (bool-val)
                    (cases bool bool-val
                      (true-val () #t)
                      (false-val () #f)))
      (oper-bin-bool-exp (prim exp1 exp2)
                         (let ((val1 (eval-exp-bool exp1 env))
                               (val2 (eval-exp-bool exp2 env)))
                           (apply-oper-bin-bool prim val1 val2)))
      (oper-un-bool-exp (prim exp1)
                        (let ((val1 (eval-exp-bool exp1 env)))
                          (apply-oper-un-bool prim val1))))))

(define apply-pred-prim
  (lambda (prim val1 val2)
    (cases pred-prim prim
      (menor () (< val1 val2))
      (mayor () (> val1 val2))
      (menorIgual () (<= val1 val2))
      (mayorIgual () (>= val1 val2))
      (igual () (= val1 val2))
      (diferente () (not (= val1 val2))))))

(define apply-oper-bin-bool
  (lambda (prim val1 val2)
    (cases oper-bin-bool prim
      (and-exp () (and val1 val2))
      (or-exp () (or val1 val2)))))

(define apply-oper-un-bool
  (lambda (prim val1)
    (cases oper-un-bool prim
      (negacion () (not val1)))))

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
              (save-sen (cdr sen) new-env))))

      (func-decl (name params body-exps return-exp)
        ;; Crear un func-closure para la función y registrarla en el ambiente
        ;; con soporte de recursión: la función puede verse a sí misma
        ;; Se usa extend-env-recursively para que el nombre de la función
        ;; esté disponible en su propio ambiente
        (let ((new-env
               (extend-env-recursively
                (list name)
                (list params)
                (list return-exp)
                env)))
          ;; Actualizar el closure en el ambiente para que sea func-closure
          ;; con body-exps y el ambiente recursivo correcto
          (let ((ref (apply-env-ref new-env name)))
            (primitive-setref! ref
              (direct-target
               (func-closure params body-exps return-exp new-env))))
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
      (var-or-call-exp (id suffix)
                       (cases call-or-var suffix
                         (var-suffix ()
                                     (indirect-target
                                      (let ((ref (apply-env-ref env id)))
                                        (cases target (primitive-deref ref)
                                          (direct-target (expval) ref)
                                          (indirect-target (ref1) ref1)))))
                         (call-suffix (rands)
                                      (direct-target (let ((proc (let ((ref (apply-env-ref env id)))
                                                                    (deref ref)))
                                                            (args (eval-rands rands env)))
                                                        (if (procval? proc)
                                                            (apply-procedure proc args)
                                                            (eopl:error 'eval-expression
                                                                        "Attempt to apply non-procedure ~s" proc)))))))
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

;; apply-list-primitive: <primitivaLista> <list-of-values> -> value
;; Aplica una primitiva de listas a los argumentos dados
(define apply-list-primitive
  (lambda (prim args)
    (cases primitivaLista prim
      (vacio?-prim () (mutable-list-empty? (car args)))
      (vacio-prim () the-empty-list)
      (crear-lista-prim () (make-mutable-list args))
      (lista?-prim () (mutable-list? (car args)))
      (cabeza-prim () (mutable-list-head (car args)))
      (cola-prim () (mutable-list-tail (car args)))
      (append-prim () (mutable-list-append (car args) (cadr args)))
      (ref-list-prim () (mutable-list-ref (car args) (cadr args)))
      (set-list-prim () (mutable-list-set! (car args) (cadr args) (caddr args))))))

;; true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
;; Acepta: #t/#f (booleanos Racket), 0 (falso), cualquier otro número (verdadero)
(define true-value?
  (lambda (x)
    (cond
      ((boolean? x) x)
      ((number? x) (not (zero? x)))
      (else #f))))

;; procval: tipo de dato para valores de procedimiento
;; closure: procedimiento simple (proc) con ids, body y env
;; func-closure: procedimiento declarado con func, con body-exps (secuencia) y return-exp
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?))
  (func-closure
   (ids (list-of symbol?))
   (body-exps (list-of expression?))
   (return-exp expression?)
   (env environment?)))

;; mutable-list: tipo de dato para listas mutables
;; Implementada como vector de Racket para permitir mutación
(define-datatype mutable-list mutable-list?
  (a-mutable-list (vec vector?)))

;; Crea una lista mutable a partir de una lista de valores
(define make-mutable-list
  (lambda (vals)
    (a-mutable-list (list->vector vals))))

;; Lista vacía
(define the-empty-list
  (a-mutable-list (vector)))

;; Verifica si una lista mutable está vacía
(define mutable-list-empty?
  (lambda (lst)
    (cases mutable-list lst
      (a-mutable-list (vec)
                      (zero? (vector-length vec))))))

;; Cabeza de una lista mutable
(define mutable-list-head
  (lambda (lst)
    (cases mutable-list lst
      (a-mutable-list (vec)
                      (if (zero? (vector-length vec))
                          (eopl:error 'cabeza "Lista vacía")
                          (vector-ref vec 0))))))

;; Cola de una lista mutable (retorna nueva lista sin el primer elemento)
(define mutable-list-tail
  (lambda (lst)
    (cases mutable-list lst
      (a-mutable-list (vec)
        (if (zero? (vector-length vec))
            (eopl:error 'cola "Lista vacía")
            (let ((len (vector-length vec)))
              (let ((new-vec (make-vector (- len 1))))
                (begin
                  (let loop ((i 0))
                    (when (< i (- len 1))
                      (vector-set! new-vec i (vector-ref vec (+ i 1)))
                      (loop (+ i 1))))
                  (a-mutable-list new-vec)))))))))

;; Concatenación de listas mutables
(define mutable-list-append
  (lambda (lst1 lst2)
    (cases mutable-list lst1
      (a-mutable-list (vec1)
        (cases mutable-list lst2
          (a-mutable-list (vec2)
            (let ((len1 (vector-length vec1))
                  (len2 (vector-length vec2))
                  (new-vec (make-vector (+ (vector-length vec1) (vector-length vec2)))))
              (begin
                (let copy1 ((i 0))
                  (when (< i len1)
                    (vector-set! new-vec i (vector-ref vec1 i))
                    (copy1 (+ i 1))))
                (let copy2 ((i 0))
                  (when (< i len2)
                    (vector-set! new-vec (+ len1 i) (vector-ref vec2 i))
                    (copy2 (+ i 1))))
                (a-mutable-list new-vec)))))))))

;; Referencia a posición en lista mutable
(define mutable-list-ref
  (lambda (lst pos)
    (cases mutable-list lst
      (a-mutable-list (vec)
                      (if (or (< pos 0) (>= pos (vector-length vec)))
                          (eopl:error 'ref-list "Índice fuera de rango: ~s" pos)
                          (vector-ref vec pos))))))

;; Modificar posición en lista mutable
(define mutable-list-set!
  (lambda (lst pos val)
    (cases mutable-list lst
      (a-mutable-list (vec)
                      (if (or (< pos 0) (>= pos (vector-length vec)))
                          (eopl:error 'set-list "Índice fuera de rango: ~s" pos)
                          (begin
                            (vector-set! vec pos val)
                            'ok))))))

;; Convierte un valor a string para impresión
(define expval->string
  (lambda (v)
    (cond
      ((number? v) (number->string v))
      ((boolean? v) (if v "true" "false"))
      ((symbol? v) (symbol->string v))
      ((string? v) v)
      ((procval? v) "<funcion>")
      ((mutable-list? v) (mutable-list->string v))
      (else (symbol->string (car v))))))

;; Convierte una lista mutable a string para impresión
(define mutable-list->string
  (lambda (lst)
    (cases mutable-list lst
      (a-mutable-list (vec)
        (string-append
         "["
         (let loop ((i 0) (strs '()))
           (if (>= i (vector-length vec))
               (apply string-append
                      (let intersperse ((lst (reverse strs)) (first #t))
                        (if (null? lst)
                            '()
                            (if first
                                (cons (car lst) (intersperse (cdr lst) #f))
                                (cons ", " (cons (car lst) (intersperse (cdr lst) #f)))))))
               (loop (+ i 1)
                     (cons (expval->string (vector-ref vec i)) strs))))
         "]")))))

;; apply-procedure: evalua el cuerpo de un procedimiento en el ambiente extendido correspondiente
;; proc: closure o func-closure
;; args: lista de valores (direct-target)
;; Para closure: evalúa body directamente
;; Para func-closure: evalúa body-exps en secuencia, luego evalúa return-exp
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env
                                      ids
                                      args
                                      (make-list-of-n-smthing(length args) 'var)
                                      env )))
      (func-closure (ids body-exps return-exp env)
                    (let ((new-env (extend-env
                                    ids
                                    args
                                    (make-list-of-n-smthing(length args) 'var)
                                    env)))
                      ;; Evaluar las expresiones del body en secuencia (por efectos secundarios)
                      (for-each
                       (lambda (exp)
                         (eval-expression exp new-env))
                       body-exps)
                      ;; Evaluar y retornar la expresión de retorno
                      (eval-expression return-exp new-env))))))

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

;; extend-env-recursively: crea ambiente extendido para procedimientos recursivos
;; proc-names: lista de nombres de funciones
;; idss: lista de listas de parámetros
;; bodies: lista de expresiones (cuerpo/retorno de cada función)
;; old-env: ambiente previo
;; Las funciones se registran con label 'func para que apply-env-label las identifique
;; Primero crea el ambiente con closures simples, luego func-decl los reemplaza por func-closures
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec (make-list-of-n-smthing len 'func) old-env)))
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

;; expval?: determina si un valor es un valor expresado válido
;; Incluye: números, booleanos, strings, procedimientos (closures)
(define expval?
  (lambda (x)
    (or (number? x) (boolean? x) (string? x) (procval? x) (mutable-list? x))))

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