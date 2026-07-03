#lang eopl
#| Diego Armando Espinosa Ossa 201942206
  Juan David Lopez Vanegas
|#
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
   (simbolo-token
    ("'" letter (arbno (or letter digit))) symbol)
   ))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (main-exp) a-program)
    (expression (number) numero)
    (expression ("call" identifier "(" (separated-list expression ",") ")") math-app-exp)
    (expression (identifier) variable)
    (expression ("set" identifier "=" expression)
            assign-exp)
    (expression ("print" expression) print-exp)
    (expression (text) texto-exp)
    (expression ("null") null-exp)
    (expression (simbolo-token) simbolo-exp)
    (expression (exp-bool) bool-oper-exp)
    (expression
     (primitivaArit "(" (separated-list expression ",") ")")
     primapp-exp)
    (expression ("if" expression "then" expression "else" expression "end")
                if-exp)
    (expression ("switch" expression "{" (arbno "case" expression ":" expression) "default" ":" expression "}")
                switch-exp)
    (expression ("proc" "(" (arbno identifier) ")" expression)
                proc-exp)
    
    (expression ("--func-body--" (arbno expression) "return" expression)
                func-body-exp)
    (expression ("while" expression "do" expression "done")
                while-exp)
    (expression ("for" identifier "in" expression "do" expression "done")
                for-exp)
    (expression ("[" (separated-list expression ",") "]")
                lista-literal-exp)
    (expression ("{" (separated-list identifier ":" expression ",") "}")
                dicc-literal-exp)
    (expression ("evaluar" "(" expression "," (separated-list identifier "=" expression ",") ")")
                evaluar-exp)
    (expression ("simplificar" "(" expression ")")
                simplificar-exp-sym)
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-struct-exp)
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
    (sentence ("symbol" identifier)
                symbol-sentence)
    (sentence ("func" identifier "(" (separated-list identifier ",") ")" "{" (arbno expression) "return" expression "}")
                func-sentence)
    
    
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
    ;; ---------- LIST PRIMITIVES ----------
      (primitivaArit ("vacio") vacio-prim)
      (primitivaArit ("vacio?") vacio-pred-prim)
      (primitivaArit ("crear-lista") crear-lista-prim)
      (primitivaArit ("lista?") lista-pred-prim)
      (primitivaArit ("cabeza") cabeza-prim)
      (primitivaArit ("cola") cola-prim)
      (primitivaArit ("append") append-prim)
      (primitivaArit ("ref-list") ref-list-prim)
      (primitivaArit ("set-list") set-list-prim)
      ;; ---------- DICT PRIMITIVES ----------
      (primitivaArit ("crear-diccionario") crear-diccionario-prim)
      (primitivaArit ("diccionario?") diccionario-pred-prim)
      (primitivaArit ("ref-diccionario") ref-diccionario-prim)
      (primitivaArit ("set-diccionario") set-diccionario-prim)
      (primitivaArit ("claves") claves-prim)
      (primitivaArit ("valores") valores-prim)
      ;; ---------- STRINGS PRIMITIVES ----------
      (primitivaArit ("longitud") longitud-prim)
      (primitivaArit ("concatenar") concatenar-prim)
      (primitivaArit ("buscar") buscar-prim)
      ))
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
                (if (eq? id 'vacio)
                    'vacio
                    (let ((ref (apply-env-ref env id)))
                      (deref ref))))
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
      (while-exp (test-exp body-exp)
                 (let loop ()
                   (if (true-value? (eval-expression test-exp env))
                       (begin
                         (eval-expression body-exp env)
                         (loop))
                       'ok)))
      (for-exp (id iterable body-exp)
               (let ((val (eval-expression iterable env)))
                 ;; Acepta tanto la lista vacía de Racket como el valor MathFlow `vacio`.
                 (if (or (list? val) (eq? val 'vacio))
                     (let loop ((lst val))
                       (if (null? lst)
                           'ok
                           (begin
                             (eval-expression body-exp (extend-env (list id) (list (car lst)) (list 'var) env))
                             (loop (cdr lst)))))
                     (eopl:error 'eval-expression "El iterador de for debe ser una lista, se obtuvo: ~s" val))))
      (proc-exp (ids body)
                (closure ids body env))
      (math-app-exp (id rands)
               (let ((proc (eval-expression (variable id) env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (func-body-exp (body-exps ret-exp)
                     (let loop ((exps body-exps))
                       (if (null? exps)
                           (eval-expression ret-exp env)
                           (begin
                             (eval-expression (car exps) env)
                             (loop (cdr exps))))))
      (lista-literal-exp (exps)
         (map (lambda (e) (eval-expression e env)) exps))
      (dicc-literal-exp (ids exps)
         (let loop ((is ids) (es exps))
            (if (null? is)
                '()
                (cons (cons (symbol->string (car is)) (eval-expression (car es) env))
                      (loop (cdr is) (cdr es))))))
      (simplificar-exp-sym (expr)
         (simplificar-exp (eval-expression expr env)))
      (evaluar-exp (expr ids vals)
         (let ((expr-val (eval-expression expr env))
               (vals-eval (map (lambda (v) (eval-expression v env)) vals)))
            (let loop ((e expr-val) (is ids) (vs vals-eval))
               (if (null? is)
                   (simplificar-exp e)
                   (loop (sustituir-simbolo e (car is) (car vs))
                         (cdr is) (cdr vs))))))
      (begin-struct-exp (exp exps)
                        (let loop ((val (eval-expression exp env))
                                   (resto exps))
                          (if (null? resto)
                              val
                              (loop (eval-expression (car resto) env)
                                    (cdr resto)))))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      (texto-exp (txt)
                 (substring txt 1 (- (string-length txt) 1)))
      (simbolo-exp (sym) sym)
      
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
              (save-sen (cdr sen) new-env))))
              
      (symbol-sentence (id)
        (let ((new-env (extend-env (list id) (list (list 'simbolico id)) (list 'const) env)))
          (if (null? (cdr sen))
              new-env
              (save-sen (cdr sen) new-env))))
      
      (func-sentence (id ids body-exps ret-exp)
        (let ((new-env (extend-env-recursively (list id) (list ids) (list (func-body-exp body-exps ret-exp)) env)))
          (if (null? (cdr sen))
              new-env
              (save-sen (cdr sen) new-env))))
              
      
      )))

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
    (eval-expression rand env)))

(define eval-def-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-def-exp-rand x env))
         rands)))

(define eval-def-exp-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expression x env))
         rands)))

;todos-numericos?: verifica si todos los elementos de una lista son numeros
(define (todos-numericos? lst)
  (cond
    ((null? lst) #t)
    ((number? (car lst)) (todos-numericos? (cdr lst)))
    (else #f)))

;reemplazar-en-lista: reemplaza el elemento en la posicion idx
(define (reemplazar-en-lista lst idx val)
  (cond
    ((null? lst) '())
    ((zero? idx) (cons val (cdr lst)))
    (else (cons (car lst) (reemplazar-en-lista (cdr lst) (- idx 1) val)))))

;es-diccionario?: verifica si un valor es una lista de pares (llave . valor)
(define (es-diccionario? val)
  (and (list? val)
       (not (null? val))
       (pair? (car val))))

;buscar-en-dict: busca una llave en el diccionario
(define (buscar-en-dict dict llave)
  (cond
    ((null? dict) 'null-val)
    ((equal? (caar dict) llave) (cdar dict))
    (else (buscar-en-dict (cdr dict) llave))))

;actualizar-dict: actualiza o agrega un par llave-valor
(define (actualizar-dict dict llave valor)
  (cond
    ((null? dict) (list (cons llave valor)))
    ((equal? (caar dict) llave) (cons (cons llave valor) (cdr dict)))
    (else (cons (car dict) (actualizar-dict (cdr dict) llave valor)))))

;buscar-subcadena: verifica si sub aparece dentro de str
(define (buscar-subcadena str sub)
  (let ((len-str (string-length str))
        (len-sub (string-length sub)))
    (if (> len-sub len-str) #f
        (let loop ((i 0))
          (cond
            ((> (+ i len-sub) len-str) #f)
            ((string=? (substring str i (+ i len-sub)) sub) #t)
            (else (loop (+ i 1))))))))

;helpers para algebra simbolica
(define (es-simbolico? val)
  (or (symbol? val)
      (and (pair? val) (eq? (car val) 'simbolico))))

(define (alguno-simbolico? args)
  (cond
    ((null? args) #f)
    ((es-simbolico? (car args)) #t)
    (else (alguno-simbolico? (cdr args)))))

(define (crear-exp-simbolica prim args)
  (cons 'simbolico (cons prim args)))

(define (primitiva-aritmetica? prim)
  (cases primitivaArit prim
    (add-prim () #t)
    (substract-prim () #t)
    (mult-prim () #t)
    (div-prim () #t)
    (mod-prim () #t)
    (incr-prim () #t)
    (decr-prim () #t)
    (else #f)))

(define prim-name
  (lambda (prim)
    (cases primitivaArit prim
      (add-prim () "+")
      (substract-prim () "-")
      (mult-prim () "*")
      (div-prim () "/")
      (mod-prim () "%")
      (incr-prim () "add1")
      (decr-prim () "sub1")
      (else "?"))))

;apply-primitive: <primitiva> <list-of-expression> -> valor
(define apply-primitive
  (lambda (prim args)
    (if (and (primitiva-aritmetica? prim) (alguno-simbolico? args))
        (crear-exp-simbolica prim args)
        (cases primitivaArit prim
          (add-prim () (+ (car args) (cadr args)))
          (substract-prim () (- (car args) (cadr args)))
          (mult-prim () (* (car args) (cadr args)))
          (div-prim () (/ (car args) (cadr args)))
          (mod-prim () (modulo (car args) (cadr args)))
          (incr-prim () (+ (car args) 1))
          (decr-prim () (- (car args) 1))
          ;primitivas de listas
          (vacio-prim () 'vacio)
      (vacio-pred-prim ()
        (let ((lst (car args)))
          (cond
            ((eq? lst 'vacio) #t)
            ((null? lst) #t)
            (else #f))))
      (crear-lista-prim ()
        (let ((elem (car args)) (lst (cadr args)))
          (cons elem (if (eq? lst 'vacio) '() lst))))
      (lista-pred-prim ()
        (let ((lst (car args)))
          (or (list? lst) (eq? lst 'vacio))))
      (cabeza-prim ()
        (let ((lst (car args)))
          (if (eq? lst 'vacio)
              (eopl:error 'cabeza "Lista vacía")
              (car (if (null? lst) '() lst)))))
      (cola-prim ()
        (let ((lst (car args)))
          (if (eq? lst 'vacio)
              (eopl:error 'cola "Lista vacía")
              (cdr (if (null? lst) '() lst)))))
      (append-prim ()
        (let ((lst1 (car args)) (lst2 (cadr args)))
          (append (if (eq? lst1 'vacio) '() lst1)
                  (if (eq? lst2 'vacio) '() lst2))))
      (ref-list-prim ()
        (let ((lst (car args)) (idx (cadr args)))
          (if (eq? lst 'vacio)
              (eopl:error 'ref-list "Lista vacía")
              (list-ref (if (null? lst) '() lst) idx))))
      (set-list-prim ()
        (let ((lst (car args)) (idx (cadr args)) (val (caddr args)))
          (if (eq? lst 'vacio)
              (eopl:error 'set-list "Lista vacía")
              (reemplazar-en-lista (if (null? lst) '() lst) idx val))))
      ;primitivas de diccionarios (listas de asociacion)
      (crear-diccionario-prim ()
        (let loop ((a args))
          (if (null? a)
              '()
              (if (null? (cdr a))
                  (eopl:error 'crear-diccionario "Falta valor para la llave ~s" (car a))
                  (cons (cons (car a) (cadr a))
                        (loop (cddr a)))))))
      (diccionario-pred-prim ()
        (es-diccionario? (car args)))
      (ref-diccionario-prim ()
        (buscar-en-dict (car args) (cadr args)))
      (set-diccionario-prim ()
        (actualizar-dict (car args) (cadr args) (caddr args)))
      (claves-prim ()
        (map car (car args)))
      (valores-prim ()
        (map cdr (car args)))
      ;primitivas de cadenas
      (longitud-prim ()
        (string-length (car args)))
      (concatenar-prim ()
        (string-append (car args) (cadr args)))
      (buscar-prim ()
        (buscar-subcadena (car args) (cadr args)))
      )))

;simplificar-exp: recorre recursivamente el arbol
;y aplica las reglas de simplificacion
(define (simplificar-exp expr)
  (cond
    ;; si no es una expresion simbolica, devolver tal cual
    ((not (and (pair? expr) (eq? (car expr) 'simbolico)))
     expr)
    ;; es simbolica: primero simplificar los hijos recursivamente
    (else
     (let* ((prim (cadr expr))
            (args-raw (cddr expr))
            ;; simplificamos cada subexpresion primero
            (args (map simplificar-exp args-raw)))
       ;si todos los argumentos ya son numeros, evaluar la operacion
       (if (todos-numericos? args)
           (apply-primitive prim args)
           ;si no, intentar simplificar con reglas algebraicas
           (simplificar-reglas prim args))))))

;simplificar-reglas: aplica identidades algebraicas
(define (simplificar-reglas prim args)
  (cases primitivaArit prim
    ;; x + 0 = x, 0 + x = x
    (add-prim ()
      (let ((a (car args)) (b (cadr args)))
        (cond
          ((and (number? b) (zero? b)) a)
          ((and (number? a) (zero? a)) b)
          (else (cons 'simbolico (cons prim args))))))
    ;; x - 0 = x
    (substract-prim ()
      (let ((a (car args)) (b (cadr args)))
        (cond
          ((and (number? b) (zero? b)) a)
          ;; x - x = 0 (si son el mismo simbolo)
          ((equal? a b) 0)
          (else (cons 'simbolico (cons prim args))))))
    ;; x * 0 = 0, 0 * x = 0, x * 1 = x, 1 * x = x
    (mult-prim ()
      (let ((a (car args)) (b (cadr args)))
        (cond
          ((and (number? a) (zero? a)) 0)
          ((and (number? b) (zero? b)) 0)
          ((and (number? b) (= b 1)) a)
          ((and (number? a) (= a 1)) b)
          (else (cons 'simbolico (cons prim args))))))
    ;; x / 1 = x
    (div-prim ()
      (let ((a (car args)) (b (cadr args)))
        (cond
          ((and (number? b) (= b 1)) a)
          ;; x / x = 1
          ((equal? a b) 1)
          (else (cons 'simbolico (cons prim args))))))
    ;; para las demas primitivas no hay regla especial
    (else (cons 'simbolico (cons prim args)))))

;sustituir-simbolo: reemplaza apariciones de sym por val
;recorre recursivamente la expresion simbolica
(define (sustituir-simbolo expr sym val)
  (cond
    ;; si es el simbolo que buscamos, reemplazar
    ((and (symbol? expr) (eq? expr sym)) val)
    ;; si es una expresion simbolica compuesta, recorrer hijos
    ((and (pair? expr) (eq? (car expr) 'simbolico))
     (let ((prim (cadr expr))
           (args (cddr expr)))
       (cons 'simbolico
             (cons prim
                   (map (lambda (a) (sustituir-simbolo a sym val)) args)))))
    ;; cualquier otra cosa se deja igual
    (else expr)))

;mathflow-display: muestra valores de MathFlow en formato adecuado
(define mathflow-display
  (lambda (val)
    (cond
      ((boolean? val) (display (if val "true" "false")))
      ((eqv? val 'null-val) (display "null"))
      ((eq? val 'vacio) (display "[]"))
      ((list? val) 
       (display "[") 
       (let loop ((l val))
         (unless (null? l)
           (mathflow-display (car l))
           (unless (null? (cdr l)) (display ", "))
           (loop (cdr l))))
       (display "]"))
      ((es-diccionario? val)
       (display "{")
       (let loop ((pares val))
         (if (null? pares)
             (void)
             (begin
               (mathflow-display (caar pares))
               (display ": ")
               (mathflow-display (cdar pares))
               (if (not (null? (cdr pares))) (display ", ") (void))
               (loop (cdr pares)))))
       (display "}"))
      ((symbol? val) (display val))
      ((and (pair? val) (eq? (car val) 'simbolico))
       (display "(")
       (display (prim-name (cadr val)))
       (display " ")
       (let loop ((args (cddr val)))
         (unless (null? args)
           (mathflow-display (car args))
           (unless (null? (cdr args)) (display ", "))
           (loop (cdr args))))
       (display ")"))
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
        (let ((env (extended-env-record proc-names vec (make-list-of-n-smthing len 'var) old-env)))
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

;; ========== Ejemplos Paso 4: Ciclos Iterativos ==========

;; --- While ---
;; (scan&parse "$ var x = 0 while <(x, 3) do begin print x; set x = +(x, 1) end done")

;; --- For ---
;; (scan&parse "$ var x = 0 for i in x do print i done") ; Dará error porque x no es una lista aún.

;; ========== Ejemplos Paso 5: Funciones y Recursión ==========

;; --- Función simple con retorno ---
;; (scan&parse "$ func sumar(a, b) { return +(a, b) } print sumar(5, 10) end")

;; --- Función sin retorno (devuelve null) ---
;; (scan&parse "$ func saludar(nombre) { print nombre } print saludar(\"Juan\") end")

;; --- Recursión ---
;; (scan&parse "$ func factorial(n) { if <=(n, 1) then return 1 else return *(n, factorial(-(n, 1))) end } print factorial(5) end")

;; ========== Ejemplos Paso 6 & 7: Listas y Diccionarios ==========

;; --- Listas ---
;; (scan&parse "$ var l = crear-lista(1, crear-lista(2, vacio)) print l end")
;; (scan&parse "$ var l = crear-lista(1, crear-lista(2, vacio)); l2 = crear-lista(3, vacio) print append(l, l2) end")
;; (scan&parse "$ var l = crear-lista(1, crear-lista(2, vacio)) set l = set-list(l, 0, 99); print l end")

;; --- Diccionarios ---
;; (scan&parse "$ var d = crear-diccionario(\"nombre\", \"Ana\", \"edad\", 34) print d end")
;; (scan&parse "$ var d = crear-diccionario(\"nombre\", \"Ana\") set d = set-diccionario(d, \"edad\", 34); print d end")
;; (scan&parse "$ var d = crear-diccionario(\"nombre\", \"Ana\") print ref-diccionario(d, \"nombre\") end")
;; (scan&parse "$ var d = crear-diccionario(\"nombre\", \"Ana\", \"edad\", 34) print claves(d) end")
;; (scan&parse "$ var d = crear-diccionario(\"nombre\", \"Ana\", \"edad\", 34) print valores(d) end")

;; ========== Ejemplos Paso 8: Cadenas y Bloques begin-end ==========

;; --- Cadenas ---
;; (scan&parse "$ print longitud(\"Hola\") end")
;; (scan&parse "$ print concatenar(\"Hola \", \"Mundo\") end")
;; (scan&parse "$ print buscar(\"Hola Mundo\", \"Mundo\") end")
;; (scan&parse "$ print buscar(\"Hola Mundo\", \"Adios\") end")

;; --- Bloques begin ... end ---
;; (scan&parse "$ var x = 0 if ==(x, 0) then begin print \"Es cero\"; set x = 1 end else print \"No es\" end end")

;; ========== Ejemplos Paso 9: Simbolos Algebraicos ==========

;; --- Símbolos ---
;; (scan&parse "$ var f = 'x print f end")

;; --- Expresiones Simbólicas ---
;; (scan&parse "$ var f = +('x, 1) print f end")
;; (scan&parse "$ var h = *(+('x, 2), 'y) print h end")

;; ========== Ejemplos Paso 10: Simplificar y Evaluar ==========

;; --- Simplificaciones basicas ---
;; (scan&parse "$ var f = +('x, 0) print simplificar(f) end")
;; (scan&parse "$ var f = *('x, 1) print simplificar(f) end")
;; (scan&parse "$ var f = *('x, 0) print simplificar(f) end")
;; (scan&parse "$ var f = -(+('x, 0), 0) print simplificar(f) end")

;; --- Evaluar (sustitucion + simplificacion) ---
;; (scan&parse "$ var f = +('x, 1) print evaluar(f, 'x, 5) end")
;; (scan&parse "$ var f = *(+('x, 2), 'y) print evaluar(f, 'x, 3) end")