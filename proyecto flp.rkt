#lang eopl
#| Diego Armando Espinosa Ossa 201942206
   Juan David Lopez Vanegas 2243077
   Juan Manuel Moreno Correa 
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
   (letter (arbno (or letter digit "?"))) symbol)
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
    
    ;; Corrección: uso de no-terminales intermedios para separar con pares
    (binding (identifier "=" expression) a-binding)
    (dic-pair (identifier ":" expression) a-dic-pair)
    
    (expression ("{" (separated-list dic-pair ",") "}")
                dicc-literal-exp)
    (expression ("evaluar" "(" expression "," (separated-list binding ",") ")")
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
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (main body (init-env))))))

; Ambiente inicial
(define init-env
  (lambda ()
     (empty-env)))

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
                   (newline)
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
                 (let ((lst (if (eq? val 'vacio) '() val)))
                   (if (list? lst)
                       (let loop ((l lst))
                         (if (null? l)
                             'ok
                             (begin
                               (eval-expression body-exp (extend-env (list id) (list (direct-target (car l))) (list 'var) env))
                               (loop (cdr l)))))
                       (eopl:error 'eval-expression "El iterador de for debe ser una lista, se obtuvo: ~s" val)))))
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
      
      ;; Corrección: Procesamiento del nuevo datatype dic-pair
      (dicc-literal-exp (pairs)
         (make-dict
           (let loop ((ps pairs))
              (if (null? ps)
                  '()
                  (cases dic-pair (car ps)
                    (a-dic-pair (id e)
                      (cons (cons (symbol->string id) (eval-expression e env))
                            (loop (cdr ps)))))))))
                      
      (simplificar-exp-sym (expr)
         (simplificar-exp (eval-expression expr env)))
      
      ;; Corrección: Procesamiento del nuevo datatype binding
      (evaluar-exp (expr bindings)
         (let ((expr-val (eval-expression expr env))
               (vals-eval (map (lambda (b) (cases binding b (a-binding (id e) (eval-expression e env)))) bindings))
               (ids (map (lambda (b) (cases binding b (a-binding (id e) id))) bindings)))
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
      )))

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
        (let ((newenv (if (null? sen) env (save-sen sen env))))
          (let loop ((es (cons exp exps))
                     (env2 newenv)
                     (last-val 'ok))
            (if (null? es)
                last-val
                (let ((v (eval-expression (car es) env2)))
                  (loop (cdr es) env2 v)))))))))

(define save-sen
  (lambda (sen env)
    (if (null? sen) env
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
            (let ((new-env (extend-env (list id) (list (direct-target (list 'simbolico id))) (list 'const) env)))
              (if (null? (cdr sen))
                  new-env
                  (save-sen (cdr sen) new-env))))
          (func-sentence (id ids body-exps ret-exp)
            (let ((new-env (extend-env-recursively (list id) (list ids) (list (func-body-exp body-exps ret-exp)) env)))
              (if (null? (cdr sen))
                  new-env
                  (save-sen (cdr sen) new-env))))))))

;; Corrección: caso base corregido para n=0
(define make-list-of-n-smthing
  (lambda(n smthing)
    (if (zero? n)
        '()
        (cons smthing (make-list-of-n-smthing (- n 1) smthing)))))

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

;; Corrección: Se eliminó la definición duplicada/defectuosa de eval-primapp-exp-rands
(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expression x env))
         rands)))

(define eval-def-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-def-exp-rand x env))
         rands)))

;; Corrección: Devuelve direct-target para evitar fallas en deref
(define eval-def-exp-rand
  (lambda (rand env)
    (direct-target (eval-expression rand env))))

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

;make-dict: envuelve una lista de pares (llave . valor) con una etiqueta explícita
;para que un diccionario nunca se confunda con una lista normal, ni siquiera si
;queda anidado dentro de otra lista (ej: crear-lista(dic, ...)).
(define (make-dict pares) (cons 'dict-tag pares))

;dict-pairs: extrae los pares (llave . valor) reales de un diccionario etiquetado
(define (dict-pairs val) (cdr val))

;es-diccionario?: verifica si un valor ES un diccionario (mira la etiqueta, no la forma)
(define (es-diccionario? val)
  (and (pair? val) (eq? (car val) 'dict-tag)))

;buscar-en-dict: busca una llave en el diccionario (recibe la lista de pares, no el dict con tag)
(define (buscar-en-dict dict llave)
  (cond
    ((null? dict) 'null-val)
    ((equal? (caar dict) llave) (cdar dict))
    (else (buscar-en-dict (cdr dict) llave))))

;actualizar-dict: actualiza o agrega un par llave-valor (recibe/devuelve la lista de pares)
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
          (mod-prim ()
            (let ((a (car args)) (b (cadr args)))
              (if (and (integer? a) (integer? b))
                  (modulo a b)
                  (- a (* b (truncate (/ a b)))))))
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
              (and (not (es-diccionario? lst))
                   (or (list? lst) (eq? lst 'vacio)))))
          
          ;; Corrección: Prevenir (car '()) en listas vacías nativas
          (cabeza-prim ()
            (let ((lst (car args)))
              (if (or (eq? lst 'vacio) (null? lst))
                  (eopl:error 'cabeza "Lista vacía")
                  (car lst))))
          (cola-prim ()
            (let ((lst (car args)))
              (if (or (eq? lst 'vacio) (null? lst))
                  (eopl:error 'cola "Lista vacía")
                  (let ((res (cdr lst)))
                    (if (null? res) 'vacio res)))))
          (append-prim ()
            (let ((lst1 (car args)) (lst2 (cadr args)))
              (append (if (eq? lst1 'vacio) '() lst1)
                      (if (eq? lst2 'vacio) '() lst2))))
          (ref-list-prim ()
            (let ((lst (car args)) (idx (cadr args)))
              (if (eq? lst 'vacio)
                  (eopl:error 'ref-list "Lista vacía")
                  (list-ref lst idx))))
          (set-list-prim ()
            (let ((lst (car args)) (idx (cadr args)) (val (caddr args)))
              (if (eq? lst 'vacio)
                  (eopl:error 'set-list "Lista vacía")
                  (reemplazar-en-lista lst idx val))))
          ;primitivas de diccionarios (listas de asociacion)
          (crear-diccionario-prim ()
            (make-dict
              (let loop ((a args))
                (if (null? a)
                    '()
                    (if (null? (cdr a))
                        (eopl:error 'crear-diccionario "Falta valor para la llave ~s" (car a))
                        (cons (cons (car a) (cadr a))
                              (loop (cddr a))))))))
          (diccionario-pred-prim ()
            (es-diccionario? (car args)))
          (ref-diccionario-prim ()
            (if (es-diccionario? (car args))
                (buscar-en-dict (dict-pairs (car args)) (cadr args))
                (eopl:error 'ref-diccionario "El valor no es un diccionario: ~s" (car args))))
          (set-diccionario-prim ()
            (if (es-diccionario? (car args))
                (make-dict (actualizar-dict (dict-pairs (car args)) (cadr args) (caddr args)))
                (eopl:error 'set-diccionario "El valor no es un diccionario: ~s" (car args))))
          (claves-prim ()
            (if (es-diccionario? (car args))
                (map car (dict-pairs (car args)))
                (eopl:error 'claves "El valor no es un diccionario: ~s" (car args))))
          (valores-prim ()
            (if (es-diccionario? (car args))
                (map cdr (dict-pairs (car args)))
                (eopl:error 'valores "El valor no es un diccionario: ~s" (car args))))
          ;primitivas de cadenas
          (longitud-prim ()
            (string-length (car args)))
          (concatenar-prim ()
            (let((a (car args))
                 (b (cadr args)) 
                 )
              (if (and (number? a)(number? b))
                  (let((stra(number->string a))
                       (strb(number->string b)))
                    (string-append stra strb)
                    )
                  (if (and (string? a)(number? b))
                  (let((strb(number->string b)))
                    (string-append a strb)
                    )
                    (if (and (number? a)(string? b))
                  (let((stra(number->string a)))
                    (string-append b stra))
                  (string-append (car args) (cadr args)))
                    ))))
          (buscar-prim ()
            (buscar-subcadena (car args) (cadr args)))
          ))))

;simplificar-exp: recorre recursivamente el arbol
(define (simplificar-exp expr)
  (cond
    ;; nodo hoja: variable simbolica sin sustituir, se deja tal cual
    ((and (pair? expr) (eq? (car expr) 'simbolico) (null? (cddr expr)))
     expr)
    ((not (and (pair? expr) (eq? (car expr) 'simbolico)))
     expr)
    (else
     (let* ((prim (cadr expr))
            (args-raw (cddr expr))
            (args (map simplificar-exp args-raw)))
       (if (todos-numericos? args)
           (apply-primitive prim args)
           (simplificar-reglas prim args))))))

;simplificar-reglas: aplica identidades algebraicas
(define (simplificar-reglas prim args)
  (cases primitivaArit prim
    (add-prim ()
      (let ((a (car args)) (b (cadr args)))
        (cond
          ((and (number? b) (zero? b)) a)
          ((and (number? a) (zero? a)) b)
          ;; Plegado asociativo: (x + c1) + c2 → x + (c1 + c2)
          ((and (number? b)
                (pair? a) (eq? (car a) 'simbolico)
                (= (length a) 4)
                (cases primitivaArit (cadr a) (add-prim () #t) (else #f))
                (number? (cadddr a)))
           (let ((sym-part (caddr a)) (num-part (cadddr a)))
             (cons 'simbolico (cons prim (list sym-part (+ num-part b))))))
          (else (cons 'simbolico (cons prim args))))))
    (substract-prim ()
      (let ((a (car args)) (b (cadr args)))
        (cond
          ((and (number? b) (zero? b)) a)
          ((equal? a b) 0)
          (else (cons 'simbolico (cons prim args))))))
    (mult-prim ()
      (let ((a (car args)) (b (cadr args)))
        (cond
          ((and (number? a) (zero? a)) 0)
          ((and (number? b) (zero? b)) 0)
          ((and (number? b) (= b 1)) a)
          ((and (number? a) (= a 1)) b)
          ;; Plegado asociativo: (x * c1) * c2 → x * (c1 * c2)
          ((and (number? b)
                (pair? a) (eq? (car a) 'simbolico)
                (= (length a) 4)
                (cases primitivaArit (cadr a) (mult-prim () #t) (else #f))
                (number? (cadddr a)))
           (let ((sym-part (caddr a)) (num-part (cadddr a)))
             (cons 'simbolico (cons prim (list sym-part (* num-part b))))))
          (else (cons 'simbolico (cons prim args))))))
    (div-prim ()
      (let ((a (car args)) (b (cadr args)))
        (cond
          ((and (number? b) (= b 1)) a)
          ((equal? a b) 1)
          (else (cons 'simbolico (cons prim args))))))
    (else (cons 'simbolico (cons prim args)))))

;sustituir-simbolo: reemplaza apariciones de sym por val
(define (sustituir-simbolo expr sym val)
  (cond
    ((and (symbol? expr) (eq? expr sym)) val)
    ;; nodo hoja: variable simbolica, p.ej. (simbolico b)
    ((and (pair? expr) (eq? (car expr) 'simbolico) (null? (cddr expr)))
     (if (eq? (cadr expr) sym) val expr))
    ;; nodo de operacion, p.ej. (simbolico mult-prim arg1 arg2)
    ((and (pair? expr) (eq? (car expr) 'simbolico))
     (let ((prim (cadr expr))
           (args (cddr expr)))
       (cons 'simbolico
             (cons prim
                   (map (lambda (a) (sustituir-simbolo a sym val)) args)))))
    (else expr)))

;; Corrección: Orden de condicionales corregido para imprimir diccionarios
(define mathflow-display
  (lambda (val)
    (cond
      ((boolean? val) (display (if val "true" "false")))
      ((eqv? val 'null-val) (display "null"))
      ((eq? val 'vacio) (display "[]"))
      ((es-diccionario? val)
       (display "{")
       (let loop ((pares (dict-pairs val)))
  (unless (null? pares)
    (mathflow-display (caar pares))
    (display ": ")
    (mathflow-display (cdar pares))
    (when (not (null? (cdr pares))) (display ", "))
    (loop (cdr pares))))
       (display "}"))
      ((list? val) 
       (display "[") 
       (let loop ((l val))
         (unless (null? l)
           (mathflow-display (car l))
           (unless (null? (cdr l)) (display ", "))
           (loop (cdr l))))
       (display "]"))
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
                                      (make-list-of-n-smthing (length args) 'var)
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
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
(define empty-env  
  (lambda ()
    (empty-env-record)))

;: <list-of symbols> <list-of numbers> enviroment -> enviroment
(define extend-env
  (lambda (syms vals labs env)
    (extended-env-record
     syms
     (list->vector vals)
     labs
     env)))

;-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
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
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

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
    (or (number? x) (procval? x) (boolean? x) (string? x) (eqv? x 'null-val) (list? x) (symbol? x) (pair? x))))

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
;; CORREGIDO: "vacio" es palabra reservada, se cambió el nombre de la variable
;; (scan&parse "$ var v = null print v end")

;; --- Combinacion de tipos con var y const ---
;; (scan&parse "$ var x = 42; nombre = \"Juan\"; activo = true print nombre end")
;; (scan&parse "$ const pi = 3.14; mensaje = \"hola\" print mensaje end")

;; --- Semantica dinamica de true-value? ---
;; false, 0, "", null son falsos. Todo lo demas es verdadero.
;; CORREGIDO: se agregó un segundo "end" (el if cierra con "end", y el programa necesita otro "end" más)
;; (scan&parse "$ var x = 0 if x then print 1 else print 0 end end")
;; (scan&parse "$ var x = false if x then print 1 else print 0 end end")
;; (scan&parse "$ var x = null if x then print 1 else print 0 end end")
;; (scan&parse "$ var x = 5 if x then print 1 else print 0 end end")

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
;; CORREGIDO: faltaba el "end" final del programa (begin..end cierra el begin, done cierra el while)
;; (scan&parse "$ var x = 0 while <(x, 3) do begin print x; set x = +(x, 1) end done end")

;; --- For ---
;; CORREGIDO: se agregó el "end" final del programa. Sigue dando error esperado (x no es lista)
;; (scan&parse "$ var x = 0 for i in x do print i done end") ; Dará error porque x no es una lista aún.
;; Ejemplo funcional de for con una lista real:
;; (scan&parse "$ var l = crear-lista(1, crear-lista(2, crear-lista(3, vacio()))) for i in l do print i done end")

;; ========== Ejemplos Paso 5: Funciones y Recursión ==========

;; --- Función simple con retorno ---
;; CORREGIDO: para invocar una función definida con "func" se necesita la palabra "call"
;; (scan&parse "$ func sumar(a, b) { return +(a, b) } print call sumar(5, 10) end")

;; --- Función sin retorno (devuelve null) ---
;; CORREGIDO: "func" exige un "return" obligatorio, y la llamada necesita "call"
;; (scan&parse "$ func saludar(nombre) { print nombre return null } print call saludar(\"Juan\") end")

;; --- Recursión ---
;; CORREGIDO: "return" solo puede aparecer UNA vez, justo antes del "}", no dentro de las ramas del if.
;; Se reescribe para que el "if" completo sea la expresión retornada; las llamadas usan "call".
;; (scan&parse "$ func factorial(n) { return if <=(n, 1) then 1 else *(n, call factorial(-(n, 1))) end } print call factorial(5) end")

;; ========== Ejemplos Paso 6 & 7: Listas y Diccionarios ==========

;; --- Listas ---
;; CORREGIDO: "vacio" siempre se llama como primitiva: vacio()
;; (scan&parse "$ var l = crear-lista(1, crear-lista(2, vacio())) print l end")
;; (scan&parse "$ var l = crear-lista(1, crear-lista(2, vacio())); l2 = crear-lista(3, vacio()) print append(l, l2) end")
;; (scan&parse "$ var l = crear-lista(1, crear-lista(2, vacio())) set l = set-list(l, 0, 99); print l end")

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
;; CORREGIDO: el binding usa identificador = expresión, sin comilla: evaluar(f, x = 5)
;; (scan&parse "$ var f = +('x, 1) print evaluar(f, x = 5) end")
;; (scan&parse "$ var f = *(+('x, 2), 'y) print evaluar(f, x = 3) end")

;Pregunta 2
; $
; var edad = 10;
; y = 10.23;
; z = null;
; diego = "Diego";
; valor = true;
; valor2 = false
; var dic = crear-diccionario("diego",24)
; symbol x
; var exp1 = +(1, x)
; var l1 = crear-lista(dic, crear-lista(y, vacio()))
; var l2 = crear-lista(z, crear-lista(diego, vacio()))
; var l3 = crear-lista(valor, crear-lista(valor2, vacio()))
; var A1 = append(l1, l2)
; var S2 = append(A1, l3)
; 
; func pregunta2(){
;                 return S2 
;                  
;                 }
; print call pregunta2()
; end

;Pregunta 3
; $
; var x = 10
; set x = 20;
; print x
; end

;;Pregunta 4
;Primer programa
; $
; const x = 10
; print x
; end
; 

;Segundo programa
; $
; const x = 10
; set x = 11
; end
; 


;Pregunta 5
; a) Enteros
; b) Flotantes
; 
; $
; var
;   enteros = [
;     +(10, 5),
;     -(10, 5),
;     *(10, 5),
;     %(10, 3),
;     /(10, 2),
;     add1(10),
;     sub1(10)
;   ];
;   
;   flotantes = [
;     +(10.5, 5.2),
;     -(10.5, 5.2),
;     *(10.5, 5.2),
;     %(10.5, 3.2),
;     /(10.5, 2.1),
;     add1(10.5),
;     sub1(10.5)
;   ]
; 
; begin
;   print enteros;
;   print flotantes
; end
; end


;Pregunta 6
;
; $
; var
;   rel_int = [
;     <(2, 5),
;     >(10, 4),
;     <=(3, 3),
;     >=(8, 7),
;     ==(5, 5),
;     <>(4, 9)
;   ];
;   
;   rel_float = [
;     <(2.5, 5.2),
;     >(10.1, 4.4),
;     <=(3.3, 3.3),
;     >=(8.5, 7.1),
;     ==(5.5, 5.5),
;     <>(4.2, 9.9)
;   ];
;   
;   bol_ops = [
;     and(true, false),
;     or(true, false),
;     not(true)
;   ]
; 
; begin
;   print rel_int;
;   print rel_float;
;   print bol_ops
; end
; end


;Pregunta 7
;
; $
; var
;   cadenas_ops = [
;     longitud("Interpretador"),
;     concatenar("Hola ", "Mundo")
;   ]
; 
; begin
;   print cadenas_ops
; end
; end


;Pregunta 11
;Punto a
; $ var l = crear-lista(1, crear-lista(2, crear-lista(3, crear-lista(4, crear-lista(5, vacio())))))
; var aux = vacio()
; for i in l do
;   begin
;   set aux = crear-lista(concatenar("1/",ref-list(l,-(5,i))),aux);
;   print l;
;   print aux
;   end
; done
; 
; end

;Punto b
; $
; var i = 1; 
; aux = false
; 
; func esPar?(n)
; {if ==(%(n,2),0)
;      then set aux = true
;      else set aux = false
;     end
;  return aux     
; }
; while <=(i,5)
; do
;  begin
;   print call esPar?(i);
;   set i = +(i,1)
;  end
; done
; end

;Pregunta 13
;Punto 1
;
; $
; symbol b;
; symbol h;
; symbol r;
; var 
;   area_triangulo = /(*(b, h), 2);
;   area_circulo = *(3.14159, *(r, r))
; begin
;   print area_triangulo;
;   print area_circulo;
;   print evaluar(area_triangulo, b = 10);
;   print evaluar(area_triangulo, h = 5);
;   print evaluar(area_triangulo, b = 10, h = 5);
;   print evaluar(area_circulo, r = 4)
; end
; end

;Punto 2
;
; Ejemplo 1: simplificar(x + 0) => x
; Ejemplo 2: simplificar(((x * 1) + 0)) => x
; Ejemplo 3: simplificar(((x + 2) + 3)) => (+ x, 5)
; Ejemplo 4: simplificar((x * 0) + 10) => 10
; Ejemplo 5: simplificar((x * 5) * 6) => (* x, 30)
; Ejemplo 6 (recursivo): simplificar(((x + 0) * 1) + (2 + 3)) => (+ x, 5)
;
; $
; symbol x;
; var
;   y = +(+(x, 2), 3);
;   ex1 = +(x, 0);
;   ex2 = +(*(x, 1), 0);
;   ex4 = +(*(x, 0), 10);
;   ex5 = *(*(x, 5), 6);
;   ex6 = +(*(+(x, 0), 1), +(2, 3))
; begin
;   print simplificar(ex1);
;   print simplificar(ex2);
;   print simplificar(y);
;   print simplificar(ex4);
;   print simplificar(ex5);
;   print simplificar(ex6)
; end
; end
; end

;Pregunta 10
;Elabore la función "map" en su lenguaje de programación. 
;La función "map" recibe una lista L y una función unaria F. 
;"map" debe retornar una lista donde se le ha aplicado la función F a cada elemento de la lista L.
;La implementación debe hacerse a través de recursión.
;
; $
; func map(L, F) {
;   return if vacio?(L)
;          then vacio()
;          else crear-lista(call F(cabeza(L)), call map(cola(L), F))
;          end
; }
; 
; func porDos(x) {
;   return *(x, 2)
; }
; 
; var miLista = [1, 2, 3, 4, 5];
; 
; begin
;   print call map(miLista, porDos)
; end
; end

;Pregunta 9
;Elabore una función que reciba una lista de enteros L y retorne un registro 
;(o diccionario) con dos claves: "valores" y "factoriales".
;
; $
; func fact(n) {
;   return if ==(n, 0) then 1 else *(n, call fact(sub1(n))) end
; }
; 
; func mapFact(L) {
;   return if vacio?(L) 
;          then vacio() 
;          else crear-lista(call fact(cabeza(L)), call mapFact(cola(L))) 
;          end
; }
; 
; func registroFactorial(L) {
;   return { valores : L, factoriales : call mapFact(L) }
; }
; 
; var listaPrueba = [1, 2, 3, 4, 7, 9];
; 
; begin
;   print call registroFactorial(listaPrueba)
; end
; end