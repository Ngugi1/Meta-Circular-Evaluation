
(#%require (only racket/base time error))
;; Booleans
(define true #t)
(define false #f)

(define (true? x)
  (not (eq? x #f)))
;; Triples 
;; Make-Triple
(define (make-triple first second third)
 (list 'triple first second third))
;; Accessor methods for triples
(define (first triple)
      (cadr triple))
(define (second triple)
  (caddr triple))
(define (third triple)
  (cadddr triple))
;; Setter procedures for the triple
(define (set-first! triple new-value)
  (set-car! (cdr triple) new-value))

(define (set-second! triple new-value)
 (set-car! (cddr triple) new-value)
  'done)

(define (set-third! triple new-value)
  (set-car! (cdddr triple) new-value)
  'done)

;; Futures
(define (future exp env)
  (let ((f (list 'future-object false exp)))
    f))
(define (future-body future)
  (caddr future))
(define (future-resolved? future)
  (cadr future))
(define (future? future)
  (tagged-list? future 'future))
(define (future-object? future)
  (tagged-list? future 'future-object))

;; Queue management
;; Threads
(define (make-thread exp env cont)
  (list exp env cont))
(define (thread-exp thread)
  (car thread))
(define (thread-env thread)
  (cadr thread))
(define (thread-cont thread)
  (caddr thread))

;; Lookup threads
(define (thread-queue)
  (lookup-threads the-global-environment))
(define (lookup-threads env)
  (let ((threads (lookup-variable-value 'threads env)))
   threads))
(define (update-threads thread)
  (let ((threads (lookup-threads the-global-environment)))
    (define-variable! 'threads (append threads (list item)) the-global-environment)
    (append threads (list item))))

(define (queue-thread! thread)
  (let* ((threads (lookup-threads the-global-environment))
         (new-threads (append threads (list thread))))
    (define-variable! 'threads new-threads the-global-environment)
    (append threads (list thread))))

(define (queue-thread!-a thread)
  (insert-queue! thread))

(define (remove-thread!)
  (let ((threads (lookup-threads the-global-environment)))
    (if (not (null? threads))
         (define-variable! 'threads (cdr threads) the-global-environment))
   ))

(define (remove-thread!-a)
  (remove-queue!))

(define (active-thread)
  (let ((threads (lookup-threads the-global-environment)))
    (car threads)))
(define (active-thread-a)
  (front-queue thread-queue))

(define (empty-threads? threads)
  (= (length threads) 0))

;; thread execution management
(define (lookup-execution-count)
  (let ((counter (lookup-variable-value 'execution-counter the-global-environment)))
    counter))
(define (increment-execution-count)
   (define-variable! 'execution-counter  (+ 1 (lookup-execution-count)) the-global-environment))
(define (execution-count-exceeded)
  (>= (lookup-execution-count) 20))
(define (reset-exececution-count)
  (define-variable!  'execution-counter 0 the-global-environment))


;; deref - execute the future 
(define (deref future)
  (cond ((future-object? future)
         (cond ((future-resolved? future)
                (future-body future))
               (else (let ((res (evaluate (procedure-body (future-body future)) (procedure-environment (future-body future))
                                          (lambda(result)
                                            (remove-thread!)
                                            (future-body result)))))
                       res))))))
;; Switch - execute the next thread
(define (switch)
  (cond ((not (empty-threads? (thread-queue)))
         (let ((active (active-thread)))
           (queue-thread! active)
           (remove-thread!)
           (let ((to-activate (active-thread)))
             (evaluate (thread-exp to-activate)
                       (thread-env to-activate)
                       (thread-cont to-activate)))))))



(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))

(define (variable? exp)
  (symbol? exp))

(define (make-triple? exp)
  (tagged-list? exp 'make-triple))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   
                   (cddr exp))))
;; Lambdas
(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))



;; Ifs 
(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      false))
;; Begin
(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp)
  (cdr exp))

(define (last-exp? seq)
  (null? (cdr seq)))

(define (first-exp seq)
  (car seq))

(define (rest-exps seq)
  (cdr seq))
;; Procedure application
(define (application? exp)
  (pair? exp))
;; Operator
(define (operator exp)
  (car exp))
;; Operands
(define (operands exp)
  (cdr exp))

(define (no-operands? ops)
  (null? ops))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))

;; Environments
(define (enclosing-environment env)
  (cdr env))

(define (first-frame env)
  (car env))

(define the-empty-environment
  '())
;; Frames
(define (make-frame)
  (cons '() '()))

(define (frame-variables frame)
  (car frame))

(define (frame-values frame)
  (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
;; Extend environment
(define (extend-environment vars vals base-env)
  (define frame (make-frame))
  (define (extend-loop vars vals)
    (if (null? vars)
        (if (null? vals)
            (cons frame base-env)
            (error "Too many arguments supplied"))
        (if (null? vals)
            (error "Too few arguments supplied")
            (begin
              (add-binding-to-frame! (car vars) (car vals) frame)
              (extend-loop (cdr vars) (cdr vals))))))
    (extend-loop vars vals))
;; Look up variable in an environment
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) 
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;; Procedures
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p)
  (cadr p))

(define (procedure-body p)
  (caddr p))

(define (procedure-environment p)
  (cadddr p))
;; Primitive procedures
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

;; Continuations
(define (make-continuation cont)
  (list 'continuation cont))
 
(define (continuation? c)
  (tagged-list? c 'continuation))
 
(define (continuation-continuation p)
  (cadr p))

(define (call-cc-prim cont args)
  (let ((proc (car args)))
    (apply-c proc (list (make-continuation cont)) cont)))
;; check execution to see if it has exceeded its allocated execution count 
(define (check-execution exp env cont)
  (cond ((execution-count-exceeded)
         ;; Queue this thread
         (if (not (empty-threads? (thread-queue)))
             (begin
               ;; Make the head become the tail
               (queue-thread! (make-thread exp env cont))
               (reset-exececution-count)
               (let ((active (active-thread)))
                 (remove-thread!)
                 (evaluate (thread-exp active) (thread-env active) (thread-cont active)))))
             (reset-exececution-count))
        (else (increment-execution-count))))

(define (evaluate exp env cont)
  (check-execution exp env cont)
  (cond ((self-evaluating? exp) (cont exp))
        ((begin? exp) (eval-begin exp env cont))
        ((variable? exp) (eval-variable exp env cont))
        ((quoted? exp) (eval-quoted exp cont))
        ((assignment? exp) (eval-assignment exp env cont))
        ((definition? exp) (eval-definition exp env cont))
        ((if? exp) (eval-if exp env cont))
        ((lambda? exp) (eval-lambda exp env cont))
        ;; Add your special forms here!
        ((future? exp) (eval-future exp env cont))
        ((application? exp) (eval-application exp env cont))
        (else (error "Unknown expression type -- EVAL" exp))))

;; Evals
(define (eval-future exp env cont)
  (let ((f (future (cadr exp) env)))
    (cont f)))

(define (eval-variable exp env cont)
  (let ((value (lookup-variable-value exp env)))
    (cont value)))

(define (eval-quoted exp cont)
  (let ((text (text-of-quotation exp)))
    (cont text)))

(define (eval-lambda exp env cont)
  (cont (make-procedure (lambda-parameters exp)
                        (lambda-body exp)
                        env)))
(define (eval-assignment exp env cont)
  (evaluate (assignment-value exp) env
            (lambda (value)
              (set-variable-value! (assignment-variable exp)
                                   value
                                   env)
              (cont value))))

(define (eval-def-future exp value env)
  (let* ((thread-body
          `(begin (set! ,(definition-variable exp) (list 'future-object true ,(future-body value)))))
         (proc (make-procedure '() thread-body env)))
    (define-variable! (definition-variable exp)
      (list 'future-object #f proc)
      env)
    (queue-thread! (make-thread
                    (procedure-body proc)
                    (procedure-environment  proc)
                    (lambda(results)
                      (remove-thread!)
                      results)))))

(define (eval-definition exp env cont)
  (evaluate (definition-value exp) env
            (lambda (value)
              (cond ((future-object? value)
                     (eval-def-future exp value env))
                    (else (define-variable! (definition-variable exp)
                            value
                            env)
                          ))
              (cont 'ok))))

(define (eval-if exp env cont)
  (evaluate (if-predicate exp) env
            (lambda (value)
              (if (true? value)
                  (evaluate (if-consequent exp) env cont)
                  (evaluate (if-alternative exp) env cont)))))

(define (eval-begin exp env cont) 
  (eval-sequence (begin-actions exp) env cont))


(define (eval-sequence exps env cont)
  (if (last-exp? exps)
      (evaluate (first-exp exps) env cont)
      (evaluate (first-exp exps) env
                (lambda (ignored)
                  (eval-sequence (rest-exps exps) env cont)))))

(define (eval-application exp env cont)
  (evaluate (operator exp)
            env
            (lambda (procedure)
              (eval-operands (operands exp) env
                                   (lambda (arguments)
                                      (apply-c procedure arguments cont)
                                     )))))

(define (eval-operands exps env cont)
  (define (exp-loop exps arguments)
    (if (no-operands? exps)
        (cont (reverse arguments))
        (evaluate (first-exp exps) env
                  (lambda (value)
                    (exp-loop (rest-exps exps) (cons value arguments))))))
  (exp-loop exps '()))

(define (apply-c procedure arguments cont)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments cont))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))
           cont))
        ((continuation? procedure)
         ((continuation-continuation procedure) (car arguments)))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (apply-primitive-procedure proc args cont)
  (cont ((primitive-implementation proc) cont args)))

(define (prim/cc prim)
    (lambda (cont args)
      (apply prim args)))

(define primitive-procedures
  (list (list 'car (prim/cc car))
        (list 'cdr (prim/cc cdr))
        (list 'cadr (prim/cc cadr))
        (list 'cddr (prim/cc cddr))
        (list 'cdddr (prim/cc cdddr))
        (list 'caddr (prim/cc caddr))
        (list 'cadddr (prim/cc cadddr))
        (list 'set-car! (prim/cc set-car!))
        (list 'set-cdr! (prim/cc set-cdr!))
        (list 'cons (prim/cc cons))
        (list 'null? (prim/cc null?))
        (list 'list (prim/cc list))
        (list '+ (prim/cc +))
        (list '- (prim/cc -))
        (list '* (prim/cc *))
        (list '/ (prim/cc /))
        (list '= (prim/cc =))
        (list 'newline (prim/cc newline))
        (list 'display (prim/cc display))
        (list 'append (prim/cc append))
        
        ;; New in this version:
        (list 'call/cc call-cc-prim)
        ;; Add your primitives here!
        (list 'make-triple (prim/cc make-triple))
        (list 'first (prim/cc first))
        (list 'second (prim/cc second))
        (list 'third (prim/cc third))
        (list 'set-first! (prim/cc set-first!))
        (list 'set-second! (prim/cc set-second!))
        (list 'set-third! (prim/cc set-third!))
        ;; Threads and futures
        (list 'deref (prim/cc deref))
        (list 'switch (prim/cc switch))
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    (define-variable! 'threads '() initial-env)
    (define-variable! 'execution-counter 0 initial-env)
    initial-env))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop value)
  (announce-output output-prompt)
  (user-print value)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (evaluate input the-global-environment driver-loop)))
   
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>)))
        ((future-object? object)
         (if (future-resolved? object)
             (display (list 'resolved 'future))
             (display (list 'unresolved 'future))))
      (else (display object))))

(define the-global-environment (setup-environment))

(driver-loop "CPS Evaluator 0.9")