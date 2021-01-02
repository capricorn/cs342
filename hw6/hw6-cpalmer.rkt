#lang racket

(require "program.rkt")
(provide (all-defined-out))

(define (synchk-bcond expr)
  (cond
    [(equal? (car expr) 'gt) (and (synchk-arithexpr (cadr expr)) (synchk-arithexpr (cadr (cdr expr))))]
    [(equal? (car expr) 'lt) (and (synchk-arithexpr (cadr expr)) (synchk-arithexpr (cadr (cdr expr))))]
    [(equal? (car expr) 'eq) (and (synchk-arithexpr (cadr expr)) (synchk-arithexpr (cadr (cdr expr))))]
    [else #f]))

(define (synchk-condexpr expr)
  (cond
    [(not (list? expr)) #f]
    [(and (equal? (length expr) 2) (equal? (car expr) 'not) (synchk-condexpr (cadr expr))) #t]
    [(not (equal? (length expr) 3)) #f] ; Makes sure from this point on, len(expr) = 3
    [(and (equal? (car expr) 'or) (synchk-condexpr (cadr expr)) (synchk-condexpr (cadr (cdr expr)))) #t]
    [(and (equal? (car expr) 'and) (synchk-condexpr (cadr expr)) (synchk-condexpr (cadr (cdr expr)))) #t]
    [(synchk-bcond expr) #t]
    [else #f]))

(define (synchk-op-symbol op)
  (or (equal? '+ op) (equal? '- op) (equal? '* op) (equal? '/ op)))

(define (synchk-op expr)
  (cond
    [(not (equal? (length expr) 3)) #f]
    [else (and (synchk-op-symbol (car expr)) (synchk-arithexpr (cadr expr)) (synchk-arithexpr (cadr (cdr expr))))]))

(define (synchk-arithexpr expr)
  (cond
    [(number? expr) #t]
    [(symbol? expr) #t]
    [(list? expr) (synchk-op expr)]
    [else #f]))

(define (synchk-stmt-decl stmt) ; stmt -> (decl var)
  (and (equal? (car stmt) 'decl) (symbol? (cadr stmt))))    ; stmt -> decl -> (decl symbol)

(define (synchk-stmt-assign stmt) ; stmt -> (assign var arithexpr)
  (and (equal? 'assign (car stmt)) (symbol? (cadr stmt)) (synchk-arithexpr (cadr (cdr stmt)))))

(define (synchk-stmt-if stmt) ; stmt -> (if condexpr (sseq))
  (and (equal? 'if (car stmt)) (synchk-condexpr (cadr stmt)) (synchk-sseq (cadr (cdr stmt)))))

(define (synchk-stmt stmt)  ; We know each stmt must be a list itself
  (cond
    [(not (list? stmt)) #f] ; stmt must be a list
    [(equal? (length stmt) 2) (synchk-stmt-decl stmt)]  ; stmt -> (decl var)
    ; stmt -> (assign var arith) | 
    [(equal? (length stmt) 3) (or (synchk-stmt-assign stmt) (synchk-stmt-if stmt))]
    [else #f])) ; len(stmt) = 1

(define (synchk-sseq sseq)
  (cond
    [(equal? (length sseq) 1) (synchk-stmt (car sseq))] ; (sseq)->(stmt)
    [(> (length sseq) 1) (and (synchk-stmt (car sseq)) (synchk-sseq (cdr sseq)))] ; (sseq)->(stmt sseq)
    [else #f]))

(define (synchk prog)
    (if (list? prog)
        (synchk-sseq prog)
        #f))

(define (sem-op op env)
  (cond
    [(equal? (car op) '+) (+ (sem-arithexpr (cadr op) env) (sem-arithexpr (cadr (cdr op)) env))]
    [(equal? (car op) '-) (- (sem-arithexpr (cadr op) env) (sem-arithexpr (cadr (cdr op)) env))]
    [(equal? (car op) '*) (* (sem-arithexpr (cadr op) env) (sem-arithexpr (cadr (cdr op)) env))]
    [(equal? (car op) '/) (/ (sem-arithexpr (cadr op) env) (sem-arithexpr (cadr (cdr op)) env))]))

(define (lst-filter-rec lst bop keep-lst)
  (cond
    [(null? lst) keep-lst]
    [(bop (car lst)) (lst-filter-rec (cdr lst) bop (lst-append keep-lst (car lst)))]
    [else (lst-filter-rec (cdr lst) bop keep-lst)])) 

(define (lst-filter lst bop)
  (lst-filter-rec lst bop '()))

(define (sem-arithexpr expr env)
  (cond
    [(number? expr) expr]
    [(not (equal? '() (get-env-func expr env))) expr]
    [(symbol? expr) (cadr (car (lst-filter env (lambda (k) (equal? (car k) expr)))))]
    [else (sem-op expr env)]))

(define (sem-stmt-decl decl)
  (list (cadr decl) 0))

(define (sem-bcond bcond env)
  (cond
    [(equal? (car bcond) 'gt) (> (sem-arithexpr (cadr bcond) env) (sem-arithexpr (cadr (cdr bcond)) env))]
    [(equal? (car bcond) 'lt) (< (sem-arithexpr (cadr bcond) env) (sem-arithexpr (cadr (cdr bcond)) env))]
    [(equal? (car bcond) 'eq) (equal? (sem-arithexpr (cadr bcond) env) (sem-arithexpr (cadr (cdr bcond)) env))]))

(define (sem-condexpr cexpr env)
  (cond
    [(equal? (car cexpr) 'or) (or (sem-condexpr (cadr cexpr) env) (sem-condexpr (cadr (cdr cexpr)) env))]
    [(equal? (car cexpr) 'and) (and (sem-condexpr (cadr cexpr) env) (sem-condexpr (cadr (cdr cexpr)) env))]
    [(equal? (car cexpr) 'not) (not (sem-condexpr (cadr cexpr) env))]
    [else (sem-bcond cexpr env)]))

(define (stack-push stack e)
  (cons e stack))

(define (stack-pop stack)
  (cdr stack))

(define (stack-pop-n stack n)
  (if (equal? n 0)
      stack
      (stack-pop-n (cdr stack) (- n 1))))

(define (stack-peek stack)
  (car stack))

(define (lst-get-rec lst cnt idx)
  (if (equal? cnt idx)
      (car lst)
      (lst-get-rec (cdr lst) (+ cnt 1) idx)))

(define (lst-get lst idx)
  (lst-get-rec lst 0 idx))

(define (lst-find-rec lst eqcmp idx)
  (cond
    [(null? lst) -1]
    [(eqcmp (car lst)) idx]
    [else (lst-find-rec (cdr lst) eqcmp (+ idx 1))]))

(define (lst-append lst e) 
  (if (null? lst)
      (list e)
      (if (equal? (length lst) 1)
          (cons (car lst) (list e))
          (cons (car lst) (lst-append (cdr lst) e))))) 

; Return first index which contains e in lst
(define (lst-find lst eqcmp)
  (lst-find-rec lst eqcmp 0))

(define (lst-set-rec lst new-lst cnt idx e)
  (cond
    [(null? lst) new-lst]
    [(equal? cnt idx) (lst-set-rec (cdr lst) (lst-append new-lst e) (+ cnt 1) idx e)]
    [else (lst-set-rec (cdr lst) (lst-append new-lst (car lst)) (+ cnt 1) idx e)]))

; lst[idx] = e
; Note: no bounds checking done, use with caution
(define (lst-set lst idx e)
  (lst-set-rec lst '() 0 idx e))

; Instead, check first if the stmt is being assigned to a function
; If it is, simply set its value to that function's name.
; This will also work for any assignments to variables holding functions.
; Otherwise, resolve as an arith expr. In sem-funccall, we'll handle resolving variables.
(define (get-assign-tuple stmt env)
  (list (cadr stmt) (sem-arithexpr (cadr (cdr stmt)) env)))

(define (sem-stmt-assign stmt env)
  (lst-set env (lst-find env (lambda (k) (equal? (car k) (cadr stmt)))) (get-assign-tuple stmt env)))

; Need to fix this too -- should return new environment / heap. Probably could've popped the stack here.
(define (sem-stmt-if stmt env heap)
  (if (sem-condexpr (cadr stmt) env)
      (sem-sseq (cadr (cdr stmt)) env heap)
      (list env heap)))

; The only real issue is dealing with funcall
; Introduce a conditional that if we have a funcall, make funcall return (env, heap),
; pass it via a lambda, and then call sem-sseq with the proper params? 
(define (sem-sseq sseq env heap)
  (if (null? sseq)
      (list env heap)
      (cond
        [(or (equal? heap '(oom)) (equal? heap '(fma)) (equal? heap '(ooma))) (list env heap)]
        [(equal? (car (car sseq)) 'call) ((lambda (output) (sem-sseq (cdr sseq) (car output) (cadr output)))
                                          (sem-funcall (car (cdr (car sseq))) env heap))]
        [(equal? (car (car sseq)) 'if) ((lambda (output) (sem-sseq (cdr sseq) (stack-pop-n (car output) 
                                                                              (- (length (car output)) (length env))) 
                                                                   (cadr output)))
                                        (sem-stmt-if (car sseq) env heap))]
        ; Need to utilize lambdas on all of these to move output onward
        [(equal? (car (car sseq)) 'deref) ((lambda (output) 
                                             (sem-sseq (cdr sseq) (car output) (cadr output)))
                                           (sem-stmt-deref (car sseq) env heap))]
        [(equal? (car (car sseq)) 'free) ((lambda (output)
                                           (sem-sseq (cdr sseq) (car output) (cadr output)))
                                         (sem-stmt-free (car sseq) env heap))]
        [(equal? (car (car sseq)) 'ref) ((lambda (output)
                                           (sem-sseq (cdr sseq) (car output) (cadr output)))
                                        (sem-stmt-ref (car sseq) env heap))]
        [(equal? (car (car sseq)) 'wref) ((lambda (output)
                                            (sem-sseq (cdr sseq) (car output) (cadr output)))
                                         (sem-stmt-wref (car sseq) env heap))]
        [else (sem-sseq (cdr sseq) 
                (cond
                    [(equal? (car (car sseq)) 'decl) (stack-push env (sem-stmt-decl (car sseq)))]
                    [(equal? (car (car sseq)) 'assign) (sem-stmt-assign (car sseq) env)]
                    ;[(equal? (car (car sseq)) 'call) (sem-funcall (car (cdr (car sseq))) env heap)] ; need special handling
                    [(equal? (car (car sseq)) 'fundecl) (sem-fundecl (car sseq) env)]
                    [else env]) heap)])))
            ; Keep only changes to the original env (those variables introduced outside of the scope)
            ; See https://piazza.com/class/jqn2z64aro460t?cid=352
            ;[(equal? (car (car sseq)) 'if) ((lambda (new-env) 
            ;                             (stack-pop-n new-env (- (length new-env) (length env))))
            ;                             (sem-stmt-if (car sseq) env))]) heap)])))

; Order of output may differ from that of tests -- see https://piazza.com/class/jqn2z64aro460t?cid=361
(define (sem prog env heap)
  (sem-sseq prog env heap))

; A function is defined with a parameter list. When that function is called with arguments,
; a mapping between the arguments and parameter names (pname, arg) must be pushed onto the stack.
; Then, the function definition block should be executed with this new environment.

; Function definitions are also pushed onto the stack in order of definition. Therefore, if two functions
; have the same name, then the most recently defined will be utilized.

; Does recursion work in this language?

; Begin by simply making fundecl modify the environment appropriately
; fundecl -> ((FName ParamList) (SSeq))
(define (sem-fundecl fundecl env)
  (stack-push env (cdr fundecl)))

(define (stack-push-lst stack lst)
  (if (null? lst)
      stack
      (stack-push-lst (stack-push stack (car lst)) (cdr lst))))

(define (my-map-rec f lst res)
  (if (null? lst)
      res
      (my-map-rec f (cdr lst) (lst-append res (f (car lst)))))) 

(define (my-map f lst)
  (my-map-rec f lst '()))

; funcall -> (FName (Args))
; Resolve semantics of arglist (maybe perform a map on args?)
; Push args mapping onto env 
; Locate FName on the stack - the env we want to pass to sem(SSeq) is env including arglist & function
; Call it's body utilizing the now mapped env
; Also, aren't we essentially already utilizing dynamic scoping?

(define (sem-funcall fcall env heap)
  ; We really only want to return modifications made to items on the stack;
  ; This can be accomplished by simply popping off length(nenv) - length(env) items,
  ; and returning it. sem-sseq should give us a stack back.
  ; This returns our new env
  ; Needs cleaned up too
  (if (equal? (get-env-func (car fcall) env) '())
      (sem-funcall (list (sem-arithexpr (car fcall) env) (cadr fcall)) env heap) 
      ((lambda (output)
        (list (stack-pop-n (car output) (- (length (car output)) (length env))) (cadr output)))
      (sem-sseq (cadr (get-env-func (car fcall) env)) (push-args (cadr (car (get-env-func (car fcall) env))) (cadr fcall) env) heap))))

; Needs to map according to parameter list
(define (push-args plist args env)
  (stack-push-lst env (zip plist (my-map (lambda (k) (sem-arithexpr k env)) args))))

; return {(lst1[i], lst2[j]) where i=j and len(lst1) = len(lst2)}
(define (zip-rec lst1 lst2 ret)
  (if (null? lst1)
      ret
      (zip-rec (cdr lst1) (cdr lst2) (lst-append ret (list (car lst1) (car lst2))))))

(define (zip lst1 lst2)
  (zip-rec lst1 lst2 '()))

; Checks if lst == ((fname (paramlist)) (sseq)), kind of a synchk
; Probably not perfect
(define (is-function? lst)
  (cond
    [(null? lst) #f]
    [(not (equal? (length lst) 2)) #f]
    [(not (and (list? (car lst)) (equal? (length (car lst)) 2) (list? (cadr (car lst))))) #f]
    [(not (list? (cadr lst))) #f]
    [else (and (symbol? (car (car lst))) (list? (cadr (car lst))) (list? (cadr lst)))]))

(define (func-eq fname)
    (lambda (k)
        (and (is-function? k) (equal? (car (car k)) fname))))

(define (get-env-func fname env)
  ((lambda (func-idx)
    (if (not (equal? -1 func-idx))
        (lst-get env func-idx)
        '())) (lst-find env (func-eq fname))))

; expr when resolved is the memory location to free
; expr needs to be resolved as an arithexpr, of course
; maybe pass into the lambda
(define (free expr env heap)
  ((lambda (idx)
    (if (equal? -1 idx)
      (list env '(ooma))    ; Return env with ooma as the heap
      (list env (lst-set heap idx (list expr 'free))))) (lst-find heap (lambda (k) (equal? (car k) expr)))))

; stmt = '(deref X arithexpr)
(define (sem-stmt-deref stmt env heap)
  (deref (cadr stmt) (sem-arithexpr (cadr (cdr stmt)) env) env heap))

; stmt = (free arithexpr)
(define (sem-stmt-free stmt env heap)
  (free (sem-arithexpr (cadr stmt) env) env heap))

; stmt = (ref var arithexpr)
(define (sem-stmt-ref stmt env heap)
  (ref (cadr stmt) (sem-arithexpr (cadr (cdr stmt)) env) env heap))

; stmt = (wref arithexpr arithexpr)
(define (sem-stmt-wref stmt env heap)
  (wref (sem-arithexpr (cadr stmt) env) (sem-arithexpr (cadr (cdr stmt)) env) env heap))

(define (deref var expr env heap)
  ((lambda (idx)
    (if (equal? idx -1)
        (list env '(ooma))
        ((lambda (heap-item)
          (if (equal? (cadr heap-item) 'free)
              (list env '(fma))
              ; Find variable idx in env, set its value to heap value expr
              ; Still need to return this list and env
              ; Might help to break this down a bit -- not too clean
              (list (lst-set env (lst-find env (lambda (k) (equal? (car k) var))) (list var (cadr heap-item))) heap))) 
                (lst-get heap idx)))) (lst-find heap (lambda (k) (equal? (car k) expr)))))

(define (ref var expr env heap)
  ((lambda (free-idx) ; Repeat the pattern you used earlier, where a lambda holds the heap-idx and heap-item.
    (if (equal? -1 free-idx)
        (list env '(oom))
        ((lambda (heap-item)
            (list (lst-set env (lst-find env (lambda (k) (equal? (car k) var))) (list var (car heap-item))) 
                  (lst-set heap free-idx (list (car heap-item) expr))))
        (lst-get heap free-idx)))) 
   (lst-find heap (lambda (k) (equal? (cadr k) 'free)))))

(define (wref addr val env heap)
  ((lambda (idx)
    (if (equal? -1 idx)
      (list env '(ooma))
      ((lambda (heap-item)
        (if (equal? 'free (cadr heap-item))
          (list env '(fma))
          (list env (lst-set heap idx (list (car heap-item) val)))))
      (lst-get heap idx))))
   (lst-find heap (lambda (k) (equal? (car k) addr)))))
