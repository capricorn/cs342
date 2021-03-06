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

(define (sem-stmt-if stmt env)
  (if (sem-condexpr (cadr stmt) env)
      (sem-sseq (cadr (cdr stmt)) env)
      env))

(define (sem-sseq sseq env)
  (if (null? sseq)
      env
      (sem-sseq (cdr sseq) (cond
        [(equal? (car (car sseq)) 'decl) (stack-push env (sem-stmt-decl (car sseq)))]
        [(equal? (car (car sseq)) 'assign) (sem-stmt-assign (car sseq) env)]
        [(equal? (car (car sseq)) 'call) (sem-funcall (car (cdr (car sseq))) env)]
        [(equal? (car (car sseq)) 'fundecl) (sem-fundecl (car sseq) env)]
        ; Keep only changes to the original env (implying variables introducted outside of the scope)
        ; See https://piazza.com/class/jqn2z64aro460t?cid=352
        [(equal? (car (car sseq)) 'if) ((lambda (new-env) 
                                         (stack-pop-n new-env (- (length new-env) (length env))))
                                         (sem-stmt-if (car sseq) env))]))))

; Order of output may differ from that of tests -- see https://piazza.com/class/jqn2z64aro460t?cid=361
(define (sem prog env)
  (sem-sseq prog env))

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

(define (sem-funcall fcall env)
  ; We really only want to return modifications made to items on the stack;
  ; This can be accomplished by simply popping off length(nenv) - length(env) items,
  ; and returning it. sem-sseq should give us a stack back.
  ; This returns our new env
  ; Needs cleaned up too
  (if (equal? (get-env-func (car fcall) env) '())
      (sem-funcall (list (sem-arithexpr (car fcall) env) (cadr fcall)) env) 
      ((lambda (new-env)
        (stack-pop-n new-env (- (length new-env) (length env))))
      (sem-sseq (cadr (get-env-func (car fcall) env)) (push-args (cadr (car (get-env-func (car fcall) env))) (cadr fcall) env)))))

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

;(sem-fundecl '((f (x y)) (+ x y)) '((x 3)))
;(stack-push-lst '() '(1 2 3))
;(my-map (lambda (x) (+ x 1)) '(1 2 3))
;(push-args '((+ 3 1) (* 1 2) (+ 3 4)) '()) ; actually needs to take parameter names into account
;(is-function? '((f (x y)) (+ x y)))

; env test for locating function
; our lst-find is a bit odd -- hardcodes the value in the function
;(define my-env '( (x 3) (y 2) ((f (x y)) (+ x y))))
;(define my-env '( (x 3) (y 2) ))
;(define my-env '())
; We need to find a function by name -- here it's hardcoded
;(lst-get my-env (lst-find my-env (func-eq 'f)))
;(get-env-func 'f)
;(sem-funcall '(f (1 2)) my-env)
;(lst-get (lst-find my-env is-function?) my-env)
;(println "test")
;(call (f (1 2)))
;(push-args '(x y z) '(1 2 3) '())
;(zip '(1 2 3) '(a b c))

#|
(define p0
  '(
    (fundecl (f (x)) (
                      (assign y (+ x 1)))
                     )
    (decl y)
    (call (f (0)))))

(define p1
  '(
     (fundecl (f (x)) (
        (assign y (+ x 1))
     ))
     (decl y)
     (decl z)
     (assign z f)
     (call (z (0)))
   ))

(define p2
  '(
     (decl y)
     (decl z)
     (assign z f)
     (call (z (0)))
   ))

; Test for calling function mapped to variable name
(define p3
  '(
     (decl x)
     (decl y)
     (decl q)
     ; Functions in this language are side-effects only
     (fundecl (f (z)) (
        (assign q (* z z))
     ))
     (assign x f)
     (assign y x)
     (call (y (2)))
   ))

; recursion - factorial function
(define p4 
  '(
     (decl z)
     (assign z 1)
     (fundecl (f (x)) (
       (assign z (* z x))
       (if (gt x 1) (
         (call (f ((- x 1))))
       ))
     ))
     (call (f (n)))
   ))

; Calling nested functions of the same name -- and playing around with scope
(define p5
  '(
     (decl z)
     (decl x)
     (fundecl (f (x)) (
       (decl x)
       (fundecl (f (x)) (
         (assign z 1)
       ))
       ; Interesting question: If I call f here, which should be called?
       ; In dynamic scoping, it should call the above function f, and assign z to 1.
       (call (f (10)))
     ))
     (call (f (10)))
   ))

; In this case, call searches the current block context and resolves f if it is actually a function.
; Doesn't seem like this case is exactly specified by the provided semantics
(define p6
  '(
     (decl z)
     (fundecl (f (x)) (
       (assign z x)
     ))
     (decl f)
     (call (f (3)))
   ))

(define p7
  '(
     (decl q)
     (fundecl (f (x y z)) (
       (assign q (+ z (+ x y)))
     ))
     (call (f (1 2 3)))
   ))

; Empty function call
(define p8
  '(
     (decl z)
     (fundecl (f ()) (
       (assign z 3)
     ))
     (call (f ()))
   ))

(println "P0")
(sem p0 '())
(println "P1")
(sem p1 '())
(println "P2")
(sem p2 '(((f (x)) ((assign y (+ x 1))))))
(println "P3")
(sem p3 '())
(println "P4")
(sem p4 '((n 5)))
(println "P5")
(sem p5 '())
(println "P6")
(sem p6 '())
(println "P7")
(sem p7 '())
(println "P8")
(sem p8 '())
|#
