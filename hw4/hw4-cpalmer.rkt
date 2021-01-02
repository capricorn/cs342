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
        ; Keep only changes to the original env (implying variables introducted outside of the scope)
        ; See https://piazza.com/class/jqn2z64aro460t?cid=352
        [(equal? (car (car sseq)) 'if) ((lambda (new-env) 
                                         (stack-pop-n new-env (- (length new-env) (length env))))
                                         (sem-stmt-if (car sseq) env))]))))

; Order of output may differ from that of tests -- see https://piazza.com/class/jqn2z64aro460t?cid=361
(define (sem prog env)
  (sem-sseq prog env))
