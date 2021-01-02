#lang racket

(require "hw4.rkt")

(define program1
      '(
        (decl x)
        (assign x 3)
        (decl y)
        (assign y 10)
        (if (gt x 2)
            (
             (decl x)
             (assign x y)
             (assign x (+ x y))
) )
        (assign x (+ x 1))
) )

(define program2
  '(
    (decl x)
    (decl z)
    (assign x (+ y 1))
    (if (gt x 1)
        ((assign z 1)))
    (if (gt x 2)
        ((assign z 2)))))

(define program3
  '(
    (decl x)
    (if (gt y 1)
        (
            (assign x (* y 2))
            (if (gt x 2)
                (
                    (assign y 15)
                ))
        ))
    ))

(define square_prog
  '(
    (assign x (* x x))))

(define program4
  '(
    (if (gt x 4)
        (
            (decl y)
            (assign y 4)
            (if (gt (+ x y) 3)
                (
                    (decl z)
                    (assign z 7)
                    (if (gt (* z y) 3)
                        (
                            (assign x 1)
                        ))
                ))
        ))
     (assign x 7)
   ))

(define program5
  '(
        (assign x 5)
        (decl x)
   ))
;(synchk program1)
;(synchk program2)
;(sem-op '(- 1 2))
;(sem-bcond '(lt 1 2))
;(sem-stmt-decl '(decl x 3))
;(sem-sseq '((decl y) (decl x)) '())
;(lst-set '(1 2 3) 0 4)
;(lst-append '(1 2 3) 4)
; We need to handle variables in arithmetic expressions
; question: should assign affect an input environment variable?
;(sem-stmt-assign '(assign x (* y 3)) '((y 2) (x 0)))
;(sem-sseq '((decl y) (assign y 2) (decl x) (assign x (+ y 3))) '())
;(sem-condexpr '((gt 3 2)) '())
;(sem-stmt-if '(if (gt 2 1) ((decl x))) '((y 3)))

; TODO Make sure synchk is still correct on bcond!
(synchk program1)
;(synchk program3)
;(sem program1 '())
;(sem program1 '((x 20)))
(sem program2 '((y 10)))
;(sem program2 '((y 0)))
(sem program3 '((y 2)))
;(synchk '((decl 1)))
;(synchk square_prog)
;(sem square_prog '((x 8)))
;(synchk '((if (gt 3 1) ((decl x)))))
;(synchk program4)
;(sem program4 '((x 4)))
;(sem program5 '((x 0)))
;(synchk '((if (gt 3 2) ((decl x)))))
;(synchk '((assign x 1 1)))
;(sem '((assign x 1 1)))
;(sem '(
;       (decl x)
;       (assign x 3)
;       (decl y)
;       (assign y 10)
;) '())

;(synchk '(7 6 3 a))
;(synchk '((decl b) (decl a)))
;(synchk '((assign b 3) (decl a)))
;(synchk '((assign b (* 3 (* 2 ()))) (if 3 (f))))

