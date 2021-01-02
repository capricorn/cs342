#|
(free 1 '() '((1 x)))
(deref 'x 2 '((x 2)) '((2 4)))
(deref 'x 3 '((x 2)) '((2 4)))
(deref 'x 2 '((x 2)) '((2 free)))

(ref 'x 3 '((x 9)) '((1 free)))
(ref 'x 3 '((x 9)) '((2 1) (3 free)))

(wref 2 1 '() '((3 2)))
(wref 2 1 '((y 7) (z 2)) '((3 2)))
(wref 3 1 '() '((3 free)))
(wref 3 1 '((x 3)) '((3 free)))
(wref 2 5 '((y 7)) '((2 3)))
|#

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
(sem p0 '() '())
(println "P1")
(sem p1 '() '())
(println "P2")
(sem p2 '(((f (x)) ((assign y (+ x 1))))) '())
(println "P3")
(sem p3 '() '())
(println "P4")
(sem p4 '((n 5)) '())
(println "P5")
(sem p5 '() '())
(println "P6")
(sem p6 '() '())
(println "P7")
(sem p7 '() '())
(println "P8")
(sem p8 '() '())

(define ex0 '(
      (decl x)
      (decl y)
      (ref x 10)
      (deref y x)
))

(sem ex0 '() '((1 free) (2 free)))
(sem ex0 '() '((1 20) (2 free)))
(sem ex0 '() '((1 20) (2 40)))

(define ex2 
  '(
      (decl x)
      (decl y)
      (ref x 10)
      (wref x 30)
      (deref y x)
      (free x)
))

(sem ex2 '() '((1 free) (2 free)))

(define ex5
  '(
     (decl x)
     (assign x 0)
     (free x)
))

(sem ex5 '() '((1 free)))

(define ex6
  '(
    (decl x)
    (deref x 1))
)

(sem ex6 '() '((1 free)))

(define ex-swap
  '(
     (fundecl (swap (x y)) (
        (decl temp1)
        (decl temp2)
        (deref temp1 x)
        (deref temp2 y)
        (wref x temp2)
        (wref y temp1)
     ))
     (decl a)
     (decl b)
     (assign a 1)
     (assign b 2)
     (call (swap (a b)))
) )

(sem ex-swap '() '((1 20) (2 500)))

(define ex-if
  '(
    (decl x)
    (decl z)
    (ref x 5)
    (if (gt y 5) (
     (deref z x)))
   ))

(sem ex-if '((y 6)) '((1 free)))
|#
