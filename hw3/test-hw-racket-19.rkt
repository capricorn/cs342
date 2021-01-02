#lang racket
;(require "sol-hw-racket-19.rkt")
(require racket/trace)
(require "DB.rkt")
;(require "hw3-cpalmer.rkt")
(require "hw3-cpalmer-cpy2.rkt")


(define (myfunc lst pos)
    (if (null? lst)
        lst
        (if (> pos 0)
            (myfunc (cdr lst) (- pos 1))
            (car lst)))) 

(provide (all-defined-out)) 

(define (myequal lst1 lst2) 
  (cond
    [(and (null? lst1) (null? lst2)) true ] ;; both lists are empty
    [(and (null? lst1) (not (null? lst2))) false ]
    [(and (not (null? lst1)) (null? lst2)) false ] 
    [(present (car lst1) lst2) (myequal (cdr lst1) (remove (car lst1) lst2)) ]
    [ else false ]))

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))



;; does some specific course present in the course list
(define (present x lst)
  (if (null? lst)
      false
      (if (equal? x (car lst))
          true
          (present x (cdr lst)))))

(define inplan
  (lambda (course)
    (lambda (student-record)
      (list (car student-record) (cadr student-record) (present course (cadr (cdr student-record)))))))


(define totalpoints 100) ;; total points for this assignment
(define cnt 0)  ;; test counts

(define (utest testcnt testname testfun testpoints)
  (begin
    (write testcnt)
    (write testname)
    (write ':)
    (write testpoints)
    (writeln 'pts)
    (with-handlers ([exn:fail? (lambda (exn)
                                 (begin
                                   (writeln exn)
                                   (writeln "Exception")
                                   (writeln "incorrect")
                                   (set! totalpoints (- totalpoints testpoints))
                                   ))])
      (if (eval testfun ns)
          (writeln "correct")
          (begin
            (writeln "incorrect output")
            (set! totalpoints (- totalpoints testpoints))))
    )
    ))

(define (hw2)
  (begin

    (writeln '************************************************)
    (writeln 'Tests-on-Q1)
    (writeln '************************************************)

    
    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt ':ptComparator/ptSort '(equal? (ptsort mptlist1 ptcomparator) sorted1) 2)
                               

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':newComparator/ptSort '(equal? (ptsort mptlist2 newcomparator) sorted2) 3)
    
    (writeln '--------------)
    (set! cnt (+ cnt 1))
   
    (utest cnt ':getcen1 '(or (equal? (getcen1 mptlist1) cen1frac)
                          (equal? (getcen1 mptlist1) cen1decimal)) 2.5)

    (writeln '--------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt ':getcen2 '(or (equal? (getcen2 mptlist1) cen1a)
                           (equal? (getcen2 mptlist1) cen1b)) 2.5)


    (writeln '--------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt ": (classify ptlist1 '(1 0) '(6 5) true)" '(equal? (myroundptlist (sort (classify mptlist1 '(1 0) '(6 5) true) ptcomparator)) classifytrue1b) 7)
        
   

    (writeln "--------------")
    (set! cnt (+ cnt 1))
    
    (utest cnt ": (kclassify ptlist1 '(1 0) '(6 5) true 3)" '(equal? (myroundptlist (sort (kclassify mptlist1 '(1 0) '(6 5) true 3) ptcomparator)) kclassifyfalse1b) 8)


    (writeln '--------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt ": (kclassify ptlist1 '(1 0) '(6 5) false 3)" '(equal? (sort (kclassify mptlist1 '(1 0) '(6 5) false 3) ptcomparator) '((6 5) (19 1))) 8)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ": (nclassify ptlist3 true 10)" '(equal? (myroundptlist (sort (nclassify mptlist3 true 10) ptcomparator)) nclasstrue) 10)
        
    (writeln '--------------)
    (set! cnt (+ cnt 1))
    ;; multiple equivalent solutions
    (utest cnt ": (nclassify ptlist3 false 10)" '(or (equal? (sort (nclassify mptlist3  false 10) ptcomparator) nclassfalse1)
                                                       (equal? (sort (nclassify mptlist3  false 10) ptcomparator) nclassfalse2)
                                                       (equal? (sort (nclassify mptlist3  false 10) ptcomparator) nclassfalse3)
                                                       (equal? (sort (nclassify mptlist3  false 10) ptcomparator) nclassfalse4))  10)
    
    (writeln '************************************************)
    (writeln 'Tests-on-Q2)
    (writeln '************************************************)
    
    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt ": ((applyonstds student-table)(lambda(x) (cadr x)))" '(myequal ((applyonstds mstudent-table)(lambda(x) (cadr x))) '(Asterix Obelix Getafix Cacofonix)) 5)
        
    (writeln '-------------)
    (set! cnt (+ cnt 1))
   
    (utest cnt ": ((applyonstds student-table) (inplan 342))" '(myequal ((applyonstds mstudent-table) (inplan 342)) '((0 Asterix #t) (1 Obelix #t) (2 Getafix #f) (3 Cacofonix #f))) 10)
        
    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt ": (numberofcourses (cadr student-table))" '(myequal (numberofcourses (cadr mstudent-table)) '(1 Obelix 4)) 5)
    
    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest  cnt ": ((applyonstds student-table) numberofcourses)" '(myequal ((applyonstds mstudent-table) numberofcourses) '((0 Asterix 4) (1 Obelix 4) (2 Getafix 5) (3 Cacofonix 5))) 8)
    

    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt ": ((applyonstds student-table) (studentgpa grade-table))"
             '(myequal (stdtabretround ((applyonstds mstudent-table) (studentgpa mgrade-table))) '((0 Asterix 3.67) (1 Obelix 3.0) (2 Getafix 0) (3 Cacofonix 3.0))) 9)
        

    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt ":((applyonstds student-table) (studentgpa grade-table1))"
             '(myequal (stdtabretround ((applyonstds mstudent-table) (studentgpa mgrade-table1))) '((0 Asterix 4.0) (1 Obelix 3.0) (2 Getafix 3.0) (3 Cacofonix 2.67))) 10)
 

    (writeln '---------------------------------------)
    (write "                      Total Points: ")
    (writeln totalpoints)

    
    )
)

(hw2)

