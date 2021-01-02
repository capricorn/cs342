#lang racket

(provide (all-defined-out))
(require racket/trace)
(require "DB.rkt")

; Potential tests that need done / checked:
; - empty lists
; - definition of "distinct" points
;   - if this means "different" then things are correct
;   - Otherwise, if a list is passed with all equal points, then the program will not terminate (see pick-two-points)
; - negative numbers?
; - It should be possible to get an empty partition when using new centroids as p1 and p2.. 
; - avoid division by zero?

; l - a list
; returns: a random element from list l
(define (pick l)
  (pickat l (* (length l) (random))))

; l - a list
; posval - some number [0, (length l) - 1]
; returns: the element in l at position posval
(define (pickat l posval)
  (if (< posval 1)
      (car l)
      (pickat (cdr l) (- posval 1))))

; p1 - a 2d coordinate (x y)
; p2 - a 2d coordinate (x y)
; returns: euclidean distance between p1 and p2 (result left squared)
(define (distance p1 p2)
  (+ (* (- (car p2) (car p1)) (- (car p2) (car p1))) (* (- (cadr p2) (cadr p1)) (- (cadr p2) (cadr p1)))))

; p1 - a 2d coordinate (x y)
; p2 - a 2d coordinate (x y)
; returns: true if p1 is closer to (0 0) than p2 is
(define (ptcomparator p1 p2)
  (< (distance '(0 0) p1) (distance '(0 0) p2)))

; lst - list of 2d coordinates
; comp - binary comparator that takes a single argument and returns #t/#f
; part - list of elements according to comp, with a list for true and a list for false ((true-elements) (false-elements))
; returns: part
(define (bin-partition-rec lst comp part)
  (if (null? lst)
      part
      (if (comp (car lst)) 
          (bin-partition-rec (cdr lst) comp (list (lst-append (car part) (car lst)) (cadr part)))  ; append to first pair
          (bin-partition-rec (cdr lst) comp (list (car part) (lst-append (cadr part) (car lst))))))) ; append to second pair

; lst - list of 2d coordinates
; comp - binary comparator that takes a single argument and returns #t/#f
; returns: lst partitioned according to comp
(define (bin-partition lst comp)
  (bin-partition-rec lst comp '(() ())))

; lst - A list of 2d coordinates
; comp - a comparator that takes 2 inputs, and returns #t/#f
; returns: sorted list, according to comp
(define (ptsort lst comp)
  (select-sort lst comp))

; lst - a list
; e - some element
; returns: e appended to lst
(define (lst-append lst e)
  (cond
    [(equal? (length lst) 0) (list e)]
    [(equal? (length lst) 1) (cons (car lst) (list e))]
    [else (cons (car lst) (lst-append (cdr lst) e))]))

; For some reason this implementation seems wrong
(define (lst-min-rec lst cmp m)
  (if (null? lst)
      m
      (if (cmp (car lst) m) ; if element < m
          (lst-min-rec (cdr lst) cmp (car lst))
          (lst-min-rec (cdr lst) cmp m))))

(define (lst-min lst cmp)
  (lst-min-rec lst cmp (car lst)))

; lst - a list
; e - element to my-removefrom the list
; returns: lst with the first occurrence of e removed, or lst if e not in lst.
(define (my-remove lst e)
  (remove-rec lst '() e #f))

; lst - a list
; nlst - a temporary list for elements kept from lst
; e - element to my-removefrom the list
; found - boolean representing whether e was found or not
; returns: lst with the first occurrence of e removed, or lst if e not in lst.
(define (remove-rec lst nlst e found)
  (if (null? lst)
      nlst
      (if (and (equal? (car lst) e) (not found))
          (remove-rec (cdr lst) nlst e #t)
          (remove-rec (cdr lst) (lst-append nlst (car lst)) e found))))

; You could use a lambda here to avoid recomputing lst-min
; lst - a list
; cmp - a < comparator that takes two inputs, and returns a boolean
; returns: a pair (x y), where x is the minimum element, and y is the list excluding x
(define (remove-min lst cmp)
  (list (lst-min lst cmp) (my-remove lst (lst-min lst cmp))))

; lst - a list
; cmp - a comparator that takes 2 inputs, and returns a boolean
; sorted - the temporary sorted list
; returns: lst sorted according to cmp
(define (select-sort-rec lst cmp sorted)
  (if (null? lst)
      sorted
      (select-sort-rec (cadr (remove-min lst cmp)) cmp (lst-append sorted (lst-min lst cmp)))))

; lst - a list
; cmp - a comparator that takes 2 inputs, and returns a boolean
; returns: lst sorted according to cmp
(define (select-sort lst cmp)
    (select-sort-rec lst cmp '()))

; lst - a list of numbers
; tally - current temporary sum
; returns: sum of elements in lst
(define (sum-rec lst tally)
  (if (null? lst)
      tally
      (sum-rec (cdr lst) (+ (car lst) tally ))))

; lst - a list of numbers
; returns: sum of elements in lst
(define (sum lst)
  (sum-rec lst 0))

; lst - a list of numbers
; returns: average of numbers in lst
(define (average lst)
  (/ (sum lst) (length lst)))

; lst - a list
; f - a function that takes a single input, and returns a single output
; res - temporary list of results from applying f to lst
; returns: lst, where every element e becomes f(e)
(define (lst-map-rec lst f res)
  (if (null? lst)
      res
      (lst-map-rec (cdr lst) f (lst-append res (f (car lst))))))

; lst - a list
; f - a function that takes a single input, and returns a single output
; returns: lst, where every element e becomes f(e)
(define (lst-map lst f)
    (lst-map-rec lst f '()))

; Note: Results not floored, as per piazza post https://piazza.com/class/jqn2z64aro460t?cid=170
; pts - list of 2d coordinates (x y)
; returns: pair (a b), where a is the average of all x coords in pts, and b the average of all y coords in pts
(define (getcen1 pts)
  (list (/ (sum (lst-map pts (lambda (pt) (car pt)))) (length pts)) (/ (sum (lst-map pts (lambda (pt) (cadr pt)))) (length pts))))

; pts - a list of 2d coordinates (x y)
; returns: pair (a b), which is the median point of sorted pts
(define (getcen2 pts)
  (median (ptsort pts ptcomparator)))

; Note: Selected median according to piazza post https://piazza.com/class/jqn2z64aro460t?cid=99
; lst - a list
; returns: median value of lst
(define (median lst)
  (pickat lst (floor (- (/ (+ (length lst) 1) 2) 1))))

; pt - base 2d coordinate to measure from
; a - 2d coordinate
; b - 2d coordinate
; returns: true if pt is closer to a than b by euclidean distance, false otherwise
(define (closer-pt pt a b)
  (< (distance pt a) (distance pt b)))

; lst - list of 2d coordinates
; p1 - some point, st p1 != p2
; p2 - some point, st p2 != p1
; flag - boolean indicating which centroid function to use.
;   true = getcen1()
;   false = getcen2()
; returns: a pair (a b), where a and b are centroids corresponding to the two separate partitions of lst
(define (classify lst p1 p2 flag)
  (define part (partition lst p1 p2))
  (if flag
    (list (getcen1 (car part)) (getcen1 (cadr part)))
    (list (getcen2 (ptsort (car part) ptcomparator)) (getcen2 (ptsort (cadr part) ptcomparator)))))

; pts - a list of 2d coordinates
; a - a 2d coordinate
; b - a 2d coordinate
; returns: pair (a b), where 'a' and 'b' are lists, with 'a' containing all points in pts
;   closer to 'a' than 'b', and 'b' containing the remaining points.
(define (partition pts a b)
    (bin-partition pts (lambda (k) (closer-pt k a b))))

; lst - a list of 2d coordinates
; p1 - a 2d coordinate, where p1 != p2 
; p2 - a 2d coordinate, where p2 != p1
; flag - boolean indicating which centroid function to use.
;   true = getcen1()
;   false = getcen2()
; k - Number of iterations to run function classify on arguments
; returns: pair (a b), where a and b are centroids as calculated by classify,
;   except these centroids are then passed as the new p1 and p2, until k
;   iterations have occurred.
(define (kclassify lst p1 p2 flag k)
  (if (equal? k 0)
      (list p1 p2)
      (kclassify lst (car (classify lst p1 p2 flag)) (cadr (classify lst p1 p2 flag)) flag (- k 1))))

; lst - list of 2d coordinates
; p1 - a 2d coordinate, where p1 != p2
; p2 - a 2d coordinate, where p2 != p1
; flag - boolean indicating which centroid function to use.
;   true = getcen1()
;   false = getcen2()
; k - Number of iterations to run function classify on arguments
; returns: pair (a b), the results of function kclassify
(define (nclassify-rec lst p1 p2 flag k)
  (if (equal? p1 p2)
      (nclassify-rec lst (pick lst) (pick lst) flag k)
      (kclassify lst p1 p2 flag k)))

; lst - list of 2d coordinates
; flag - boolean indicating which centroid function to use.
;   true = getcen1()
;   false = getcen2()
; k - Number of iterations to run function classify on arguments
; returns: pair (a b), the results of function kclassify
(define (nclassify lst flag k)
    (nclassify-rec lst (pick lst) (pick lst) flag k))



; === part 2 of assignment ===



; lambda table - student table
; lambda f - function to apply to each record in the table
; returns: student table, where every e in table = f(e)
(define applyonstds
  (lambda (table)
    (lambda (f)
      (lst-map table f))))

; lambda record - a student record
; returns: the initial student record, where course-plan is replaced with (length course-plan)
(define numberofcourses
  (lambda (record)
    (list (car record) (cadr record) (length (cadr (cdr record))))))

; lambda gtable - grade table
; lambda record - student record
; returns: a student record, where course-plan is replaced with the student's gpa
(define studentgpa
  (lambda (gtable)
    (lambda (record)
      (list (car record) (cadr record) (get-gpa gtable record)))))

; g-table - grade table
; record - a student record
; returns: average gpa of the student described by record
(define (get-gpa g-table record)
  (average (get-grade-lst g-table record)))
#|
(define (get-gpa g-table record)
  ((lambda (grade-lst)
    (if (null? grade-lst)
        0
        (average grade-lst))) (get-grade-lst g-table record)))
|#

; g-table - grade table
; id - student id
; course-lst - course list from a student record
; grade-lst - temporary list of grades associated with a student's course list
; returns: list of numerical grades of the student described by record
(define (get-grade-lst-rec g-table id course-lst grade-lst)
  (if (null? course-lst)
      grade-lst
      ((lambda (grade)
        (if (equal? grade '())
            (get-grade-lst-rec g-table id (cdr course-lst) grade-lst)
            (get-grade-lst-rec g-table id (cdr course-lst) (cons grade grade-lst)))) (find-grade g-table (car course-lst) id))))

      ;((lambda (grade) find-grade)) ; Idea to avoid extra calls, and technically 'declare' a variable
      ;(get-grade-lst-rec g-table id (cdr course-lst) (cons (find-grade g-table (car course-lst) id) grade-lst))))

; g-table - grade table
; record - student record
; returns: list of numerical grades from student described by record
(define (get-grade-lst g-table record)
  (get-grade-lst-rec g-table (car record) (cadr (cdr record)) '()))

; grade
; returns: numerical interpretation according to grade symbol 
(define (grade-to-gpa grade)
  (cond
    [(equal? grade 'A) 4]
    [(equal? grade 'B) 3]
    [(equal? grade 'C) 2]
    [(equal? grade 'D) 1]
    [(equal? grade 'F) 0]))

; Note: If a grade doesn't exist, it is returned as an empty list, which later on is removed when calculating the average.
; table - grade table
; course - course number
; id - student id
; returns: grade of a student id in a given course.
(define (find-grade table course id)
  (if (null? table)
      '()
      (if (and (equal? (car (car table)) course) (equal? (cadr (car table)) id))
          (grade-to-gpa (cadr (cdr (car table))))
          (find-grade (cdr table) course id))))

; f - lambda-wrapped function that takes no inputs, and returns its output
; res - result to test f() against
; desc - name of function tested
; returns: nothing, outputs if the test passed or not.
(define (test f res desc)
  (if (equal? (f) res)
      (printf "Passed test: ~a = ~s\n" desc res)
      (printf "Failed test: ~a != ~s\n" desc res)))

; == tests for problem 1 ==

#|
(test (lambda () (distance '(1 2) '(3 4))) 8 "distance 1")
(test (lambda () (distance '(1 -2) '(-5 4))) 72 "distance 1")

(test (lambda () (ptcomparator '(1 2) '(3 4))) #t "ptcomparator 1")
(test (lambda () (ptcomparator '(1 2) '(1 2))) #f "ptcomparator 2")
(test (lambda () (ptcomparator '(1 -2) '(-5 4))) #t "ptcomparator 3")

(test (lambda () (lst-min '(4 2 3 4) <)) 2 "lst-min 1")
(test (lambda () (my-remove'(1 2 3 3) 1)) '(2 3 3) "my-remove1")
(test (lambda () (remove-min '(1 2 3 3) <)) '(1 (2 3 3)) "remove-min 1")
(test (lambda () (select-sort '(1 4 2 3 7 1 8) <)) '(1 1 2 3 4 7 8) "select-sort 1")
(test (lambda () (select-sort '((3 4) (1 2) (2 3) (6 4)) ptcomparator)) '((1 2) (2 3) (3 4) (6 4)) "select-sort 2")

(test (lambda () (lst-map '(1 2 3) (lambda (x) (* x 2)))) '(2 4 6) "lst-map 1")
(test (lambda () (lst-map '((1 2) (3 4)) (lambda (pt) (cadr pt)))) '(2 4) "lst-map 2")

(test (lambda () (getcen1 '((3 4) (1 3) (2 3)))) '(6/3 10/3) "getcen1")

(test (lambda () (median '(1 2 3))) 2 "median 1")
(test (lambda () (median '(1 2 3 4 5 6))) 3 "median 2")

(test (lambda () (lst-append '(2 3) 1)) '(2 3 1) "lst-append 1")

(test (lambda () (ptsort '((3 4) (7 5) (2 3) (6 4)) ptcomparator)) '((2 3) (3 4) (6 4) (7 5)) "ptsort 1")

(test (lambda () (getcen2 '((3 4) (1 2) (2 3) (6 4)))) '(2 3) "getcen2 1")
(test (lambda () (bin-partition '(1 3 5 7) (lambda (k) (< k 5)))) '((1 3) (5 7)) "bin-partition 1")
(test (lambda () (closer-pt '(1 2) '(5 7) '(3 4))) #f "closer-pt 1")
(test (lambda () (closer-pt '(1 2) '(3 4) '(5 7))) #t "closer-pt 2")

(test (lambda () (partition '((3 4) (1 2) (7 3) (4 2) (1 5)) '(1 2) '(1 5))) '(((1 2) (7 3) (4 2)) ((3 4) (1 5))) "partition 1")
(test (lambda () (classify '((3 4) (1 2) (2 3) (6 4)) '(6 4) '(2 3) #t)) '((6 4) (2 3)) "classify 1")
(test (lambda () (classify '((3 4) (1 2) (2 3) (6 4)) '(6 4) '(3 4) #f)) '((6 4) (2 3)) "classify 2")

(test (lambda () (kclassify '((3 4) (1 2) (2 3) (6 4)) '(6 4) '(3 4) #f 1)) '((6 4) (2 3)) "kclassify 1")
(test (lambda () (kclassify '((3 4) (1 2) (2 3) (6 4)) '(6 4) '(3 4) #f 3)) '((6 4) (2 3)) "kclassify 2")
(test (lambda () (kclassify '((3 4) (1 2) (2 3) (6 4)) '(6 4) '(3 4) #f 3)) '((6 4) (2 3)) "kclassify 2")

;(nclassify '((3 4) (1 2) (2 3) (6 4)) #f 3)
|#

; == tests for problem 2 ==

#|
((applyonstds s-table) numberofcourses)
(find-grade g-table 227 2)
(get-grade-lst g-table (car s-table))
(get-gpa g-table (car s-table))
(get-gpa g-table (cadr s-table))
((studentgpa g-table) (car s-table))
((applyonstds s-table) (studentgpa g-table))
|#


#|
(define inplan
  (lambda (course)
    (lambda (student-record)
      (list (car student-record) (cadr student-record) (present course (cadr (cdr student-record)))))))

(define (present x lst)
  (if (null? lst)
      false
      (if (equal? x (car lst))
          true
          (present x (cdr lst)))))

((applyonstds mstudent-table)(lambda(x) (cadr x)))
((applyonstds mstudent-table) (inplan 342))
|#
