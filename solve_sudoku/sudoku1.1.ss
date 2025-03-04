

;lib function

(define (power x y)
  (if (= 0 y) 1 (* x (power x (- y 1))))
  )

(define (i-between-1-and-the-length-of-list? i list)
      (if (>= i 1) 
      (if (<= i (length list))
        #t
        (begin ;#f
        ;(display "i cat not bigger than the length of the list!")
        (begin (display "i(=") (display i) (display ")cat not bigger than the length of the list!"))
        (newline)
        #f)
        )
      (begin ;#f
      ;(display "i cat not smaller than one!!")
      (begin (display "i(=") (display i) (display ")cat not smaller than one!!"))
      (newline)
      #f)
      )
  )


(define (get-i-th-element-from-list i list)
;  (lambda (i list) 
    (if (i-between-1-and-the-length-of-list? i list) 
      (if (= i 1)
        (car list)
        (get-i-th-element-from-list (- i 1) (cdr list)))
      (begin (display "What element do you want?") (newline))
      )
;    )
  )

(define (get-list-whithout-i-th-element i list)
  (if (i-between-1-and-the-length-of-list? i list)
    (if (= i 1) 
      (cdr list)
      (cons (car list) (get-list-whithout-i-th-element (- i 1) (cdr list)))
      )
    (begin (display "i\'st element is not exist!") (newline))
    )
  )

(define (add-x-to-be-the-i-th-element-to-list x i list)
  (if (i-between-1-and-the-length-of-list? i (cons 0 list))
    (if (= i 1)
      (cons x list)
      (cons (car list) (add-x-to-be-the-i-th-element-to-list x (- i 1) (cdr list)))
      )
    (begin (newline) (display "the list is too short that x can not add as i'st element OR the i is too large than the length of list\'=(cons 0 list).") )
    )
  )

(define (update-i-th-element-equal-x x i list)
  (add-x-to-be-the-i-th-element-to-list x i (get-list-whithout-i-th-element i list))
  )

(define (exchange-i-th-and-next i list) 
  (if (< i (length list))
    (if (> i 0)
      (if (= i 1)
        (cons (car (cdr list)) (cons (car list) (cdr (cdr list))))
        (cons (car list) (exchange-i-th-and-next (- i 1) (cdr list)))
        )
      (begin (display "i can not smaller than one.") (newline) #f)
      )
    (begin (display "i can not biger or equal than length of list.") (newline) #f)
    )
  )

(define (exchange-i-and-j-th-element i j list)
  (update-i-th-element-equal-x (get-i-th-element-from-list j list) i (update-i-th-element-equal-x (get-i-th-element-from-list i list) j list))
  )

(define (generate-n-length-list-per-element-equal-x n x) 
  (if (= n 0)
    '()
    (cons x (generate-n-length-list-per-element-equal-x (- n 1) x)))
  )

(define (generate-base-sequence-tmp n)
  (if (= n 0)
   '()
    (cons n (generate-base-sequence-tmp (- n 1)))
    )
  )
(define (generate-base-sequence n)
  (reverse-list (generate-base-sequence-tmp n))
  )

(define (reverse-list-temp list temp-list) 
    (if (null? list)
      temp-list
      (reverse-list-temp (cdr list) (cons (car list) temp-list)) 
      )
  )
(define (reverse-list list) (reverse-list-temp list '()))

;write by AI-copilot
;(define (reverse-list list)
;  (if (null? list)
;    '()
;    (append (reverse-list (cdr list)) (cons (car list) '()))    ;(cons (car list) '())==(list (car list))
;    )
;  )

(define FACT-G 
  (lambda (f) 
    (lambda (n) 
      (if (= n 0)
        1
        (* n (f (- n 1)))))))

(define Y
  (lambda (f1)
    ( (lambda (f2) (f2 f2)) 
      (lambda (f3) 
        (lambda (f4)
          ((f1 (f3 f3)) f4))))))

(define FACT (Y FACT-G))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (x-in-list? x list)
  (if (null? list)
    #f
    (if (= x (car list))
      #t
      (x-in-list? x (cdr list))
      )
    )
  )
;rewirt by lambda and cond
;(define x-in-list?
;  (lambda (x list)
;    (cond
;      ((null? list) #f)
;      ((= x (car list)) #t)
;      (else (x-in-list? x (cdr list))))))

(define (get-x-first-metric-in-list-tmp x list tmpnum) 
    (if (null? list)
      (begin (display "someting wrong,is list empty?") (newline) #f)
      (if (= x (car list)) 
        (+ tmpnum 1)
        (get-x-first-metric-in-list-tmp x (cdr list) (+ tmpnum 1))
        )
      )
  )
(define (get-x-first-metric-in-list x list)
  (if (x-in-list? x list) 
    (get-x-first-metric-in-list-tmp x list 0)
    (begin (display "x is not in the list!") (newline) #f)
    )
  )

(define (get-x-all-metric-in-list-tmp x list tmpnum tmp-metric-list) 
  (if  (null? list)  
    tmp-metric-list 
    (if (= x (car list)) 
      (get-x-all-metric-in-list-tmp x (cdr list) (+ tmpnum 1) (cons (+ tmpnum 1) tmp-metric-list))
      (get-x-all-metric-in-list-tmp x (cdr list) (+ tmpnum 1) tmp-metric-list)
      )
    )
  )
(define (get-x-all-metric-in-list x list)
  (if (x-in-list? x list) 
    (reverse-list (get-x-all-metric-in-list-tmp x list 0 '()))
    (begin (display "x is not in the list!") (newline) #f)
    )
  )

(define (permute-list-by-i-steps i list)
  (if (= i 0)
    list
    (add-x-to-be-the-i-th-element-to-list 
      (car list) 
      (+ (quotient i (factorial (- (length list) 1))) 1) 
      (permute-list-by-i-steps 
        (remainder i (factorial (- (length list) 1))) 
        (cdr list)))
    )
  )

(define (get-index-of-list2-base-list1-tmp list1 list2 tmp-number)
  (if (null? list1)
    tmp-number
    (get-index-of-list2-base-list1-tmp 
    (cdr list1) 
    (get-list-whithout-i-th-element (get-x-first-metric-in-list (car list1) list2) list2)
    (+ tmp-number 
      (* 
        (-  (get-x-first-metric-in-list (car list1) list2) 1)  
        (factorial (-  (length list2) 1)))))
    )
  )
(define (get-index-of-list2-base-list1 list1 list2)
  (get-index-of-list2-base-list1-tmp list1 list2 0))

(define (make-bignum-by-numlist-base-n-tmp n reverse-numlist)
  (if (null? reverse-numlist)
    0
    (+ (* (car reverse-numlist) (power (factorial n) (- (length reverse-numlist) 1))) 
      (make-bignum-by-numlist-base-n-tmp n (cdr reverse-numlist)) )
    )
  )
(define (make-bignum-by-numlist-base-n n numlist)
  (make-bignum-by-numlist-base-n-tmp n (reverse-list numlist))
  )

(define (make-numlist-by-bignum-base-n-tmp n bignum tmp-numlist supdate-i-th)
  (if (= 0 supdate-i-th)
    tmp-numlist
    (make-numlist-by-bignum-base-n-tmp 
      n 
      (quotient bignum (factorial n)) 
      (update-i-th-element-equal-x (remainder bignum (factorial n)) supdate-i-th tmp-numlist) 
      (- supdate-i-th 1))
    )
  )
(define (make-numlist-by-bignum-base-n n bignum)
  (reverse-list (make-numlist-by-bignum-base-n-tmp n bignum (generate-n-length-list-per-element-equal-x n n) n))
  )

;(define (make-A-by-row-vectors row-vector-list)
;  (if (null? row-vector-list)
;    '()
;    (cons (car row-vector-list) (make-A-by-row-vectors (cdr row-vector-list)))
;  ))
;this is just a (lambda (x) (x)) for list!

;A =  row-vector-list!!!!!

(define (get-i-row-j-column-element-from-A i j A)
  (get-i-th-element-from-list j (get-i-th-element-from-list i A))
  )

(define create-coord
  (lambda (x y)
    (list x y)))

(define get-x
  (lambda (coord)
    (car coord)))

(define get-y
  (lambda (coord)
    (car (cdr coord))))

(define equal-coords?
  (lambda (c1 c2)
    (and
      (= (get-x c1) (get-x c2))
      (= (get-y c1) (get-y c2)))))

(define get-element-form-A-by-coord 
  (lambda (coord A)
    (get-i-row-j-column-element-from-A (get-x coord) (get-y coord) A)))

(define (get-i-th-column-vector-from-A i A)
  (if (null? A)
    '()
    (cons (get-i-th-element-from-list i (car A)) (get-i-th-column-vector-from-A i (cdr A)))
    )
  )

(define (get-i-th-row-vector-from-column-vectors i column-vector-list)
  (if (null? column-vector-list)
    '()
    (cons (get-i-th-element-from-list i (car column-vector-list)) 
      (get-i-th-row-vector-from-column-vectors i (cdr column-vector-list))))
  )

(define (make-A-by-column-vectors-tmp column-vector-list tmpnum)
  (if (= tmpnum 0)
    '()
    (cons 
      (get-i-th-row-vector-from-column-vectors 
        (- (length (car column-vector-list)) -1 tmpnum) 
        column-vector-list)
      (make-A-by-column-vectors-tmp column-vector-list (- tmpnum 1)))
    )
  )
(define (make-A-by-column-vectors column-vector-list)
  (make-A-by-column-vectors-tmp column-vector-list (length (car column-vector-list)))
  )

(define (trans-row-vectors-to-column-vectors A)
  (make-A-by-column-vectors A)
  )

(define (double x) (make-A-by-column-vectors (make-A-by-column-vectors x)));it is fun,the make-A-by-column-vectors function is a transpose ope

(define (A-to-numlist A)
  (if (null? A)
    '()
    (cons (get-index-of-list2-base-list1 (generate-base-sequence (length (car A))) (car A))
      (A-to-numlist (cdr A)))
    )
  )

(define (A-to-numlist-base-n A n)
  (if (null? A)
    '()
    (cons (get-index-of-list2-base-list1 (generate-base-sequence n) (car A))
      (A-to-numlist (cdr A)))
    )
  )

(define (numlist-to-A-base-n n numlist)
  (if (null? numlist)
    '()
    (cons 
      (permute-list-by-i-steps (car numlist) (generate-base-sequence n)) 
      (numlist-to-A-base-n n (cdr numlist)))))

(define (A-to-bignum A)
  (make-bignum-by-numlist-base-n (length (car A)) (A-to-numlist A)))

(define (A-to-bignum-base-n n A)
  (make-bignum-by-numlist-base-n n (A-to-numlist A)))

(define (bignum-to-A-base-n n bignum)
  (numlist-to-A-base-n n (make-numlist-by-bignum-base-n n bignum))
  )

(define (check-A-by-constraint-1 A constraint-1)
  (= (car (cdr constraint-1)) (get-i-row-j-column-element-from-A (get-x (car constraint-1)) (get-y (car constraint-1)) A))
  )

(define (check-A-by-constraint-list1 A constraint-list1)
  (if (null? constraint-list1)
    #t
    (if (check-A-by-constraint-1 A (car constraint-list1))
      (check-A-by-constraint-list1 A (cdr constraint-list1))
      #f
      )
    )
  )

(define (make-new-list-from-A-by-coord-list A coord-list)
  (if (null? coord-list)
    '()
    (cons (get-i-row-j-column-element-from-A (get-x (car coord-list)) (get-y (car coord-list)) A) (make-new-list-from-A-by-coord-list A (cdr coord-list)))
    )
  )

(define (check-list-can-be-arrange-to-1-to-n-number-list list n)
 (if (= (length list) n)  
   (if (null? list)
     #t
     (if (x-in-list? n list)
       (check-list-can-be-arrange-to-1-to-n-number-list (get-list-whithout-i-th-element (get-x-first-metric-in-list n list) list) (- n 1))
       #f
       )
     )
     #f
    )
  )

(define (check-A-by-constraint-2 A constraint-2)
  (check-list-can-be-arrange-to-1-to-n-number-list (make-new-list-from-A-by-coord-list A constraint-2) (length constraint-2))
  )

(define (check-A-by-constraint-list2 A constraint-list2)
  (if (null? constraint-list2)
    #t
    (if (check-A-by-constraint-2 A (car constraint-list2))
      (check-A-by-constraint-list2 A (cdr constraint-list2))
      #f
      )
    )
  )




;apply function

(define order 5)

(define constraint-list1 '(((2 1) 5) ((3 2) 4) ((4 2) 1) ((3 5) 2)))

;(define (test-make-pairs-end-x-base-n-tmp1 x n i)
;  (if (> i n)
;    '()
;    (cons (cons i (cons x '())) (test-make-pairs-end-x-base-n-tmp1 x n (+ i 1)))
;    )
;  )
;(define (test-make-pairs-end-x-base-n x n) 
;  (test-make-pairs-end-x-base-n-tmp1 x n 1)
;  )
;
;(define (test-make-pairs-begin-x-base-n-tmp1 x n i)
;  (if (> i n)
;    '()
;    (cons (cons x (cons i '())) (test-make-pairs-begin-x-base-n-tmp1 x n (+ i 1)))
;    )
;  )
;(define (test-make-pairs-begin-x-base-n x n) 
;  (test-make-pairs-begin-x-base-n-tmp1 x n 1)
;  )

;(define (make-line x pair) 
;  (if (= (car pair) 0)
;    '()
;    (cons (cons x (cons (car (cdr pair)) (quote ()))) (make-line x (cons (- (car pair) 1) (cons (+ (car (cdr pair)) 1) (quote ())))))
;    ))

;  (define make-line-G (lambda (f x pair) 
;    (if (= (car pair) 0)
;    '()
;    (cons (cons x (cons (car (cdr pair)) (quote ()))) (f x (cons (- (car pair) 1) (cons (+ (car (cdr pair)) 1) (quote ()))))))
;    ))

(define (make-pair-i_x-base-n x n)
  (if (= n 0)
    '()
    (cons (list n x) (make-pair-i_x-base-n x (- n 1)))
    )
  )
(define (pairs-martix-base-n-m n m)
  (if (= m 0)
    '()
    (cons (reverse-list (make-pair-i_x-base-n m n)) (pairs-martix-base-n-m n (- m 1)))
    )
  )
(define constraint-list2-base 
  (reverse-list (pairs-martix-base-n-m order order))
  )

(define constraint-list2-part 
  (quote (((1 1) (2 1) (3 1) (4 1) (3 2)) 
  ((1 2) (1 3) (1 4) (1 5) (2 4)) 
  ((2 2) (2 3) (3 3) (4 3) (4 2)) 
  ((5 1) (5 2) (5 3) (5 4) (4 4)) 
  ((2 5) (3 5) (4 5) (5 5) (3 4)))))

(define constraint-list2 (append constraint-list2-base constraint-list2-part))

(define (check-A-1 A)
  (check-A-by-constraint-list1 A constraint-list1))

(define (check-A-2 A)
  (check-A-by-constraint-list2 A constraint-list2))

(define (check-A A)
  (if (check-A-1 A)
    (check-A-2 A)
    #f))

(define (solve-help index-now max-index resoult-list)
  (if (> index-now max-index)
    resoult-list
    (if (check-A (bignum-to-A-base-n order index-now))
      (begin (if (= (remainder index-now 2) 0) (begin (display "now,index=") (display index-now) (display "") (newline)) ) (cons index-now resoult-list))
      (solve-help (+ index-now 1) max-index resoult-list)
      )
    )
  )

;(define (solve)
;  (solve-help 0 (- (power (FACT order) order) 1) '())
;  )


