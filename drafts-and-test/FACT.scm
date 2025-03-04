(+ 1 1)
(define FACT-G (lambda (x n) (if (= n 0) 1 (* n (x (- n 1))))))
(define Y (lambda (f1) ((lambda (f2) (f2 f2)) (lambda (f3) (lambda (f4) (f1 (f3 f3) f4))))))
(define FACT (Y FACT-G))  
(FACT 12)
(FACT 110)

(define FACT-noY (lambda (n) (if (= n 0) 1 (* n (FACT-noY (-  n 1))))))

(define (FACT-nolambda n) (if (= n 0) 1 (* n (FACT-nolambda (-  n 1)))))
(FACT-nolambda 13)

(define f-3 (lambda (f n) (if (= n 0) 1 (* n (f (- n 1))))))
(f-3 18)