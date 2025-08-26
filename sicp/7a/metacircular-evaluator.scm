;;wirte on Chez Scheme 10.2.0 https://github.com/cisco/ChezScheme/releases/tag/v10.2.0

(define false?
  (lambda (x) (eq? x #f)))

(define false?
    (lambda (x)
      (if x
          #f
          #t)))

(define false? not)

(define true?
  (lambda (x) (eq? x #t)))

(define true?
      (lambda (bool)
        (if bool
          #t
          #f)))

(define true?
  (lambda (x)
    (not (not x))))

(define true? (lambda (x) x))

(define true?
  (lambda (x)
    (if (boolean? x)
      x
      (error "var is not a boolean"))))

;;;wrong-one
(define primitive?
      (lambda (proc)
        (cond 
          ((eq? proc '+) #t)
          ((eq? proc 'car) #t)
          ((eq? proc 'cdr) #t)
          ((eq? proc '-) #t)
          ((eq? proc '=) #t)
          ((eq? proc '*) #t)
          ((eq? proc '/) #t)
          ((eq? proc '>=) #t)
          ((eq? proc '<=) #t)
          ((eq? proc '>) #t)
          ((eq? proc '<) #t)
          ((eq? proc 'eq?) #t)
          ((eq? proc 'zero?) #t)
          ((eq? proc 'null?) #t)
          ((eq? proc 'cons) #t)
          (else #f))))

;should be as possible as small for research this define the system-level-basest-process
(define primitive?
      (lambda (proc)
        (cond
          ((eq? proc +) #t)
          ((eq? proc car) #t)
          ((eq? proc cdr) #t)
          ((eq? proc -) #t)
          ((eq? proc =) #t)
          ((eq? proc *) #t)
          ((eq? proc /) #t)
          ((eq? proc >) #t)
          ((eq? proc <) #t)
          ((eq? proc eq?) #t)
          ((eq? proc null?) #t)
          ((eq? proc cons) #t)
          (else #f))))


;;;stable-one-v1.2
(define primitive?
      (lambda (proc)
        (cond
          ((eq? proc +) #t)
          ((eq? proc car) #t)
          ((eq? proc cdr) #t)
          ((eq? proc -) #t)
          ((eq? proc =) #t)
          ((eq? proc *) #t)
          ((eq? proc /) #t)
          ((eq? proc >=) #t)
          ((eq? proc <=) #t)
          ((eq? proc >) #t)
          ((eq? proc <) #t)
          ((eq? proc eq?) #t)
          ((eq? proc zero?) #t)
          ((eq? proc null?) #t)
          ((eq? proc cons) #t)
          ((eq? proc display) #t)
          (else #f))))

(define primitive-list
  '(+ - * / = > < >= <= eq? zero? null? cons car cdr cons append pair? sin cos tan asin acos atan expt ))

;;;set E0 as system-env/init-env inclouds system-basest-level-process key-val pai And some customized-define
(define E0
    (list (list 
            (cons '+ +)
            (cons '- -)
            (cons '* *)
            (cons '/ /)
            (cons 'car car)
            (cons 'cdr cdr)
            (cons 'cons cons)
            (cons 'list list)
            (cons 'append append)
            (cons 'eq? eq?)
            (cons 'list? list?)
            (cons 'zero? zero?)
            (cons 'number? number?)
            (cons 'null? null?)
            (cons 'string? string?)
            (cons 'symbol? symbol?)
            (cons 'true? (lambda (x) (if x #t #f)))
            (cons 'false? (lambda (value) (if value #f #t)))
            (cons 'boolean? boolean?)
            (cons 'vector? vector?)
            (cons 'procedure? procedure?)
            (cons 'char? char?)
            (cons 'pair? pair?)  
            (cons 'port? port?)
            (cons 'quotient quotient)
            (cons 'remainder remainder)
            (cons 'modulo modulo)
            (cons 'sqrt sqrt)
            (cons 'abs abs)
            (cons 'max max)
            (cons 'min min)
            (cons 'sin sin)
            (cons 'cos cos)
            (cons 'tan tan)
            (cons 'asin asin)
            (cons 'acos acos)
            (cons 'atan atan)
            (cons 'log log)
            (cons 'ln (lambda (x) (log x)))
            (cons 'log10 (lambda (x) (log x 10)))
            (cons 'gcd gcd)
            (cons 'lcm lcm)
            (cons 'truncate truncate)
            (cons 'floor floor)
            (cons 'ceiling ceiling)
            (cons 'round round)
            (cons '= =)
            (cons '> >)
            (cons '< <)
            (cons '>= >=)
            (cons '<= <=)
            (cons 'expt expt)
            (cons 'make-rectangular make-rectangular)
            (cons 'real-part real-part)
            (cons 'imag-part imag-part)
            (cons 'magnitude magnitude)
            (cons 'angle angle)
            (cons 'complex? complex?)
            (cons 'display display)
            (cons 'Y1 (lambda (f1)  
              ((lambda (f2) (f2 f2))
              (lambda (f3)
                  (lambda (f4)
                      ((f1 (f3 f3))
                      f4)))))))))

(define E1
    (cons (list (cons 'a 7) (cons 'b 11)) E0))

(define E2
    (cons (list (cons 'false? '(lambda (bool) (cond ((eq? bool #t) #f))))) E1))


(define (sicp-apply-primitive proc args)
    (apply proc args))

;;;debug-one
(define sicp-apply-debug
    (lambda (proc args)  
      (begin            
        (display "Applying: ") (display proc) (newline)
        (display "With args: ") (display args) (newline)
        (cond          
          ((primitive? proc)               
           (sicp-apply-primitive proc args))
          ((eq? (car proc) 'CLOSURE)
           (sicp-eval-debug
             (cadadr proc)
             (sicp-bind    
               (caadr proc)
               args
               (caddr proc))))
          (else 'error)))))

;;;stable-one
(define sicp-apply       
        (lambda (proc args)
          (cond          
            ((primitive? proc)               
             (sicp-apply-primitive proc args))
            ((eq? (car proc) 'CLOSURE)
             (sicp-eval       
               (cadadr proc)
               (sicp-bind    
                 (caadr proc)
                 args
                 (caddr proc))))
            (else 'error))))


;;;debug-one
(define sicp-eval-debug
  (lambda (exp env)
    (display "Evaluating: ") (display exp) (newline)
    ;(display "Enviroment: ") (display env) (newline)
    (cond
      ((boolean? exp) exp)
      ((number? exp) exp)
      ((symbol? exp) (sicp-lookup exp env))
      ((eq? (car exp) (quote quote)) (cadr exp))
      ((and (list? (cadr exp)) (eq? (car exp) 'let))
       (sicp-eval-debug
         (caddr exp)
         (cons
           (map
             (lambda (list_2)
               (cons
                 (car list_2)
                 (sicp-eval-debug (cadr list_2) env)))
             (cadr exp))
           env)))
      ((eq? (car exp) 'lambda)
       (list 'CLOSURE (cdr exp) env))
      ((eq? (car exp) 'cond)
       (sicp-evcond (cdr exp) env))
      ((and (eq? (car exp) 'begin) (eq? (cdr exp) '())) '())
      ((eq? (car exp) 'begin)
       (cons (sicp-eval-debug (cadr exp) env) (sicp-eval-debug (cons (car exp) (cddr exp)) env)))
      (else (let ((proc (sicp-eval-debug (car exp) env))
                  (args (sicp-evlist (cdr exp) env)))
              (sicp-apply-debug proc args))))))

;;;stable-one-v1.1
(define sicp-eval
      (lambda (exp env)
        (cond 
          ((boolean? exp) exp)
          ((number? exp) exp)
          ((symbol? exp) (sicp-lookup exp env))
          ((eq? (car exp) (quote quote)) (cadr exp)) 
          ((eq? (car exp) 'lambda)
           (list 'CLOSURE (cdr exp) env))
          ((eq? (car exp) 'cond)         
           (sicp-evcond (cdr exp) env))
          (else (sicp-apply 
                  (sicp-eval (car exp) env)
                  (sicp-evlist (cdr exp) env))))))



;;;stable-one-v1.3 add begin and let as syntax key word
(define sicp-eval
      (lambda (exp env)
        (cond
          ((boolean? exp) exp)
          ((number? exp) exp)
          ((symbol? exp) (sicp-lookup exp env))
          ((eq? (car exp) (quote quote)) (cadr exp))
          ((and (list? (cadr exp)) (eq? (car exp) 'let))
           (sicp-eval
             (caddr exp)
             (cons
               (map
                 (lambda
                   (list_2)
                   (cons
                     (car list_2)
                     (sicp-eval (cadr list_2) env)))
                 (cadr exp))
               env)))
          ((eq? (car exp) 'lambda)
           (list 'CLOSURE (cdr exp) env))
          ((eq? (car exp) 'cond)
           (sicp-evcond (cdr exp) env))
          ((and (list? exp) (eq? (car exp) 'begin))
            (evbegin (cdr exp) env #f))
          ((and (eq? (car exp) 'begin-list) (eq? (cdr exp) '())) '())
          ((eq? (car exp) 'begin-list)
           (cons (sicp-eval (cadr exp) env) (sicp-eval (cons (car exp) (cddr exp)) env)))
          (else (sicp-apply
                  (sicp-eval (car exp) env)
                  (sicp-evlist (cdr exp) env))))))

(define evbegin
  (lambda (exp env result)
    (if (null? exp)
      result
      (evbegin (cdr exp) env (sicp-eval (car exp) env)))))

(define pair-up                       
      (lambda (vars vals)                
        (cond                           
          ((eq? vars '())                    
           (cond ((eq? vals '()) '())       
             (else (error TooMuchArguments))))
          ((eq? vals '()) (error TooFewArguments))
          (else                        
            (cons (cons (car vars)    
                        (car vals))
                  (pair-up (cdr vars)
                           (cdr vals))))))) 


(define sicp-bind                  
    (lambda (vars vals env)         
      (cons (pair-up vars vals) env)))

(define sicp-assq                     
      (lambda (sym alist)                
        (cond ((eq? alist '()) '())     
          ((eq? sym (caar alist))
           (car alist))
          (else                        
            (sicp-assq sym (cdr alist))))))

(define sicp-lookup
    (lambda (sym env)
      (cond ((eq? env '()) (error UnBoundVariable))
        (else 
          ((lambda (vcell)
             (cond ((eq? vcell '())
                    (sicp-lookup sym
                      (cdr env)))
               (else (cdr vcell))))
             (sicp-assq sym (car env)))))))

(define pair-up                       
    (lambda (vars vals)                
      (cond                           
        ((eq? vars '())                    
         (cond ((eq? vals '()) '())       
           (else (error TooMuchArguments))))
        ((eq? vals '()) (error TooFewArguments))
        (else                        
          (cons (cons (car vars)
                      (car vals))
                (pair-up (cdr vars)
                         (cdr vals))))))) 

(define sicp-evlist                           
    (lambda (list env)                         
      (cond                                   
        ((eq? list '()) '())
        (else (cons                          
                (sicp-eval (car list) env)
                (sicp-evlist (cdr list) env))))))

(define sicp-evcond
    (lambda (clauses env)
      (cond
        ((eq? clauses '())
         '())
        ((eq? (caar clauses) 'else)
         (sicp-eval (cadar clauses) env))
        ((false? (sicp-eval (caar clauses) env))
           (sicp-evcond (cdr clauses) env))
        (else
          (sicp-eval (cadar clauses) env)))))


;;;;for test FACT func, it does not work. 2025-08-23-22-39-38
;(sicp-eval '(((lambda (f1)
;              ((lambda (f2) (f2 f2))
;              (lambda (f3)
;                  (lambda (f4)
;                      ((f1 (f3 f3))
;                      f4))))) (lambda (f)
;        (lambda (n)
;          (cond ((<= n 0) 1)
;            (else (* n (f (- n 1)))))))) 100) E0)


(define FACT-G
    (lambda (f)
      (lambda (n)
        (cond ((<= n 0) 1)
          (else (* n (f (- n 1))))))))

(define FACT
    (lambda (n)
      (cond
        ((<= n 0) 1)
        (else (* n (FACT (- n 1)))))))

(define FACT-G
    (lambda (f)
      (lambda (n)
        (if (<= n 0)
            1
          (* n (f (- n 1)))))))

(define Y
    (lambda (f)
      ((lambda (x) (f (lambda (y) ((x x) y))))
       (lambda (x) (f (lambda (y) ((x x) y)))))))


(define Y1 (lambda (f1)
  ((lambda (f2) (f2 f2))
    (lambda (f3)
      (lambda (f4)
        ((f1 (f3 f3))
         f4))))))

((Y FACT-G) 20)
((Y1 FACT-G) 20)
(FACT 20)
(sicp-eval (quote (((lambda (f1)
              ((lambda (f2) (f2 f2))
              (lambda (f3)
                  (lambda (f4)
                      ((f1 (f3 f3))
                      f4)))))
              (lambda (f)
                (lambda (n)
                  (cond
                    ((<= n 0) 1)
                    (else (* n (f (- n 1))))))))
             100) )
             E0)
;(sicp-eval '((Y G) 100) (cons (list
;(cons 'Y '(lambda (f1)
;              ((lambda (f2) (f2 f2))
;              (lambda (f3)
;                  (lambda (f4)
;                      ((f1 (f3 f3))
;                      f4))))))
;                      (cons 'G '(lambda (f)
;                (lambda (n)
;                  (cond
;                    ((<= n 0) 1)
;                    (else (* n (f (- n 1))))))))) E2))
