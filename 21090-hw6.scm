

(define if-stmt? (lambda (e)
  (and (list? e) (equal? (car e) 'if) (= (length e) 4))))

(define letstar-stmt? (lambda (e)
  (and (list? e) (equal? (car e) 'let*) (= (length e) 3))))
  
(define let-stmt? (lambda (e)
  (and (list? e) (equal? (car e) 'let) (= (length e) 3))))

(define define-stmt? (lambda (e)
  (and (list? e) (equal? (car e) 'define) (symbol? (cadr e)) (= (length e) 3))))

(define formal-list? (lambda (e)
  (and (list? e) (symbol? (car e)) (or (null? (cdr e)) (formal-list? (cdr e))))))
  
(define lambda-stmt? (lambda (e)
  (and (list? e) (equal? (car e) 'lambda) (formal-list? (cadr e)) (not (define-stmt? (caddr e))))))
  
(define built-in-operator? (lambda (o)
  (cond 
   ((eq? o '+) #t)
   ((eq? o '-) #t)
   ((eq? o '/) #t)
   ((eq? o '*) #t)
   (else #f)
  )))

 (define get-operator (lambda (op env) 
 (cond 
 ((equal? op '+) +) 
 ((equal? op '-) -) 
 ((equal? op '*) *) 
 ((equal? op '/) /) 
 (else (get-value op env))
 )))
 

(define get-value (lambda (var env)
	(cond 
	((null? env) (error "unbound variable:" var)) 
	((equal? (caar env) var) (cdar env)) 
	(else (get-value var (cdr env))))))
	


(define extend-env (lambda (var val old-env) 
(cons (cons var val) old-env)))



(define repl (lambda (env) 
(let* (
(var0 (display "cs305> ")) 
(expr (read)) 
(new-env (if (define-stmt? expr) (extend-env (cadr expr) (hw6 (caddr expr) env) env) env)) ; if it is a define statement then add to environment
(val (if (define-stmt? expr) (cadr expr) (hw6 expr env))) ; then 
(var (display "cs305: ")) 
(var1 (display val)) 
(var2 (newline)) 
(var3 (newline)))
(repl new-env))
))




(define hw6 (lambda (e env) 

(cond 
 ((number? e) e)
 ((symbol? e) (get-value e env)) 
 ((not (list? e)) (error "cannot evaluate:" e))
 ((if-stmt? e) (if (eq? (hw6 (cadr e) env) 0) 
                    ( hw6 (cadddr e) env) 
                    ( hw6 (caddr e) env)))
 ((let-stmt? e)
      (let* ((names (map car (cadr e)))
            (inits (map cadr (cadr e)))
            (vals (map (lambda (init) (hw6 init env)) inits)) ;; Evaluate inits in env
            (new-env (append (map cons names vals) env)))
            ;; Extend env with names+vals
            (hw6 (caddr e) new-env)))
			
 

 ((letstar-stmt? e) 
		(if (= (length (cadr e)) 1) ;;if there is only one statement to be evaluated
		
		(let* ((expr_list (list 'let (cadr e) (caddr e) ) )  
        (names (map car (cadr expr_list)))
         ;;repeat the procedure as in let statement
			  (inits (map cadr (cadr expr_list)))
        (vals (map (lambda (init) (hw6 init env)) inits))
        (new-env (append (map cons names vals) env)))
		    (hw6 (caddr expr_list) new-env))

		

		;;if there are more than one statements, evaluate sequentially
		(let ((first-stmt (list 'let (list (caadr e)))) 
	       	(rest (list 'let* (cdadr e) (caddr e)))) 
		(let
		 ((expr_list (append first-stmt (list rest))));;recursive let*

		;;repeat the procedure as in let statement for the first one
		(let* ( 
		(names (map car (cadr expr_list))) 
		(inits (map cadr (cadr expr_list)))
                (vals (map (lambda (init) (hw6 init env)) inits))
		(new-env (append (map cons names vals) env)))
		(hw6 (caddr expr_list) new-env))))))			
																																

((built-in-operator? (car e)) 
   (let 
  ((operator (get-operator (car e) env))
   (operands (map hw6 (cdr e) (make-list (length (cdr e)) env) ;;evaluate the operands
    )))
	(cond 
   	((and (equal? operator '+) (= (length operands) 0)) 0) 
		((and (equal? operator '*) (= (length operands) 0)) 1) 
		((and (or (equal? operator '-) (equal? operator '/))
     (= (length operands) (or 0 1))) 
    (error "at least two operands are needed for: " operator))
		 (else (apply operator operands)) ) ;;apply 
     ))


((lambda-stmt? e) e)

 (else 
 (if
  (lambda-stmt? (car e))
	(if (= (length (cadar e)) (length (cdr e))) ;;if matches the parameter list
	(let*  ((args (map hw6 (cdr e) (make-list (length (cdr e)) env))) ;evaluate parameters
	(new_env (append (map cons (cadar e) args) env)));then add bound variables
	(hw6 (caddar e) new_env));;apply lambda
	(error "wrong number of parameters!"))
  ;;if none of the above matches the current statement
	(let ((result (hw6 (list (get-value (car e) env) (cadr e)) env))) result)) ;;read from env
 ))))
 
(define cs305 (lambda () (repl '())))


