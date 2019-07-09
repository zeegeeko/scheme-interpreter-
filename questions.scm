(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (if (null? items) '() (cons (proc (car items)) (map proc (cdr items))))
)

(define (cons-all first rests)
  (map (lambda (x) (cons first x)) rests)
)

(define (zip pairs)
  (list (map car pairs) (map car (map cdr pairs)))
)

(define (add-index index lst)
  (cond
    ((null? lst) '())
    (else  (cons (list index (car lst)) (add-index (+ index 1) (cdr lst)))  )
  )
)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (add-index 0 s)
  )
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond
    ((null? denoms) nil)
    ((< total 0) nil)
    ((equal? total 0) (cons (cons (car denoms) nil) nil))
    ((> (car denoms) total) (list-change total (cdr denoms)))
    ((= (car denoms) total) (append (list-change (- total (car denoms)) denoms) (list-change total (cdr denoms))))
    (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))
  )

  )
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19

           ;(list form params (map let-to-lambda body) )

           (cons form (cons params  (map let-to-lambda body)))

           ;(cond
            ;((null? (cdr body)) (list form params (car body)))
            ;(else (list form params (car body) (let-to-lambda (cdr body)) ))
           ;)
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cons
                (cons 'lambda (cons (car  (zip (map let-to-lambda values)))  (map let-to-lambda body)))
                   (cadr (zip (map let-to-lambda values))           ))
           ;(list (list 'lambda (car (zip values)) (car body)) (car(cadr (zip values))) (cadr (cadr (zip values))))  
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19

         (map let-to-lambda expr)
         ;(list (let-to-lambda (car expr)) (let-to-lambda (cadr expr)) (let-to-lambda (car (cddr expr)) ) )
         ; END PROBLEM 19
         )))
