(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
    (define (conser rest) (cons first rest))
    (map conser rests))

(define (zip pairs)
  (if (null? pairs)
    (list nil nil)
    (list (cons (car (car pairs)) (car (zip (cdr pairs)))) (cons (car (cdr (car pairs))) (car (cdr (zip (cdr pairs)))))))
  )

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (begin (define (enumerate-helper index lst) 
      (if (null? lst) lst
        (cons (list index (car lst)) (enumerate-helper (+ index 1) (cdr lst)))))
    (enumerate-helper 0 s)))
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond 
    ((null? denoms)
      (list nil))
    ((< (- total (car denoms)) 0)
      (list-change total (cdr denoms)))
    ((null? (cdr denoms))
      (cons-all (car denoms) (list-change (- total (car denoms)) denoms)))
    (else
      (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms)))))
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
           (if (null? (cdr body))
            (list form params (car body))
            (list form params (let-to-lambda (car body)) (let-to-lambda (car (cdr body)))))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (define zipped (zip values))
           (define body (car (map let-to-lambda body)))
           (define zip-values (car (cdr zipped)))
           (append `((lambda ,(let-to-lambda (car zipped)) ,body)) (map let-to-lambda zip-values))
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (cons (let-to-lambda (car expr)) (let-to-lambda (cdr expr)))
         ; END PROBLEM 19
         )))
