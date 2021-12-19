#lang racket

(define (semtab f)
  (if (null? f)
      '()
      (semtab-aux f '())))

(define (semtab-aux f previous)
  (if (null? f)
       (list previous)
      (let ((present (car f)) (next (cdr f)))
       ; (display present) (newline)
        (if (null? previous) 
            (if (pair? present)
                (cond ((eqv? (car present) 'OR) (append-map (lambda (x) (semtab-aux (append (list x) next) '())) (cdr present))) ;=> Cas du OR
                      ((eqv? (car present) 'AND) (semtab-aux  (append (cdr present) next) '())) ;=> Cas du AND
                      ((and (eqv? (car present) 'IFTHEN) (eqv? (null? (cadr present)) '#f) (eqv? (null? (caddr present)) '#f) (null? (cdddr present))) (append (semtab-aux (append (list (append (list 'NOT) (list (cadr present)))) next) '()) (semtab-aux (append (list (caddr present)) next) '())));=> Cas du IFTHEN
                      ((and (eqv? (car present) 'NOT) (eqv? (null? (cddr present)) '#f)) (semtab-aux (append (list (list 'NOT (cadr present)) (append (list 'NOT) (cddr present))) next) '())) ;=> Cas du NOT sur plusieurs arguments
                      ((and (eqv? (car present) 'NOT) (pair? (cadr present)) (eqv? (caadr present) 'NOT)) (semtab-aux (append (cdadr present) next) '())) ;=> Cas du (NOT(NOT ))
                      ((and (eqv? (car present) 'NOT) (pair? (cadr present)) (eqv? (caadr present) 'OR)) (semtab-aux  (append (list (append (list 'NOT) (cdadr present))) next) '()))  ;=> Cas du (NOT(OR ))
                      ((and (eqv? (car present) 'NOT) (pair? (cadr present)) (eqv? (caadr present) 'AND)) (append-map (lambda (x) (semtab-aux (append (list (append (list 'NOT) (list x))) next) '())) (cdadr present))) ;;=> Cas du (NOT(AND ))
                      ((and (eqv? (car present) 'NOT) (pair? (cadr present)) (eqv? (caadr present) 'IFTHEN) (eqv? (null? (cadadr present)) '#f) (eqv? (null? (car (cddadr present))) '#f)) (semtab-aux (append (list (cadadr present)) (list (append (list 'NOT) (list (car (cddadr present))))) next) '())) ;=> Cas du (NOT(IfTHEN ))
                      ((eqv? (car present) 'NOT) (semtab-aux next (list (list 'NOT (cadr present)))))
                      ( else (semtab-aux next (list present))))
                (semtab-aux next (list present)))
            (if (pair? present)
                (cond ((eqv? (car present) 'OR) (append-map (lambda (x) (semtab-aux (append (list x) next) previous)) (cdr present))) ;=> Cas du OR
                      ((eqv? (car present) 'AND) (semtab-aux  (append (cdr present) next) previous)) ;=> Cas du AND
                      ((and (eqv? (car present) 'IFTHEN) (eqv? (null? (cadr present)) '#f) (eqv? (null? (caddr present)) '#f)) (append (semtab-aux (append (list (append (list 'NOT) (list (cadr present)))) next) previous) (semtab-aux (append (list (caddr present)) next) previous)));=> Cas du IFTHEN
                      ((and (eqv? (car present) 'NOT) (eqv? (null? (cddr present)) '#f)) (semtab-aux (append (list (list 'NOT (cadr present)) (append (list 'NOT) (cddr present))) next) previous)) ;=> Cas du NOT sur plusieurs arguments
                      ((and (eqv? (car present) 'NOT) (pair? (cadr present)) (eqv? (caadr present) 'NOT)) (semtab-aux (append (cdadr present) next) previous)) ;=> Cas du (NOT(NOT ))
                      ((and (eqv? (car present) 'NOT) (pair? (cadr present)) (eqv? (caadr present) 'OR)) (semtab-aux  (append (list (append (list 'NOT) (cdadr present))) next) previous)) ;=> Cas du (NOT(OR ))
                      ((and (eqv? (car present) 'NOT) (pair? (cadr present)) (eqv? (caadr present) 'AND)) (append-map (lambda (x) (semtab-aux (append (list (append (list 'NOT) (list x))) next) previous)) (cdadr present))) ;=> Cas du (NOT(AND ))
                      ((and (eqv? (car present) 'NOT) (pair? (cadr present)) (eqv? (caadr present) 'IFTHEN) (eqv? (null? (cadadr present)) '#f) (eqv? (null? (car (cddadr present))) '#f) ) (semtab-aux (append (list (cadadr present)) (list (append (list 'NOT) (list (car (cddadr present))))) next) previous)) ;=> Cas du (NOT(IfTHEN ))
                      ((and (eqv? (car present) 'NOT) (eqv? (null? (cadr present)) '#f)) (if (pair? previous)
                                                     (semtab-aux next (append previous (list (list 'NOT (cadr present)))))
                                                     (semtab-aux next (append (list previous) (list (list 'NOT (cadr present)))))))
                      (else (if (pair? previous)
                                (semtab-aux (append (cdr present) next) (append previous (list (car present))))
                                (semtab-aux (append (cdr present) next) (append (list previous) (list (car present)))))))
                (if (pair? previous)
                                (semtab-aux next (append previous (list present)))
                                (semtab-aux next (append (list previous) (list present)))))))))

;(define mylist (list (list 'NOT (list 'OR 'l 'mi) (list 'AND 'i 'k) (list 'NOT 'y 'o)) (list 'IFTHEN 'vo 'sk))) 
(define mylist (list (list 'NOT (list 'OR 'l 'mi) (list 'AND 'i 'k)  (list 'AND 'ty 'ui) (list 'NOT 'y 'o)) (list 'IFTHEN 'vo 'sk)))
(define mylist0 (list (list 'OR 'p 'q) (list 'r) (list 'NOT 'q)))
(semtab mylist)
