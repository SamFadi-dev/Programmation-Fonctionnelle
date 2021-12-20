#lang racket
;-------------------------
;Implémentation de semtab
;-------------------------

;Fonction semtab

(define (semtab f)
  (if (null? f)
      '()
      (semtab-aux f '())))

;Fonction auxilliaire

(define (semtab-aux f previous)
  (if (null? f)
      (list previous)
      (let ((present (car f)) (next (cdr f)))
        (if (pair? present)
            ; (display present) (newline) 
            (cond ((eqv? (car present) 'OR) (my_OR_fct present previous next)) ;=> Cas du OR
                  ((eqv? (car present) 'AND) (my_AND_fct present previous next)) ;=> Cas du AND
                  ((and (eqv? (car present) 'IFTHEN) (not (null? (cadr present))) (not (null? (caddr present)))) (my_IFTHEN_fct present previous next));=> Cas du IFTHEN
                  ((and (eqv? (car present) 'NOT) (not (null? (cddr present)))) (my_NOT_fct present previous next)) ;=> Cas du NOT sur plusieurs arguments
                  ((and (eqv? (car present) 'NOT) (pair? (cadr present)) (eqv? (caadr present) 'NOT)) (my_NOTNOT_fct present previous next)) ;=> Cas du (NOT(NOT ))
                  ((and (eqv? (car present) 'NOT) (pair? (cadr present)) (eqv? (caadr present) 'OR)) (my_NOTOR_fct present previous next)) ;=> Cas du (NOT(OR ))
                  ((and (eqv? (car present) 'NOT) (pair? (cadr present)) (eqv? (caadr present) 'AND)) (my_NOTAND_fct present previous next)) ;=> Cas du (NOT(AND ))
                  ((and (eqv? (car present) 'NOT) (pair? (cadr present)) (eqv? (caadr present) 'IFTHEN) (not (null? (cadadr present))) (not (null? (car (cddadr present))))) (my_NOTIFTHEN_fct present previous next)) ;=> Cas du (NOT(IfTHEN ))
                  ((and (eqv? (car present) 'NOT) (pair? (cadr present)) (eqv? (caadr present) 'EQUIV) (not (null? (cadadr present))) (not (null? (car (cddadr present))))) (my_NOTEQUIV_fct present previous next))
                  ((and (eqv? (car present) 'NOT) (pair? (cadr present)) (eqv? (caadr present) 'NAND) (not (null? (cadadr present))) (not (null? (car (cddadr present))))) (my_NOTNAND_fct present previous next))
                  ((and (eqv? (car present) 'NOT) (pair? (cadr present)) (eqv? (caadr present) 'XOR) (not (null? (cadadr present))) (not (null? (car (cddadr present))))) (my_NOTXOR_fct present previous next))
                  ((and (eqv? (car present) 'NOT) (pair? (cadr present)) (eqv? (caadr present) 'XNOR) (not (null? (cadadr present))) (not (null? (car (cddadr present))))) (my_NOTXNOR_fct present previous next))
                  ((and (eqv? (car present) 'NOT) (not (null? (cadr present)))) (my_NOTSINGLE_fct present previous next))
                  ((and (eqv? (car present) 'EQUIV) (not (null? (cadr present))) (not (null? (caddr present)))) (my_EQUIV_fct present previous next)) 
                  ((and (eqv? (car present) 'NAND) (not (null? (cadr present))) (not (null? (caddr present)))) (my_NAND_fct present previous next)) 
                  ((and (eqv? (car present) 'XOR) (not (null? (cadr present))) (not (null? (caddr present)))) (my_XOR_fct present previous next)) 
                  ((and (eqv? (car present) 'XNOR) (not (null? (cadr present))) (not (null? (caddr present)))) (my_XNOR_fct present previous next)) 
                  (else (my_ELSE_fct present previous next)))
            (my_ELSE_NEXT_fct present previous next)))))

;Fonction qui traite le cas OR

(define (my_OR_fct present previous next)
  (append-map (lambda (x) (semtab-aux (append (list x) next) previous)) (cdr present)))

;Fonction qui traite le cas AND

(define (my_AND_fct present previous next)
  (semtab-aux (append (cdr present) next) previous))

;Fonction qui traite le cas IFTHEN

(define (my_IFTHEN_fct present previous next)
  (append (semtab-aux (append (list (append (list 'NOT) (list (cadr present)))) next) previous) (semtab-aux (append (list (caddr present)) next) previous)))

;Fonction qui traite le cas NOT avec plusieurs arguments

(define (my_NOT_fct present previous next)
  (semtab-aux (append (list (list 'NOT (cadr present)) (append (list 'NOT) (cddr present))) next) previous))

;Fonction qui traite le cas (NOT(NOT ..))

(define (my_NOTNOT_fct present previous next)
  (semtab-aux (append (cdadr present) next) previous))

;Fonction qui traite le cas (NOT(OR ..))

(define (my_NOTOR_fct present previous next)
      (semtab-aux (append (list (append (list 'NOT) (cdadr present))) next) previous))

;Fonction qui traite le cas (NOT(AND ..))

(define (my_NOTAND_fct present previous next)
  (append-map (lambda (x) (semtab-aux (append (list (append (list 'NOT) (list x))) next) previous)) (cdadr present)))

;Fonction qui traite le cas (NOT(IFTHEN ..))

(define (my_NOTIFTHEN_fct present previous next)
  (semtab-aux (append (list (cadadr present)) (list (append (list 'NOT) (list (car (cddadr present))))) next) previous))

;Fonction qui traite le cas (NOTSINGLE) (exemple : present (NOT p))

(define (my_NOTSINGLE_fct present previous next)
  (cond ((null? previous) (semtab-aux next (list (list 'NOT (cadr present)))))
        ((pair? previous) (semtab-aux next (append previous (list (list 'NOT (cadr present))))))
        (else (semtab-aux next (append (list previous) (list (list 'NOT (cadr present))))))))

;Fonction qui traite le cas ELSE (exemple : si liste sans opérateur devant (ex : present = (a b c))

(define (my_ELSE_fct present previous next)
  (cond ((null? previous) (semtab-aux next present))
        ((pair? previous) (semtab-aux (append (cdr present) next) (append previous (list (car present)))))
        (else (semtab-aux (append (cdr present) next) (append (list previous) (list (car present)))))))

;Fonction qui traite le cas ELSE (exemple : si present est une formule (ex : present = t))

(define (my_ELSE_NEXT_fct present previous next)
  (cond ((null? previous)  (semtab-aux next (list present)))
        ((pair? previous) (semtab-aux next (append previous (list present))))
        (else (semtab-aux next (append (list previous) (list present))))))

;Fonction qui traite le cas (EQUIV ..)

(define (my_EQUIV_fct present previous next)
  (append (semtab-aux (append (list (append (list 'NOT) (cdr present))) next) previous) (semtab-aux (append (cdr present) next) previous)))

;Fonction qui traite le cas (NAND ..)

(define (my_NAND_fct present previous next)
  (append-map (lambda (x) (semtab-aux (append (list (append (list 'NOT) (list x))) next) previous)) (cdr present)))

;Fonction qui traite le cas (XOR ..)

(define (my_XOR_fct present previous next)
  (append (semtab-aux (append (list (append (list 'NOT) (list (cadr present)))) (list (caddr present))  next) previous) (semtab-aux (append (list (cadr present)) (list (append (list 'NOT) (list (caddr present)))) next) previous)))

;Fonction qui traite le cas (XNOR ..)

(define (my_XNOR_fct present previous next)
  (append (semtab-aux (append (list (append (list 'NOT) (cdr present))) next) previous) (semtab-aux (append (cdr present) next) previous)))

;Fonction qui traite le cas (NOT(EQUIV ..))

(define (my_NOTEQUIV_fct present previous next)
  (append (semtab-aux (append (list (append (list 'NOT) (list (cadadr present)))) (list (car (cddadr present)))  next) previous) (semtab-aux (append (list (cadadr present)) (list (append (list 'NOT) (list (car (cddadr present))))) next) previous)))

;Fonction qui traite le cas (NOT(NAND ..))

(define (my_NOTNAND_fct present previous next)
  (semtab-aux (append (cdadr present) next) previous))

;Fonction qui traite le cas (NOT(XOR ..))

(define (my_NOTXOR_fct present previous next)
  (append (semtab-aux (append (list (append (list 'NOT) (cdadr present))) next) previous) (semtab-aux (append (cdadr present) next) previous)))

;Fonction qui traite le cas (NOT(XNOR ..))

(define (my_NOTXNOR_fct present previous next)
  (append (semtab-aux (append (list (append (list 'NOT) (list (cadadr present)))) (list (car (cddadr present)))  next) previous) (semtab-aux (append (list (cadadr present)) (list (append (list 'NOT) (list (car (cddadr present))))) next) previous)))


;-----------------------------------------------------
;Implémentation des fonctions de notre bibiliothèques
;-----------------------------------------------------

(define (closed? br) ;renvois vrai si la branche est fermée
  (letrec ((opposite? (lambda (e l)
                        (cond ((null? l) #f)
                              ((pair? e) (or (equal? (cadr e) (car l)) (opposite? e (cdr l))))
                              (else (or (equal? (list 'NOT e) (car l)) (opposite? e (cdr l))))))))
    (cond ((null? br) #f)
          ((opposite? (car br) (cdr br)) #t)
          (else (closed? (cdr br))))))

(define (satisfiable? f) ;renvois vrai si au moins une branche est ouverte
  (letrec ((AND (lambda (l) (if (null? l)
                                #t
                                (and (car l) (AND (cdr l)))))))
    (not (AND (map closed? (semtab f))))))

(define (tautology? f)
  (letrec ((OR (lambda (l) (if (null? l)
                               #f
                               (or (car l) (OR (cdr l)))))))
    (not (OR (map closed? (semtab f))))))

(define (contradiction? f)
  (not (satisfiable? f)))

(define (delete item list) (filter (lambda (x) (not (equal? x item))) list))

(define (remove-closed list) (filter (lambda (x) (not (closed? x))) list))

(define (contains? list item)
  (cond ((null? list) #f)
        ((pair? list) (or (contains? (car list) item) (contains? (cdr list) item)))
        ((or (equal? list item) (equal? list (cons 'NOT (cons item '())))) #t)
        (else #f)))

(define (expand tab var)
  (if (contains? tab var)
      tab
      (cons (cons var tab) (cons (cons (cons 'NOT (cons var '())) tab) '()))))

(define (expand-all tabs var)
  (cond ((null? tabs) '())
        (else (cons (expand (car tabs) var) (expand-all (cdr tabs) var)))))

(define (models f)
  (letrec ((tabs (semtab f))
           (propositions (lambda (f) (delete 'NOT (remove-duplicates (flatten tabs)))))
           (vars (propositions tabs))
           (add (lambda (tabs vars)
                  (if (null? (cdr vars))
                      tabs
                      (add (expand-all tabs (car vars)) (cdr vars))))))
    (add (remove-closed tabs) vars)))
      

;------
;Tests
;------

(define f '((OR p q) r (NOT q)))
(semtab f)
(satisfiable? f)
(contradiction? f)
(tautology? f)
(models f)