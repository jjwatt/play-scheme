
(define mylength
  (lambda (ls)
    (if (null? ls)
        0
        (add1 (length (cdr ls))))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat)(cdr lat))
      #f)
     (else (set? (cdr lat))))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (equal? (car lat) a)
               (member? a (cdr lat)))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((member? (car lat)(cdr lat))
      (makeset (cdr lat)))
     (else (cons (car lat)
                 (makeset (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a)
      (multirember a (cdr lat)))
     (else (cons (car lat)
                 (multirember a
                              (cdr lat)))))))

(define makeset-multirember
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else (cons (car lat)
                 (makeset-multirember
                  (multirember (car lat)
                               (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else
      (and (member? (car set1) set2)
           (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     ((member? (car set1) set2) #t)
     (else
      (intersect? (cdr set1) set2)))))

;; There are more to do here from ch 07
;; But, I'm messing with ch 08 now

;; The seqL fn is what insertL does that insertR doesn't
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))
;; The seqR fn is what insertR does that insertL doesn't
(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

;; insert-g acts as insertL or insertR depending on the helper fn
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((eq? (car l) old)
        (seq new old (cdr l)))
       (else
        (cons (car l) ((insert-g seq) new old (cdr l))))))))

;; Build insertL with insert-g and seqL
(define insertL (insert-g seqL))
;; Build insertR with insert-g and seqR
(define insertR (insert-g seqR))

;; The seqS fn is what subst does that insertL and insertR don't
(define seqS
  (lambda (new old l)
    (cons new l)))
;; Build subst with (insert-g seqS)
(define subst (insert-g seqS))


