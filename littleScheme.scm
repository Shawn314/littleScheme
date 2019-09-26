(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))
(or atom? '(a b c) (null? '())  )
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or? (eq? (car lat) a) (member? a (cdr lat)))))))
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or? (eq? (car lat) a) (member? a (cdr lat)))))))
(define rember
  (lambda (a lat)
    (cond
     ((null? lat ) (quote ()))
     ((eq? a (car lat)) (cdr lat) )
     (else (cons (car lat) (cdr lat))))))
(define rember
  (lambda ( a lat)
    (cond
     ( (null? lat) (quote ()))
     (else (cond
            ( (eq? a (car lat)) (cdr lat))
            (else cons (car lat)
                  (rember a (cdr lat))))))))
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat) (rember a (cdr lat)))))))
(define first
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else (cons (car (car l)) (first (cdr l)))))))
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) old) (cons old (cons new (cdr lat))))
       (else (cons (car lat) (insertR new old (cdr lat)))))))))
(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons new lat))
     (else (cons (car lat) (insertL new old (cdr lat)))))))
(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
            ((eq? (car lat) old) (cons new (cdr lat)))
            (else (cons (car lat) (subst new old (cdr lat)))))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
            ((or (eq? (car lat) o1) (eq? (car lat) 02)) (cons new (cdr lat)))
            (else (cons (car lat) (subset new o1 o2 (cdr lat)))))))))
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) a (multirember a (cdr lat))))
       (else (cons (car lat) (multirember a (cdr lat)))))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) old) (cons (car lat) (cons new (multiinsertR new old cdr (lat)))))
       (else
        (cons (car lat) (multiinsertR new old cdr (lat)))))))))
(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
            ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
            (else (cons (car lat)
                        (multisubst new old (cdr lat)))))))))
;; num practice
(define add1
  (lambda (n)
    (+ n 1)))
(define sub1
  (lambda (n)
    (- n 1)))

(define +
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (+ n (sub1 m)))))))
(define -
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (- n (sub1 m)))))))
(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (+ (car tup) (addtup (cdr tup)))))))
(define *
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (+ n (* n (sub1 m)))))))
(define tup+
  (lambda (tup1 tup2)
    (cond
     (and (null? tup1) (null? tup2) (quote ()))
     (else
      (cons (+ (car tup1) (car tup2))
            (tup+
             (cdr tup1) (cdr tup2)))))))
(define tupRevise+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons (+ (car tup1) (car tup2))
            (tupRevise+
             (cdr tup1) (cdr tup2)))))))
(define >
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else
      ( > (sub1 n) (sub1 m))))))
(define <
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else
      (< (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond
     ((zero? m) (zero? n) #t)
     ((zero? n) #f)
     (else
      (= (sub1 n) (sub1 m))))))
(define =
  (lambda (n m)
    (cond
     ((> n m) #f)
     ((< n m) #f)
     (else #t))))
(define =
  (lambda (n m)
    (cond
     ((eq? n m) #t)
     (else #f))))
(define ^
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else
      (* n (^ n (sub1 m)))))))
(define /
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else
      (add1 (/ (- n m) m))))))

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else
      (add1 (length (cdr lat)))))))


(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((null? lat) lat)
     ((zero? (sub1 n)) (cdr lat))
     (else
      (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((number? (car lat)) (no-nums (cdr lat)))
       (else
        (cons (car lat) (no-nums (cdr lat)))))))))
(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
       (else (all-nums (cdr lat))))))))
(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2)))))
(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else
      (cond
       ((eq? (car lat) a) (add1 (occur a (cdr lat))))
       (else
        (occur a (cdr lat))))))))
;; (define one?
;;   (lambda (n)
;;     (cond
;;      ((zero? (sub1 n)) #t)
;;      (else #f))))
(define one?
  (lambda (n)
    (= n 1)))
(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else
      (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
;;chapter 5
(define rember*
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((atom? (car lat))
      (cond
       ((eq? (car lat) a) (rember* a (cdr lat)))
       (else
        (cons (car lat) (rember* a (cdr lat))))))
     (else
      (cons (rember* a (car lat)) (rember* a (cdr lat)))))))
(define insertR*
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((atom? (car lat))
      (cond
       ((eq? (car lat) old)
        (cons old
              (cons new (insertR* new old (cdr lat)))))
       (else
        (cons (car lat) (insertR* new old (cdr lat))))))
     (else
      (cons (insertR* new old (car lat)) (insertR* new old (cdr lat)))))))
(define occur*
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((atom? (car lat))
      (cond
       ((eq? (car lat) a)
        (add1 (occur* a (cdr lat))))
       (else
        (occur* a (cdr lat)))))
     (else
      (+ (occur* a (car lat)) (occur* a (cdr lat)))))))
(define subst*
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((atom? (car lat))
      (cond
       ((eq? (car lat) old)
        (cons new (subst* new old (cdr lat))))
       (else
        (cons (car lat) (subst* new old (cdr lat))))))
     (else
      (cons (subst* new old (car lat)) (subst* new old (cdr lat)))))))
(define insertL*
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((atom? (car lat))
      (cond
       ((eq? (car lat) old) (cons new (cons old (insertL* new old (cdr lat)))))
       (else
        (cons (car lat) (insertL* new old (cdr lat))))))
     (else
      (cons (insertL* new old (car lat)) (insertL* new old (cdr lat)))))))
(define member*
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     ((atom? (car lat))
      (cond
       ((eq? (car lat) a) #t)
       (else
        (member* a (cdr lat)))))
     (else
      (or (member* a (car lat)) (member* a (cdr lat)))))))
(define leftmost
  (lambda (lat)
    (cond
     ((atom? (car lat))
      (car lat))
     (else
      (leftmost (car lat))))))
;; (define eqlist?
;;   (lambda (l1 l2)
;;     (cond
;;      ((and (null? l1) (null? l2)) #t)
;;      ((and (null? l1) (atom? (car l2))) #f)
;;      ((null? l1) #f)
;;      ((and (atom? (car l1)) (null? l2)) #f)
;;      ((and (atom? (car l1)) (atom? (car l2)))
;;       (and (eqan? (car l1) (car l2)
;;                   (eqlist? (cdr l1) (cdr l2)))))
;;      ((atom? (car l1)) #f)
;;      ((null? l2) #f)
;;      ((atom? (car l2)) #f)
;;      (else
;;       (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((and (atom? (car l1)) (atom? (car l2)))
      (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
     ((or (atom? (car l1)) (atom? (car l2))) #f)
     (else
      (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))
(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else (eqlist s1 s2)))))
(define eqlist?
  (lambda (l1 l2)
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    (else
     (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))))
;; chapter 6
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))
