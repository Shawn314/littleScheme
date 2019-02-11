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
