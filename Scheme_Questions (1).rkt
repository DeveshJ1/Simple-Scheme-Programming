#|Question 1|#
(define (chain functions initial-value)
  (define (apply-functions value functions)
    (if (null? functions)
        value
        (apply-functions ((car functions) value) (cdr functions))))
  (apply-functions initial-value functions))
(define (inc a) (+ a 1))
(define (dec a) (- a 1))
(display(chain '() 5))
(newline)
#|Question 2|#
(define (chain-odd functions initial-value)
  (define (apply-functions functions value)
    (if (null? functions)
        value
        (apply-functions (cdr functions)
                          (if (and (number? value) (odd? value))
                              ((car functions) value)
                              value))))
  (apply-functions functions initial-value))
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (plus2 n) (+ n 2))
(define (minus2 n) (- n 2))
(define (ident s) s)
(display (chain-odd (list ident ident) "hey"))
(newline)
#|Question 6 I put this question above since I use it as a helper method
later on for other questions|#
(define (reverse2 lst)
  (define (reverse-helper lst result)
    (if (null? lst)
        result
        (reverse-helper (cdr lst) (cons (car lst) result))))
  (reverse-helper lst '()))
(display (reverse2 '(1 2 3)))
(newline)
#|Question 3|#
(define (zip list1 list2)
  (define (zip-helper list1 list2 result)
    (if (or (null? list1) (null? list2))
        result
        (zip-helper (cdr list1) (cdr list2) 
                    (cons (list (car list1) (car list2)) result))))
  (reverse2 (zip-helper list1 list2 '())))
(display (zip '(1 2 3) '(4 5 6)))
(newline)
#|Question 4|#
(define (unzip lst)
  (define (unzip-helper lst result1 result2)
    (if (null? lst)
        (list (reverse result1) (reverse result2))
        (unzip-helper (cdr lst)
                      (cons (car (car lst)) result1)
                      (cons (cadr (car lst)) result2))))
  (unzip-helper lst '() '()))
(display (unzip '((1 4) (2 5) (3 6))))
(newline)
#|Question 5|#
(define (remove element lst)
  (cond
    ((null? lst) '())
    ((= element (car lst)) (remove element (cdr lst)))
    (else (cons (car lst) (remove element (cdr lst))))))

(define (contains? element lst)
  (cond
    ((null? lst) #f)
    ((= element (car lst)) #t)
    (else (contains? element (cdr lst)))))

(define (remove-common-elements lst1 lst2)
  (cond
    ((null? lst1) '())
    ((contains? (car lst1) lst2)
     (remove-common-elements (remove (car lst1) lst1) (remove (car lst1) lst2)))
    (else
     (cons (car lst1) (remove-common-elements (cdr lst1) lst2)))))

(define (cancellist list1 list2)
  (list (remove-common-elements list1 list2) 
        (remove-common-elements list2 list1)))
(display (cancellist '(1 2) '(2 4)))
(newline)
#|Question 7|#
(define (add-to-end element lst)
  (if (null? lst)
      (if (list? element)
          element
          (list element))
      (cons (car lst) (add-to-end element (cdr lst)))))

(define (interleave_outer result X Y)
  (cond
    ((and (null? X) (null? Y)) result)
    ((null? X) (add-to-end (reverse2 Y) result))
    ((null? Y) (add-to-end X result))
    (else (interleave_outer (add-to-end (car (reverse2 Y)) (add-to-end (car X) result))
                             (cdr X)
                             (reverse2 (cdr (reverse2 Y)))))))


(display ( interleave_outer '() '(1 2 3) '(a b c d e f)))
(newline)
#|Question 8|#
(define (count_occurrences lst x)
  (cond
    ((null? lst) 0)                       
    ((= (car lst) x)                       
     (+ 1 (count_occurrences (cdr lst) x))) 
    (else                                 
     (count_occurrences (cdr lst) x))))
(display (count_occurrences '(1 2 3 2 4 2 5) 2))