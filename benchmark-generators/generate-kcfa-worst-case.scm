#!/usr/bin/gsi

(define (count-terms t)
  (cond
   ((pair? t)    (+ 1 (count-terms (car t)) (count-terms (cdr t))))
   ((null? t)    0)
   (else         1)))
      
                        
  

(define (xargs x lo hi)
  (if (> lo hi)
      '()
      (cons (string->symbol (string-append x (number->string lo)))
            (xargs x (+ 1 lo) hi))))

(define (generate i n)
  (if (> i n)
      `((lambda (z)
          (z ,@(xargs "x" 1 n)))
        (lambda ,(xargs "y" 1 n)
          y1))
      (let (($f (string->symbol (string-append "f" (number->string i))))
            ($x (string->symbol (string-append "x" (number->string i)))))
        `((lambda (,$f) (,$f #t) (,$f #f))
          (lambda (,$x)
            ,(generate (+ i 1) n))))))




;; ((lambda (f1) (f1 #t) (f1 #f))
;;  (lambda (x1) 
;;    ((lambda (f2) (f2 #t) (f2 #f))
;;     (lambda (x2)
;;       ((lambda (f3) (f3 #t) (f3 #f))
;;        (lambda (x3)
;;          ((lambda (z)
;;             (z x1 x2 x3))
;;           (lambda (y1 y2 y3)
;;             y1))))))))

(define args (cdr (command-line)))

(if (not (pair? args))
    (begin
      (display "missing argument: value of n"))
    (begin
      (let* ((n (string->number (car args)))
             (prog (generate 1 n)))
        (display "; n = ")
        (display n)
        (newline)
        (display "; # terms = ")
        (display (count-terms prog))
        (newline)
        (write prog)
        (newline))))
    

    