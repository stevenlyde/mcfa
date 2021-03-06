#!/usr/bin/gsi

(define (read-all)
  (let ((next (read)))
    (if (eof-object? next)
        '()
        (cons next (read-all)))))

(define (count-terms term)
  (cond
   ((pair? term)   (+ 1
                      (count-terms (car term))
                      (count-terms (cdr term))))
   ((null? term)   0)
   (else           1)))

(display (count-terms (read-all)))
(newline)

