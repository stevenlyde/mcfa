#lang scheme

(define (cps proc)
  (lambda (#:cc cc . args)
    (cc (apply proc args))))

(let ((display (cps display))
      (newline (cps newline)))


  

