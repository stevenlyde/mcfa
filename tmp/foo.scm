#lang scheme

(define (cps proc)
  (lambda (#:cc cc . args)
    (cc (apply proc args))))

(let ((display (cps display))
      (newline (cps newline)))


  

((lambda (_)  ((lambda (_)  (display #:cc (lambda ($tmp$$2)  (begin  (set! _ $tmp$$2) ((lambda _$$4  ((lambda _$$3  (newline #:cc (lambda ($tmp$$1)  (begin  (set! _ $tmp$$1) ((lambda _$$2  ((lambda _$$1  ((lambda answer  answer) 'undefined)) (void))) (void)))))) (void))) (void)))) "hello, wold")) 'undefined)) 'undefined)
)
