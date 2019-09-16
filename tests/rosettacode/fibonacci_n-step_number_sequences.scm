(define (n-fib-iterator ll)
   (cons (car ll)
         (lambda ()
            (n-fib-iterator (append (cdr ll) (list (fold + 0 ll)))))))

(print "2, fibonacci : " (ltake (n-fib-iterator '(1 1)) 15))
(print "3, tribonacci: " (ltake (n-fib-iterator '(1 1 2)) 15))
(print "4, tetranacci: " (ltake (n-fib-iterator '(1 1 2 4)) 15))
(print "5, pentanacci: " (ltake (n-fib-iterator '(1 1 2 4 8)) 15))
(print "2, lucas : " (ltake (n-fib-iterator '(2 1)) 15))