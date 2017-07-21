(define-library (lib guid)
   (export
      guid)
   (import (otus lisp))

(begin

; internal "old good" fast randomizer
(define rand!
   (let* ((ss ms (clock))
          (seed (band (+ ss ms) #xffffffff))
          (seed (cons (band seed #xffffff) (>> seed 24))))
      (lambda (limit)
         (let*((next (+ (car seed) (<< (cdr seed) 24)))
               (next (+ (* next 1103515245) 12345)))
            (set-car! seed (band     next     #xffffff))
            (set-cdr! seed (band (>> next 24) #xffffff))

            (mod (mod (floor (/ next 65536)) 32768) limit)))))

; guid generator
(define (guid)
   (let ((a1 (rand! #x10000))
         (a2 (rand! #x10000))
         (b1 (rand! #x10000))
         (b2 (rand! #x10000))
         (b3 (rand! #x10000))
         (c1 (rand! #x10000))
         (c2 (rand! #x10000))
         (c3 (rand! #x10000))
         (c4 (rand! #x10000))
         (ss "0123456789abcdef"))
     (runes->string (list (string-ref ss (band (>> a1 12) 15))
                          (string-ref ss (band (>> a1  8) 15))
                          (string-ref ss (band (>> a1  4) 15))
                          (string-ref ss (band (>> a1  0) 15))
                          (string-ref ss (band (>> a2 12) 15))
                          (string-ref ss (band (>> a2  8) 15))
                          (string-ref ss (band (>> a2  4) 15))
                          (string-ref ss (band (>> a2  0) 15))
                          #\-
                          (string-ref ss (band (>> b1 12) 15))
                          (string-ref ss (band (>> b1  8) 15))
                          (string-ref ss (band (>> b1  4) 15))
                          (string-ref ss (band (>> b1  0) 15))
                          #\-
                          (string-ref ss (band (>> b2 12) 15))
                          (string-ref ss (band (>> b2  8) 15))
                          (string-ref ss (band (>> b2  4) 15))
                          (string-ref ss (band (>> b2  0) 15))
                          #\-
                          (string-ref ss (band (>> b3 12) 15))
                          (string-ref ss (band (>> b3  8) 15))
                          (string-ref ss (band (>> b3  4) 15))
                          (string-ref ss (band (>> b3  0) 15))
                          #\-
                          (string-ref ss (band (>> c1 12) 15))
                          (string-ref ss (band (>> c1  8) 15))
                          (string-ref ss (band (>> c1  4) 15))
                          (string-ref ss (band (>> c1  0) 15))
                          (string-ref ss (band (>> c2 12) 15))
                          (string-ref ss (band (>> c2  8) 15))
                          (string-ref ss (band (>> c2  4) 15))
                          (string-ref ss (band (>> c2  0) 15))
                          (string-ref ss (band (>> c3 12) 15))
                          (string-ref ss (band (>> c3  8) 15))
                          (string-ref ss (band (>> c3  4) 15))
                          (string-ref ss (band (>> c3  0) 15))
                          (string-ref ss (band (>> c4 12) 15))
                          (string-ref ss (band (>> c4  8) 15))
                          (string-ref ss (band (>> c4  4) 15))
                          (string-ref ss (band (>> c4  0) 15))
))))))
