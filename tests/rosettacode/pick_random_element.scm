; http://www.rosettacode.org/wiki/Pick_random_element
(define *include-dirs* (cons "tests/rosettacode" *include-dirs*))
(import (otus random!))

(define x '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
(print (list-ref x (rand! (length x))))
