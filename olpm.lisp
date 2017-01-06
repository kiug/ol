#!/usr/bin/ol

(import (owl parse))
; HTTP
; http://tools.ietf.org/html/rfc2616

(define (a-z? x) (<= #\a x #\z))
(define (A-Z? x) (<= #\A x #\Z))

;(define (symbol? x) (not (or
;   (eq? x #\space)
;   (eq? x #\newline))))

(define CR #\return)
(define LF #\newline)
(define SP #\space)

(define (not-sp? x) (not (eq? x SP)))
(define (not-cr? x) (not (eq? x CR)))
(define (not-lf? x) (not (eq? x LF)))

(define get-status-line
   (let-parses (
         (http-version  (get-greedy+ (get-rune-if not-sp?)))
         (skip          (get-imm SP))
         (status-code   (get-greedy+ (get-rune-if not-sp?)))
         (skip          (get-imm SP))
         (reason-phrase (get-greedy+ (get-rune-if not-cr?)))
         (skip          (get-imm CR))
         (skip          (get-imm LF)))
      (tuple http-version status-code reason-phrase)))

(define http-response-parser
   (let-parses (
         (status-line get-status-line))
      (tuple
         status-line)))


; 1. check the internet to possible update the packages list
(let ((fd (syscall 41 #f #f #f))) ; create socket
   (if (syscall 42 fd "localhost" 8080) ; connect
      (if (syscall 1 fd "HEAD /all.lisp HTTP/1.1\n\n" 0) ; send request
         (let ((answer (fd->exp-stream fd "> " http-response-parser (lambda (pos info lst)
                                                                     (print "invalid http response: " pos " - " (runes->string lst))) #f)))
            (print answer)))))
