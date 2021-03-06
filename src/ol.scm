;;;
;;; ol.scm: an Otus Lisp read-eval-print loop (REPL) binary image compiler.
;;;

#| Copyright(c) 2012 Aki Helin
 | Copyright(c) 2014 - 2020 Yuriy Chumak
 |
 | This program is free software;  you can redistribute it and/or
 | modify it under the terms of the GNU General Public License as
 | published by the Free Software Foundation; either version 2 of
 | the License, or (at your option) any later version.
 |
 | This program is distributed in the hope that it will be useful,
 | but WITHOUT ANY WARRANTY; without even the implied warranty of
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 |
 | You should have received a copy of the GNU GPL along with this
 | program.           If not, see <http://www.gnu.org/licenses/>.
 |#

(print "Loading code...")
(define build-start (time-ms))

; forget all other libraries to have them be reloaded and rebuilt
; виртуальная (src olvm) содержит список базовых элементов языка
(define *libraries*
   (keep
      (λ (lib)
         (equal? (car lib) '(src olvm)))
      *libraries*))

; предварительная загрузка зависимостей scheme core,
; (иначе импорт сбойнет)
(import (src vm))   ;; команды виртуальной машины
(import (scheme srfi-16)) ;; case-lambda
(import (scheme srfi-87)) ;; "=>" clauses in case
(import (scheme srfi-71)) ;; extended LET-syntax for multiple values
(import (scheme core))    ;; базовый языковый ...
(import (scheme base))    ;; ... набор Scheme

;; forget everhything except these and core values (later list also them explicitly)
,forget-all-but (*libraries* *codes* *vm-args* stdin stdout stderr build-start) ;set-ticker-value

;;;
;;; Time for a new REPL
;;;
(import (src olvm))     ;; vm primitives and basic special forms
(import (scheme core))  ;; core scheme functions and primitives

(define *features* '( ;; implementation features, used by cond-expand
   r7rs
   srfi-16 ; case-lambda
   srfi-87 ; <= in cases
   srfi-71 ; extended LET-syntax for multiple values
   otus-lisp
   owl-lisp)) ; backward compatibility

(define *include-dirs* '("." "libraries")) ;; now we can do (import <libname>) and have them be autoloaded to current repl
(define *owl-names* #empty)
(define *ol-version* "2.1 rc1") ;; http://semver.org/lang/ru/

(define *loaded* '())   ;; can be removed soon, used by old ,load and ,require

(import (otus lisp))
(import (lang intern))
(import (lang threading))


;; shared parameters, librarize later or remove if possible

(define exit-seccomp-failed 2)   ;; --seccomp given but cannot do it
(define max-object-size #xffff)  ; todo: change as dependent of word size

(import (owl parse))

(import (lang gensym))
(import (lang env))
(import (lang macro))
(import (lang sexp))

(import (lang ast))
(import (lang fixedpoint))
(import (lang alpha))
(import (lang cps))
(import (lang closure))
(import (lang assemble))
(import (lang compile))


(define *version*
   (let loop ((args *vm-args*))
      (if (null? args)
         *ol-version*
         (if (string-eq? (car args) "--force-version")
            (if (null? (cdr args))
               (runtime-error "version number expected" args)
               (cadr args))
            (loop (cdr args))))))

(define *features* (append *features* `(
   ,(string->symbol (string-append "ol-" *version*))
   exact-closed
   ratios
   exact-complex
   full-unicode
   immutable)))



;; (define error-tag "err")
;; (define (error? x)
;;    (and (vector? x)
;;       (eq? (ref x 1) error-tag)))

(import (owl time))
(import (owl fasl))

;; fixme: should sleep one round to get a timing, and then use avg of the last one(s) to make an educated guess
;(define (sleep ms)
;   (lets ((end (+ ms (time-ms))))
;      (let loop ()
;         ;(print (interop 18 1 1))
;         (let ((now (time-ms)))
;            (if (> now end)
;               now
;               (begin (interact sleeper-id 50) (loop)))))))

; -> mcp gets <cont> 5 reason info
; (run <mcp-cont> thunk quantum) -> result

;(define input-chunk-size  1024)
;(define output-chunk-size 4096)

(define-syntax share-bindings
   (syntax-rules (defined)
      ((share-bindings) null)
      ((share-bindings this . rest)
         (cons
            (cons 'this
               ['defined (mkval this)])
            (share-bindings . rest)))))

(define (share-modules mods)
   (for null mods
      (λ (envl mod)
         (append (ff->alist mod) envl))))

;(import (owl random))

(import (owl args))

(import (owl sys))

;;;
;;; Entering sandbox
;;;

;; a temporary O(n) way to get some space in the heap

;; fixme: allow a faster way to allocate memory
;; n-megs → _
(define (ensure-free-heap-space megs)
   #true)
;   (if (> megs 0)
;      (lets
;         ((my-word-size (get-word-size)) ;; word size in bytes in the current binary (4 or 8)
;          (blocksize 65536)              ;; want this many bytes per node in list
;          (pairsize (* my-word-size 3))  ;; size of cons cell, being [header] [car-field] [cdr-field]
;          (bytes                         ;; want n bytes after vector header and pair node for each block
;            (map (λ (x) 0)
;               (iota 0 1
;                  (- blocksize (+ pairsize my-word-size)))))
;          (n-blocks
;            (ceil (/ (* megs (* 1024 1024)) blocksize))))
;         ;; make a big data structure
;         (map
;            (λ (node)
;               ;; make a freshly allocated byte vector at each node
;               (make-bytevector bytes))
;            (iota 0 1 n-blocks))
;         ;; leave it as garbage
;         #true)))
;
;; enter sandbox with at least n-megs of free space in heap, or stop the world (including all other threads and io)
(define (sandbox n-megs)
   ;; grow some heap space work working, which is usually necessary given that we can't
   ;; get any more memory after entering seccomp
   (if (and n-megs (> n-megs 0))
      (ensure-free-heap-space n-megs))
   (or
      (syscall 157)
      (begin
         (system-stderr "Failed to enter sandbox. \nYou must be on a newish Linux and have seccomp support enabled in kernel.\n")
         (halt exit-seccomp-failed))))

(import (lang eval))
(define *features* (append *features* '(srfi-0))) ; cond-expand

;; push it to libraries for sharing, replacing the old one
(define *libraries* ; заменим старую (src olvm) на новую
   (cons
      (cons '(src olvm) *src-olvm*)
      (keep (λ (x) (not (equal? (car x) '(src olvm)))) *libraries*)))

;; todo: share the modules instead later
(define shared-misc
   (share-bindings
      *features*
      *include-dirs*
      *libraries*))      ;; all currently loaded libraries

(print "Code loaded at " (- (time-ms) build-start) " ms.")

;;;
;;; MCP, master control program and the thread controller
;;;

(define shared-bindings shared-misc)

(define initial-environment-sans-macros
   (fold
      (λ (env pair) (env-put-raw env (car pair) (cdr pair)))
      *src-olvm*
      shared-bindings))

(define initial-environment
   (bind-toplevel
      (library-import initial-environment-sans-macros
         '((otus lisp))
         (λ (reason) (error "bootstrap import error: " reason))
         (λ (env exp) (error "bootstrap import requires repl: " exp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; new repl image
;;;

;; say hi if interactive mode and fail if cannot do so (the rest are done using
;; repl-prompt. this should too, actually)
(define (make-main-entry symbols codes)
   (let*((initial-names *owl-names*))
      ; main: / entry point of the compiled image
      (λ (vm-args)

         ;; now we're running in the new repl
         (start-thread-controller
            (list ;1 thread
               ['init
                  (λ ()
                     ;; get i/o running
                     (io:init)

                     ;; repl needs symbols interning, which is handled by this thread
                     (fork-intern-interner symbols)
                     ;; and bytecode duplication avoider, is handled by this thread
                     (fork-bytecode-interner codes)

                     (define (starts-with? string prefix)
                        (and (<= (string-length prefix) (string-length string))
                             (string-eq? prefix (substring string 0 (string-length prefix)))))

                     ;(print "vm-args: " vm-args)

                     (let*((options vm-args
                              (let loop ((options #empty) (args vm-args))
                                 (cond
                                    ((null? args)
                                       (values options #null))
                                    ((string-eq? (car args) "--sandbox")
                                       (loop (put options 'sandbox #t) (cdr args)))
                                    ((string-eq? (car args) "--interactive")
                                       (loop (put options 'interactive #t) (cdr args)))
                                    ((string-eq? (car args) "--no-interactive")
                                       (loop (put options 'interactive #f) (cdr args)))
                                    ((string-eq? (car args) "--version")
                                       (print "ol (Otus Lisp) " *version*)
                                       (halt 0))
                                    ;; special case - use embed REPL version
                                    ((string-eq? (car args) "--embed")
                                       (loop (put options 'embed #t) (cdr args)))
                                    ;; home
                                    ((string-eq? (car args) "--home")
                                       (print "use --home=<path>")
                                       (halt 1))
                                    ((starts-with? (car args) "--home=")
                                       (loop (put options 'home
                                                (substring (car args) 7 (string-length (car args))))
                                             (cdr args)))
                                    ;; 
                                    ((starts-with? (car args) "--force-version") ; just skip, already processed at the begin of file
                                       (loop options (cddr args)))      ; because forced version must be reflected in *features* etc.
                                       
                                    ((starts-with? (car args) "--")
                                       (print "unknown command line option '" (car args) "'")
                                       (halt 0))
                                    (else
                                       (let ((file (car args)))
                                          (values (put options 'file
                                                         (unless (string-eq? file "-")
                                                            (let ((port (open-input-file file)))
                                                               (unless port
                                                                  (print "error: can't open/read file '" file "'")
                                                                  (halt 3))
                                                               port)))
                                                  (cdr args)))))))
                           (file (or (getf options 'file)
                                     stdin))

                           (sandbox? (getf options 'sandbox))
                           (interactive? (get options 'interactive (syscall 16 file 19))) ; isatty()
                           (embed? (getf options 'embed))

                           (home (or (getf options 'home)
                                     (getenv "OL_HOME")
                                     (let ((uname (syscall 63)))
                                        (if (vector? uname)
                                           (cond
                                              ((string-eq? (ref uname 1) "Darwin")
                                                 "/usr/local/lib/ol")
                                              (else
                                                 "/usr/lib/ol"))
                                           "/usr/lib/ol"))))

                           (version (cons "OL" *version*))
                           (env (fold
                                    (λ (env defn)
                                       (env-set env (car defn) (cdr defn)))
                                    initial-environment
                                    (list
                                       (cons '*owl-names*   initial-names)
                                       (cons '*include-dirs* (list "." home))
                                       (cons '*interactive* interactive?)
                                       (cons '*vm-args* vm-args)
                                       (cons '*version* version)
                                       (cons '*features* (let*((*features* (let ((one (vm:cast 1 type-vptr)))
                                                                              (cond
                                                                                 ((eq? (ref one 0) 1)
                                                                                    (append *features* '(little-endian)))
                                                                                 ((eq? (ref one (- (size one) 1)) 1)
                                                                                    (append *features* '(big-endian)))
                                                                                 ((eq? (ref one 1) 1)
                                                                                    (append *features* '(middle-endian)))
                                                                                 (else
                                                                                    *features*))))
                                                               (*features* (let ((uname (syscall 63)))
                                                                              (if (vector? uname)
                                                                                 (append *features* `(
                                                                                       ,(string->symbol (ref uname 1))  ; OS
                                                                                       ,(string->symbol (ref uname 5)))) ; Platform
                                                                                 *features*))))
                                                            *features*))
                                       ;(cons '*scheme* 'r5rs)
                                       (cons '*sandbox* sandbox?)))))
                           ; go:
                           (if sandbox?
                              (sandbox 1)) ;(sandbox megs) - check is memory enough

                           ; ohai:
                           (if interactive?
                              (print "Welcome to Otus Lisp " (cdr version)
                                 (if sandbox? ", you feel restricted" "")
                                 "\n"
                                 (if embed? "" "type ',help' to help, ',quit' to end session.")))

                           (if embed?
                              (let*((this (cons (vm:pin env) 0))
                                    (eval (lambda (exp args)
                                             (case exp
                                                (['ok value env]
                                                   (vm:unpin (car this))
                                                   (set-car! this (vm:pin env))
                                                   (if (null? args)
                                                      value
                                                      (apply value args)))
                                                (else is error
                                                   (print-to stderr "error: " (ref error 2))
                                                   #false))))
                                    (evaluate (lambda (expression)
                                             (halt
                                                (let*((env (vm:deref (car this)))
                                                      (exp args (uncons expression #f)))
                                                   (case (type exp)
                                                      (type-string
                                                         (eval (eval-string env exp) args))
                                                      (type-string-wide
                                                         (eval (eval-string env exp) args))
                                                      (type-fix+
                                                         (eval (eval-repl (vm:deref exp) env #f evaluate) args))
                                                      (type-bytevector
                                                         (eval (eval-repl (fasl-decode (bytevector->list exp) #f) (vm:deref (car this)) #f evaluate) args))))))))
                                 (halt (vm:pin evaluate)))
                           else
                              ; regular repl:
                              (fork-server 'repl (lambda ()
                                 ;; set a signal handler which stop evaluation instead of owl
                                 ;; if a repl eval thread is running
                                 (set-signal-action repl-signal-handler)

                                 ;; repl
                                 (shutdown
                                       (repl-trampoline env file)))))))])))))

;;;
;;; Dump the new repl
;;;

(print "Compiling ...")

;--
(define (symbols-of node)
   (define tag (list 'syms))

   (define (walk trail node)
      (cond
         ((value? node) trail)
         ((get trail node #false) trail)
         ((symbol? node)
            (let ((trail (put trail node 1)))
               (put trail tag
                  (cons node (get trail tag null)))))
         ((ref node 0) ; (ref 0) works only for blobs
            (cond
               ((eq? (type node) type-bytecode) #t)
               ((eq? (type node) type-string) #t)
               ((eq? (type node) type-port) #t)
               ((eq? (type node) type-bytevector) #t)
               ((eq? (type node) type-inexact) #t)
               (else (print "unknown raw object: " node)))
            trail)
         (else
            (fold walk
               (put trail node #true)
               (vector->list node)))))
   (define trail
      (walk (put empty tag null) node))

   (get
      (walk (put empty tag null) node)
      tag null))

;--
(define (code-refs seen obj)
   (cond
      ((value? obj) (values seen empty))
      ((bytecode? obj)
         (values seen (put empty obj 1)))
      ((get seen obj #false) =>
         (λ (here) (values seen here)))
      (else
         (let loop ((seen seen) (lst (vector->list obj)) (here empty))
            (if (null? lst)
               (values (put seen obj here) here)
               (let* ((seen this (code-refs seen (car lst))))
                  (loop seen (cdr lst)
                     (ff-union this here +))))))))
(define (codes-of ob)
   (let* ((refs this (code-refs empty ob)))
      (ff-fold (λ (out x n) (cons (cons x x) out)) null this)))




(let*((path "boot.fasl")
      (port ;; where to save the result
         (open-output-file path))

      (symbols (symbols-of make-main-entry))
      (codes   (codes-of   make-main-entry))
      (bytes ;; encode the resulting object for saving in some form
         (fasl-encode (make-main-entry symbols codes))))
   (if (not port)
      (begin
         (print "Could not open " path " for writing")
         #false)
      (begin ;; just save the fasl dump
         (write-bytes port bytes)
         (close-port port)
         (print "Output written at " (- (time-ms) build-start) " ms.")
         #true)))
