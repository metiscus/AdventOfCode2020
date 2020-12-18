;;; create binding for error
(define error #f)

;;; capture toplevel continuation
;;;  assign a function to error, allowing a variable number of arguments to
;;;  be passed
(call-with-current-continuation (lambda (k)
              (set! error
                (lambda error-arguments
                  (display ">>>> ERROR ")
                  (newline)
                  (k error-arguments)))
              'done)) 

;; Consumes whitespace eof and newlines (\r \l)
(define (chomp-input source)
  (let ((c (peek-char source)))
    (cond ((eof-object? c) '())
          ((or (char-whitespace? c)(eq? c #\newline)(eq? c #\return))
           (read-char source)
           (chomp-input source))
          (else '()))))

;; Reads a string from an input-port stopping at eof or new lines only
;; modified to stop at strings
(define (read-string source)
  (let ((st ""))
    (do ((c (read-char source) (read-char source)))
      ((or (eof-object? c) (eq? c #\space) (eq? c #\newline) (eq? c #\return)))
      (set! st (string-append st (string c)))
      )
    (chomp-input source)
    st
    )
  )

(define acc-val 0)
(define pc 0)
(define pc-history '())

;; nop just advances program counter
(define (nop arg)
  (set! pc (+ pc 1)))

;; jmp sets program counter relative to arg
(define (jmp arg)
  (set! pc (+ pc arg)))

;; acc modifies acc by arg and advanced program counter
(define (acc arg)
  (set! acc-val (+ acc-val arg))
  (set! pc (+ pc 1)))

(define (map-func str)
  (cond ((string=? str "nop") nop)
        ((string=? str "jmp") jmp)
        ((string=? str "acc") acc)))

(define source (open-input-file "input.txt"))

(define code '())
;; read the instructions into a list of pairs
(do ((l (read-string source) (read-string source)))
  ((eof-object? (peek-char source)))
  ;(display (list l (read source)))(newline)
  (set! code (append code (list (list (map-func l) (read source)))))
  (chomp-input source))

;; we need a generic way to run the program so we will write that as a function
(define (run-program prog)
  ;; initialize a 'blank' computer 
  (let ((check-end (lambda ()
                     ;(display (list pc (length prog)))
          (cond ((>= (+ pc 1) (length prog)) (begin
                                         (error (list "Terminated due to end of list" acc-val))(newline) #t))
                ((not (boolean? (member pc pc-history))) (begin
                                                           (display "Terminated due to repeat")(newline) #t))
                (else #f))
        )))
    ; reinit frame
    (set! pc 0)
    (set! pc-history '())
    (set! acc-val 0)
    ; execution loop
    (do ((ins (list-ref prog pc) (list-ref prog pc)))
      ((check-end) (display acc-val))
      (set! pc-history (append pc-history (list pc)))
      (apply (car ins) (cdr ins)))
    ))

; validate that the new version still works
(run-program code)

; save a copy of the normal trace and use it for part 2
(define orig-trace pc-history)

; we need a function that generates a modified version of the program for each entry in orig-trace
(define (modify-program prog at)
  (let ((ip 0)
        (new-prog '()))
    (for-each (lambda (op)
                (if (= at ip)
                    (cond ; we are at the point where we need to modify the code
                      ; switch nop->jmp
                      ((eq? (car op) nop) (set! new-prog (append new-prog (list (list jmp (car (cdr op)))))))
                      ((eq? (car op) jmp) (set! new-prog (append new-prog (list (list nop (car (cdr op)))))))
                      (else (set! new-prog (append new-prog (list op)))))
                    (set! new-prog (append new-prog (list op))))
                (set! ip (+ ip 1)))
              prog)
    new-prog))

(for-each (lambda (step)
            (run-program (modify-program code step))) orig-trace)
