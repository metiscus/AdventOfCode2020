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

(define source (open-input-file "c:/input.txt"))

(define code '())
;; read the instructions into a list of pairs
(do ((l (read-string source) (read-string source)))
  ((eof-object? (peek-char source)))
  ;(display (list l (read source)))(newline)
  (set! code (append code (list (list (map-func l) (read source)))))
  (chomp-input source))

;code
;; check if pc is in history, if so stop and print acc
;; else fetch instruction
;; execute instruction

(do ((ins (list-ref code pc) (list-ref code pc)))
  ((not (boolean? (member pc pc-history))) (display acc-val))
  (set! pc-history (append pc-history (list pc)))
  (apply (car ins) (cdr ins)))
