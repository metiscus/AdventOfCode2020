;; Consumes whitespace eof and newlines (\r \l)
(define (chomp-input source)
  (let ((c (peek-char source)))
    (cond ((eof-object? c) '())
          ((or (char-whitespace? c)(eq? c #\newline)(eq? c #\return))
           (read-char source)
           (chomp-input source))
          (else '()))))

;; Reads a string from an input-port stopping at eof or new lines only
(define (read-string source)
  (let ((st ""))
    (do ((c (read-char source) (read-char source)))
      ((or (eof-object? c) (eq? c #\newline) (eq? c #\return)))
      (set! st (string-append st (string c)))
      )
    ;(chomp-input source)
    st
    )
  )

(define (union a b)
  (cond ((null? b) a)
        ((member (car b) a)
         (union a (cdr b)))
        (else (union (cons (car b) a) (cdr b)))))

(define (parse-result source)
  (let ((result '()))
    (do ((c (read-string source) (read-string source)))
      ((= (string-length c) 0))
      ;(display c)
      (set! result (union result (string->list c))))
    result))


(define source (open-input-file "c:/input.txt"))

(define results '())
(do ((c (parse-result source) (parse-result source)))
  ((= (length c) 0))
  (set! results (append results (list (length c)))))

results

(define (sum-list l)
  (if (null? l) 0
      (+ (car l) (sum-list (cdr l)))
      ))

(sum-list results)
