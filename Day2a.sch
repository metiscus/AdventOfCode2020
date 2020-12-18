(define source (open-input-file "c:/test2.txt"))

(define char-zero (char->integer #\0))

(define (char->digit c)
  (- (char->integer c) char-zero))

(define (is-digit? c)
  (cond ((eof-object? c) #f)
        ((and (char-ci<=? #\0 c) (char-ci<=? c #\9)) #t)
        (else #f)
        ))


;; Consumes whitespace eof and newlines (\r \l)
(define (chomp-input source)
  (let ((c (peek-char source)))
    (cond ((eof-object? c) '())
          ((or (char-whitespace? c)(eq? c #\newline)(eq? c #\return))
           (read-char source)
           (chomp-input source))
          (else '()))))


;; Reads a number from the source file
(define (read-number source)
  (let (
        (val 0)
        (get-char (lambda (source)
                    (let ((c (read-char source)))
                      (cond ((is-digit? c)
                             (char->digit c))
                            (else
                             '())))))
        )
    ;;
    (do ((c (get-char source) (get-char source)))
      ((null? c))
      (set! val (+ (* val 10) c)))
    val
    )
  )

(define (read-string source)
  (let ((s (read source)))
    (cond ((eof-object? s) '())
          (else (string->list (symbol->string s))))
    ))

(define valid-count 0)

(define (gogo source)
  (do ((c (peek-char source) (peek-char source)))
    ((or (eof-object? c)(char-whitespace? c)(eq? c #\newline)(eq? c #\return)))
    (display c)
(let* ((low-bound (read-number source))
      (high-bound (read-number source))
      (c (read-char source))
      (trash (read-char source))
      (password (read-string source))
      (count 0))
  ;(display (list low-bound high-bound c trash password))(newline)
  (chomp-input source)
  (for-each (lambda (x)
              (cond ((eq? x c)
                     (set! count (+ count 1))))
              ) password)
  (display password)(newline)
  ;(display (list low-bound high-bound))(newline)
  (cond ((and (>= count low-bound) (<= count high-bound))
         ;(display #t)
         (set! valid-count (+ valid-count 1))
         )
        (else #f))
))
  )

(gogo source)

valid-count
