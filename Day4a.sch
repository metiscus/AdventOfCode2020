;; day 4 a
;; parse key:value pairs
;; each 'passport' will be an alist

;; Consumes whitespace eof and newlines (\r \l)
;; modified to stop at the first of newline or return
(define (chomp-input source)
  (let ((c (peek-char source)))
    (cond ((eof-object? c) '())
          ;; needed to flip the order with whitespace for special handling apparently
          ;; this implies that whitespace contains newline and return duh.
          ((or(eq? c #\newline)(eq? c #\return))
           (read-char source))
          ((or (char-whitespace? c))
           (read-char source)
           (chomp-input source))
          (else '()))))

;; Modification of parse-string to stop at spaces and colons (really should implement in terms of strok)
(define (parse-term src)
  (let ((st ""))
    (do ((c (read-char source) (read-char source)))
      ;; because whitespace contains newline and return I can simplfy this...nice hiss
      ((or (eof-object? c) (eq? c #\:) (char-whitespace? c)))
      (set! st (string-append st (string c)))
      )
    (chomp-input source)
    st
    )
  )

;; returns a pair
(define (parse-pair src)
  (let ((x (cons (parse-term src) (parse-term src))))
    ;(display (list "parse-pair:" x))(newline)
    x))

(define (parse-passport src)
  (let ((passport '()))
    (do ((c (peek-char source) (peek-char source)))
      ((or (eq? c #\newline) (eq? c #\return) (eof-object? c)))
      (set! passport (append passport (list (parse-pair src))))
      (chomp-input source)
      )
    passport;
    )
  )

(define source (open-input-file "c:/day4.txt"))

;(parse-passport source)

;; challenge for tomorrow, stop using do and write in terms of recursion
(define (parse-passports source)
  (let ((passports '()))
    (do ((p (parse-passport source) (parse-passport source)))
      ((or (null? p) (= 0 (length p))))
      (set! passports (append passports (list p)))
      (chomp-input source)
      )
    passports;
    ))

(define passports (parse-passports source))
(define (validate-passport fields passport)
  (let ((is-valid #t))
    (for-each (lambda (field)
                (let (( val (assoc field passport)))
                (if (eq? #f val)
                    (set! is-valid #f))))
              fields)
    is-valid))

(define fields (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

;(validate-passport fields (list-ref passports 0))

(define valid-count 0)
(for-each (lambda (passport)
            (if (validate-passport fields passport)
                (set! valid-count (+ valid-count 1))))
          passports)
valid-count
