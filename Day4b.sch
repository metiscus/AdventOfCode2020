;; day 4 b
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
;; instead of fields being just checked for existence we will define lambdas to validate each one

;; Checks if a string is representable as a number between low and high, inclusive
(define (validate-number sval low hi)
  (let ((val (string->number sval)))
    (if (boolean? val)
        val
    (and (>= val low) (<= val hi)))))

;; validate the eyecolour field
(define (validate-eyes sval)
  (let* ((valid-colors (list "amb" "blu" "brn" "gry" "grn" "hzl" "oth")))
    (not (boolean? (member sval valid-colors)))))


;; validates a height
(define (validate-height sval)
  (let ((num-s "")
        (unit ""))
    ;; split the string into digits and unit
    (for-each (lambda (c)
                (if (char-numeric? c)
                    (set! num-s (string-append num-s (string c)))
                    (set! unit (string-append unit (string c)))))
              (string->list sval)
              )
    ;; now process the result
    (cond ((equal? "in" unit)
           (validate-number num-s 59 76))
          ((equal? "cm" unit)
           (validate-number num-s 150 193))
          (else #f))))

(define (validate-color sval)
  (let ((len 0)
        (is-valid #t))
    ;(display sval)
    (for-each (lambda (c)
                (cond ((= 0 len)
                       (if (not (equal? #\# c)) (set! is-valid #f)))
                      ((not (or (char-numeric? c) (char-alphabetic? c))) (set! is-valid #f)))
                (set! len (+ len 1)))
              (string->list sval))
    (and (= len 7) is-valid)))

(define fields (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

(define validators (list (cons "byr" (lambda (v)
                                       (validate-number v 1920 2002)))
                         (cons "iyr" (lambda (v)
                                       (validate-number v 2010 2020)))
                         (cons "eyr" (lambda (v)
                                       (validate-number v 2020 2030)))
                         (cons "hgt" validate-height)
                         (cons "ecl" validate-eyes)
                         (cons "hcl" (lambda (s)
                                       (validate-color s)))
                         (cons "pid" (lambda (s)
                                       (let ((is-valid #t))
                                         (for-each (lambda (c)
                                                     (if (not (char-numeric? c))
                                                              (set! is-valid #f)))
                                                   (string->list s))
                                         (and is-valid (= (string-length s) 9)))))
                         )
  )

(define (validate-passport fields passport)
  (let ((is-valid #t))
    (for-each (lambda (field)
                ;(display field)(newline)
                ;(display (car field))(newline)
                (let (( p-val (assoc (car field) passport)))
                (if (eq? #f p-val)
                    (set! is-valid #f)
                    (begin
                      ;; run the validator and return its result
                      (set! is-valid (and is-valid ((cdr field) (cdr p-val)))))
                      )))
              fields)
    is-valid))

;(validate-passport validators (list-ref passports 0))

(define valid-count 0)
(for-each (lambda (passport)
            (if (validate-passport validators passport)
                (set! valid-count (+ valid-count 1))))
          passports)
valid-count
