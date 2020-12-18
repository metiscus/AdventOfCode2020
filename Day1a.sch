;; Reads a file into a list with one line per element of the list
(define (read-file file-name)
  (let ((source (open-input-file file-name)))
    (let kernel ()
      (let ((nextval (read source)))
        (cond
          ((eof-object? nextval)
           (close-input-port source)
           '())
          (else
           (cons nextval (kernel))))
        )
      )
    )
  )

; checks if two numbers meet our conditions
(define (check-2020 a b)
       (let ((val (+ a b)))
         (cond
           ((= val 2020)
            (display (* a b))))
         )
       )

(define v (read-file "C:/input.txt"))

(define (aaa la lb)
  (cond ((not (null? la))
         (map check-2020 la lb)
         (aaa (cdr la) lb))))

(aaa v v)
