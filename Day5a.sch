(define (make-interval l h)
  (cons l h))

(define (split-interval upper i)
  ;; bug: needed +1 since we are zero index smh
  (let ((range (quotient (+ 1 (- (cdr i) (car i))) 2)))
    (cond (upper
           (cons (+ (car i) range) (cdr i)))
          ((not upper)
           (cons (car i) (- (cdr i) range)))
          )))

(define source (open-input-file "c:/input.txt"))


(define (parse-row src)
  (let ((row (make-interval 0 127)))
    ;; parse solve for row
    (do ((i 0 (+ i 1)))
      ((< 5 i))
      (let ((c (read-char src)))
        (set! row (split-interval (equal? c #\B) row))
        ;(display row)(newline)
        ))

    ;; decide which value within the interval we take (this may need to be moved)
    (if (equal? #\B (read-char src))
        (cdr row) (car row))
    ))

(define (parse-seat src)
  (let ((table (list
                (cons "LLL" 0)
                (cons "LLR" 1)
                (cons "LRL" 2)
                (cons "LRR" 3)
                (cons "RLL" 4)
                (cons "RLR" 5)
                (cons "RRL" 6)
                (cons "RRR" 7)))
             
        (str (string (read-char src) (read-char src) (read-char src))))
    ;(display str)(newline)
    (cdr (assoc str table))))

;; calculate the row
(do ((highest 0))
  ((eof-object? (peek-char source)) (display highest)(newline))
  (let ((val (+ (* (parse-row source) 8 ) (parse-seat source) )))
    (if (> val highest)
        (set! highest val)))
  (read-char source)
  highest
  )
