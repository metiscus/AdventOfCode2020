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

(define source (open-input-file "input.txt"))

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


(define (list-set! list k val)
    (if (zero? k)
        (set-car! list val)
        (list-set! (cdr list) (- k 1) val)))

;; basically we need to allocate a list of lists to represent the plane, fill the rows with f, and set the correct
;; one based on what we calculate below, then use map or fold and count the falses in each row and print any row number
;; with more than 0 falses in it
(define seat-map '())
(do ((i 0 (+ i 1)))
  ((= 127 i))
  (set! seat-map (append seat-map (list (list #f #f #f #f #f #f #f #f )))))

;; calculate the row
(do ((highest 0))
  ((eof-object? (peek-char source)) (display highest)(newline))
  (let* ( (row (parse-row source))
            (seat (parse-seat source))
            (val (+ (* row 8 ) seat )))
    (list-set! (list-ref seat-map row) seat #t)
  (read-char source)
  ))

;(display seat-map)
(define (count-true l)
                    (let ((count 0))
                      (for-each (lambda (v)
                                  (if (eq? #t v) (set! count (+ 1 count))))
                                l)
                      count))
(define row 0)
(for-each (lambda (x)
            (display (list row x))(newline)
            (set! row (+ 1 row))
            ) seat-map)
;; answer was manually calculated from drawn map
;; however a far better way would have been to accumulate just the ticket ids as they are read and find a jump, the missing value
;; would have been correct but I was too far along the map method to stop once I thought of it
