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
    (chomp-input source)
    st
    )
  )

; reads in a map from the file and returns it as a list of strings
(define (map-read source)
  (let (( rows '()))
    (do ((row (read-string source) (read-string source)))
      ((string=? "" row))
      (set! rows (append rows (list row)))
      )
    rows
    )
  )

; returns the height of the map
(define (map-height map)
  (length map))

; wraps a coordinate onto 0->(max-val-1)
(define (wrap-coord val max-val)
  (modulo val max-val))

(define (map-get map x y)
  (let ((width (string-length (list-ref map 0))))
    (string-ref (list-ref map y) (wrap-coord x width))))

(define source (open-input-file "c:/day3.txt"))

(define the-map (map-read source))

;(display (map-height the-map))

(let (
      (trees 0)
      ;(height (map-height the-map))
      )
  (do ((x 0 (+ x 3))
       (y 0 (+ y 1))
       (h (map-height the-map)))
    ((>= y h))
   ; (display x y (map-get the-map x y))
    (if (eq? (map-get the-map x y) #\#)
        (set! trees (+ trees 1))))
  trees
  )
  
