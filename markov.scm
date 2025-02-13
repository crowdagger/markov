(import (srfi srfi-1)
        (srfi srfi-11)
        (srfi srfi-13)
        (ice-9 textual-ports)
        (oop goops))

(define sentence-end (string->char-set ".?!"))
(define valid-chars (char-set-union char-set:letter char-set:whitespace (string->char-set "'’-.")))

(define-class <model> ()
  (gram #:init-value 2
        #:init-keyword #:gram
        #:getter gram)
  (hash #:init-form (make-hash-table)))

(define-generic disp)
(define-method (disp (m <model>))
  (let ([h (slot-ref m 'hash)])
    (format #t "#<model ~a>{\n" (gram m))
    (hash-map->list (λ (k v)
                      (format #t "~a: ~a\n" k v))
                    h)
    (display "}\n")))

(define-generic empty-ngram)
(define-method (empty-ngram (m <model>))
  (make-list (gram m) 'empty))
  

(define-generic add-ngram)
(define-method (add-ngram (m <model>) (k <list>) v)
  (unless (= (gram m) (length k))
    (error "List is not of size appropriate to the model" k (gram m)))
  (let* ([h (slot-ref m 'hash)]
         [initial-v (hash-ref h k)]
         [initial-v (if initial-v
                        initial-v
                        '())])
    (hash-set! h k (cons v initial-v))))

(define-generic list-possible)
(define-method (list-possible (m <model>) (ngram <list>))
  (unless (= (gram m) (length ngram))
    (error "List is not of size appropriate to the model" ngram (gram m)))
  (let* ([h (slot-ref m 'hash)]
         [v (hash-ref h ngram)])
    (if v
        v
        '())))

(define (generate m)
  "Generate a string using the model m"
  (let lp ([str ""]
           [prev (empty-ngram m)])
    (let* ([possible (list-possible m prev)]
           [n (random (length possible))]
           [choice (list-ref possible n)])
      (if (eq? choice 'empty)
          str
          (lp (string-append str " " choice)
              (append (cdr prev) (list choice)))))))

(define-generic learn)
(define-method (learn (m <model>) (l <list>))
  "Learn all the ngrams from the list of strings l"
  (let ([n (gram m)])
    (if (<= (length l)
            n)
        'm
        (let-values ([(head tail) (split-at (take l (1+ n)) n)])
          (add-ngram m head (car tail))
          (learn m (cdr l))))))

(define-method (learn (m <model>) (s <string>))
  (let ([lst (listify m s)])
    (if (<= (length lst)
            (* 3 (gram m)))
        m ; do nothing since the string is too short
        (learn m lst))))

(define (learn-from-file m f)
  (call-with-input-file f
    (lambda (p)
      (let* ([s (get-string-all p)]
;             [lst (string-split s sentence-end)])
             [lst (string-split s #\newline)])
        (for-each (λ (e)
                    (learn m e))
                  lst)
        '()))))
             
  

(define (listify m str)
  (let* (;[str (string-filter valid-chars str)]
         ;[str (string-downcase str)]
         [l (string-split str char-set:whitespace)]
         [l (map string-trim-both l)]
         [l (filter (λ (s) (not (equal? "" s))) l)]
         [empty (empty-ngram m)])
    (append empty l empty)))

(define (main)
  (define m (make <model> #:gram 3))
  (define m2 (make <model> #:gram 2))
  (learn-from-file m2 "karima.txt")
  (learn-from-file m2 "razor.txt")
  (learn-from-file m2 "betty.txt")
  (learn-from-file m2 "cookie.txt")
  (learn-from-file m "prompts.txt")
  (disp m)
  (newline)

  (set! *random-state* (random-state-from-platform))

  (for-each (lambda (e)
              (display (generate m))
              (newline))
            (make-list 10))

  (for-each (lambda (e)
              (display (generate m2))
              (newline))
            (make-list 10))

)

(main)
