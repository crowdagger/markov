(import (oop goops))

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

(define-generic add-ngram)
(define-method (add-ngram (m <model>) (k <list>) (v <string>))
  (unless (= (gram m) (length k))
    (error "List is not of size appropriate to the model" k (gram m)))
  (let* ([h (slot-ref m 'hash)]
         [initial-v (hash-ref h k)]
         [initial-v (if initial-v
                        initial-v
                        '())])
    (hash-set! h k (cons v initial-v))))
