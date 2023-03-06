#lang racket
(require racket/port)


(define (process-file filename)
  (regexp-split #px" "
   (remove-characters
    (first
     (read-file filename))
    '("\n" "\\?" "," "\\." "\"" "!" "-" ":" ";"))))


(define (read-file filename)
  (port->lines
     (open-input-file filename
                       #:mode 'text) #:line-mode 'return))


; TODO figure out how to make this recursive
(define (remove-characters file chars)
  (if (empty? chars)
      file
      (remove-characters (regexp-replace* (regexp (first chars)) file " ") (rest chars))))

(define (capitalize file))
  

; Opening all input files
(define Doyle (process-file "Doyle.txt"))
(define Lovecraft (process-file "Lovecraft.txt"))
(define mystery1 (process-file "mystery1.txt"))
(define mystery2 (process-file "mystery2.txt"))