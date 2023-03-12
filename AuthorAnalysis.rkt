#lang racket
(require racket/port)


; <--- Initial file processing --->
; Opens input file, removes characters from file, converts string to uppercase, converts string to a list based on spaces
(define (process-file filename)
  (regexp-split #px" +"
   (string-upcase
    (remove-characters
     (first
      (read-file filename))
     '("\n" "\\?" "," "\\." "\"" "!" "-" ":" ";")))))


; Opening file
(define (read-file filename)
  (port->lines
     (open-input-file filename
                       #:mode 'text) #:line-mode 'return))


; Parameterized list of characters (chars) to remove from file (file)
(define (remove-characters file chars)
  (if (empty? chars)
      file
      (remove-characters (regexp-replace* (regexp (first chars)) file " ") (rest chars))))


; <--- Hash creation/processing operations --->
; Creates hashmap of: (Key = word) (Value = count of word)
(define (hash-create input output)
  (if (empty? input)
      output
      (begin
        (if (hash-has-key? output (first input))
          (let
              ([value (+ (hash-ref output (first input)) 1)])
            (hash-set! output (first input) value))
          (hash-set! output (first input) 1))
        (hash-create (rest input) output))))


; Given a list of values from hashmap, sums all values and returns it
(define (hash-sum input total)
  (if (empty? input)
      total
      (let
          ([sum (+ total (first input))])
        (hash-sum (rest input) sum))))


; Given a hashmap, list of keys, and the sum, creates frequency distribution for how often word is used
(define (hash-frequency all-hash keys sum)
  (if (empty? keys)
      all-hash
      (let
          ([converted-quotient (* (log10 (/ (hash-ref all-hash (first keys)) sum)) -1)])
        (hash-set! all-hash (first keys) converted-quotient)
        (hash-frequency all-hash (rest keys) sum))))


; Function for calculating logbase10
(define (log10 n)
  (/ (log n) (log 10)))


; <--- Convert input files to hashmaps of word counts --->
; NOTE: Change name of text files here to test against different input/mystery files

; sacrificing a little memory here for speed so I dont need to recaclulate the hash every time I want to reference it or it's keys
(define Doyle-hash
  (hash-create
   (process-file "Doyle.txt")
   (make-hash '())))

(define Lovecraft-hash
  (hash-create
   (process-file "Lovecraft.txt")
   (make-hash '())))

(define mystery1-hash
  (hash-create
   (process-file "mystery1.txt")
   (make-hash '())))

(define mystery2-hash
  (hash-create
   (process-file "mystery2.txt")
   (make-hash '())))


; <--- Convert hashmaps to frequency of words used --->
(define Doyle-frequency
  (hash-frequency Doyle-hash
                  (hash-keys Doyle-hash)
                  (hash-sum (hash-values Doyle-hash) 0)))

(define Lovecraft-frequency
  (hash-frequency Lovecraft-hash
                  (hash-keys Lovecraft-hash)
                  (hash-sum (hash-values Lovecraft-hash) 0)))

(define mystery1-frequency
  (hash-frequency mystery1-hash
                  (hash-keys mystery1-hash)
                  (hash-sum (hash-values mystery1-hash) 0)))

(define mystery2-frequency
  (hash-frequency mystery2-hash
                  (hash-keys mystery2-hash)
                  (hash-sum (hash-values mystery2-hash) 0)))


; <--- Find Author Calculations --->
; Takes the absolute value difference of value of word from reference text and mystery text
(define (get-abs word reference-text mystery-text)
  (abs
   (-
    (hash-ref reference-text word)
    (hash-ref mystery-text word))))


; (First reference-texts) is reference text 1, (Second reference-texts) is reference text 2, (Third reference-texts) is mystery1 text
; (First scores) is score for reference text 1, (Second scores) is score for reference text 2
; keys are the keys from the mystery text
(define (get-author-scores reference-texts scores keys)
  (if (empty? keys)
      scores
      (if (and (hash-has-key? (first reference-texts) (first keys)) (hash-has-key? (second reference-texts) (first keys)))
          (let
              ([first-score (+ (first scores) (get-abs (first keys) (first reference-texts) (third reference-texts)))]
               [second-score (+ (second scores) (get-abs (first keys) (second reference-texts) (third reference-texts)))])
            (get-author-scores reference-texts (list first-score second-score) (rest keys)))
          (get-author-scores reference-texts scores (rest keys)))))


; <--- Displaying Results --->
; Calculates and displays results of author comparisons to the console
(define (get-author-results mystery-frequency mystery-hash text-num)
  (begin
    (display (string-append "Analyzing Mystery Text " text-num "...\n"))
    (let
        ([scores (get-author-scores (list Doyle-frequency Lovecraft-frequency mystery-frequency) (list 0 0) (hash-keys mystery-hash))])
      (display (string-append "\tMystery Text " text-num " is probably "))
      (if (< (first scores) (second scores))
          (display "Doyle\n")
          (display "Lovecraft\n")))))


; Displaying final outputs for authors
(define find-authors
  (begin
    (get-author-results mystery1-frequency mystery1-hash "1")
    (get-author-results mystery2-frequency mystery2-hash "2")))