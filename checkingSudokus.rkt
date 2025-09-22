#lang racket
(provide readSudoku)
(require racket/string)

;; global variables 
;; to track if the current board is valid
;; and to store results
(define is-board-valid #t)
(define board-results (make-hash))

;; helper functions
;; helper check if a row is blank or convert a string to a list of numbers thent to make them rows in board
;;helped by AI as my original string->sudoku-row was not working properly
(define (blank? s) (zero? (string-length (string-trim s))))
(define (string->sudoku-row s)
  (map string->number (string-split (string-trim s) " ")))
(define (no-dups? xs)
  (= (length xs) (length (remove-duplicates xs))))

;; --- ROWS VALIDATION
;; checks each row for duplicates
;; if any row fails, sets is-board-valid to #f
(define (rows-valid! board cur-board)
  (set! is-board-valid #t)
  (for ([i (in-naturals)] [row board])
    (if (no-dups? row)
        (printf "Board ~a row ~a OK~n" cur-board i)
        (begin
          (printf "Board ~a row ~a FAILED~n" cur-board i)
          (set! is-board-valid #f))))
  is-board-valid)

;; --- COLUMNS VALIDATION
;; checks each column for duplicates
;; if any column fails, sets is-board-valid to #f
(define (columns-valid! board cur-board)
  (when is-board-valid
    (for ([c (in-range 9)])
      (define col (for/list ([r board]) (list-ref r c)))
      (if (no-dups? col)
          (printf "Board ~a col ~a good~n" cur-board c)
          (begin
            (printf "Board ~a col ~a UH OH~n" cur-board c)
            (set! is-board-valid #f)))))
  is-board-valid)

;; --- BOXES VALIDATION
;; checks each 3x3 box for duplicates
;; if any box fails, sets is-board-valid to #f
;;NOTE Ai co-pilot assisted with this function howver made an error it needed a nested for loop to work properly was only good for the diagnoal boxes.
(define (boxes-valid! board cur-board)
  (when is-board-valid
    (for* ([br (in-range 3)]
           [bc (in-range 3)])
      (define box
        (for*/list ([r (in-range (* br 3) (+ (* br 3) 3))]
                    [c (in-range (* bc 3) (+ (* bc 3) 3))])
          (list-ref (list-ref board r) c)))
      (if (no-dups? box)
          (printf "Board ~a box (~a,~a) good~n" cur-board br bc)
          (begin
            (printf "Board ~a box (~a,~a) Uh OH~n" cur-board br bc)
            (set! is-board-valid #f)))))
  is-board-valid)

;; basic reading file function
;;note that ai co-pilot helped me with this function as well as its basic code structure
;; however I had to modify it to fit my needs and fix some errors it had
(define (readSudoku filename)
  (set! board-results (make-hash)) 
  (define input-port (open-input-file filename))
  (displayln "File opened successfully!!!!")
  (let loop ([line (read-line input-port)]
             [rows 0]
             [cur-board 1]
             [acc '()])
    (cond
      [(eof-object? line)
       (close-input-port input-port)
       (displayln "File closed :((")
       board-results]

      [(blank? line)
       (loop (read-line input-port) rows cur-board acc)]

      [else
       (let* ([row       (string->sudoku-row line)]
              [acc*      (cons row acc)]
              [next-line (read-line input-port)])
         (if (= (add1 rows) 9)
             (let ([board (reverse acc*)])
               (rows-valid! board cur-board)
               (columns-valid! board cur-board)
               (boxes-valid! board cur-board)
               (hash-set! board-results cur-board is-board-valid)
               (printf "Board ~a FINAL RESULT: ~a~n~n"
                       cur-board (if is-board-valid "PASSED ALL TESTS ;D" "FAILED SOME TESTS :("))
               (loop next-line 0 (add1 cur-board) '()))
             (loop next-line (add1 rows) cur-board acc*)))])))
