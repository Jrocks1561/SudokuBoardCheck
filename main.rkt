#lang racket

;;this built in racket function converts the string to numbers and splits the string by spaces
(define (string->sudoku-row line-string)
  (map string->number (string-split line-string " "))) 

;; Function to check if a list has no duplicate values
(define (no-dups? lst)
  (= (length lst) (length (remove-duplicates lst))))

;;open the file and read each line, convert it to a sudoku row
(define input-port (open-input-file "Sudokus.txt"))
(displayln "File opened successfully yayy")

;; Main loop with parameters:
;; line: current line being read from file
;; rows: counter for rows in current board (0-8, resets after each board)
;; is-board-valid: tracks if current board has any duplicate rows
;; cur-board-num: which board number we're currently processing
(let loop ([line (read-line input-port)]  
           [rows 0]                           
           [is-board-valid #t]                 
           [cur-board-num 1])               
          
  ;; Only continue if not the eof
  (when (not (eof-object? line))
    ;; Converting the string line to a list of numbers
    (define row (string->sudoku-row line))

    ;; Check if current row has duplicates
    (define current-row-valid (no-dups? row))

    ;; Check if board was valid before this row
    (define was-board-valid is-board-valid)

    ;; Board is valid only if BOTH conditions are true
    (define new-board-valid (and was-board-valid current-row-valid))

    ;; Check if we just completed 9 rows (a full board)
    (if (= (+ rows 1) 9)
        ;; We just finished a board - report it and start new board
        (begin
          (if new-board-valid
              (printf "Board ~a is VALID~n" cur-board-num)
              (printf "Board ~a is INVALID due to duplicates~n" cur-board-num))
          (loop (read-line input-port) 0 #t (+ cur-board-num 1)))
        ;; Still building current board - continue
        (loop (read-line input-port) (+ rows 1) new-board-valid cur-board-num))))

(close-input-port input-port)
(displayln "File closed")