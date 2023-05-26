#lang racket

(require rackunit)

(struct sos [board targets] #:transparent)
; An SOS-State is a structure:
;    (sos [List-of SOS-Letter] [List-of String])
; Interpretation:
; The List of SOS Letters represents the board with "S"s "O"s and " "s (empty space)
; The List of Strings represents the list of target strings

(define WIN 1.0)
(define DRAW 0.5)
(define LOSS 0.0) ; (player 1's perspective)

; An SOS-Letter is one of:
; - "S"
; - "O"
; - " " (empty space)

; A Result is one of:
; - 1 (Player 1 wins)
; - 0.5  (Draw)
; - 0 (Player 2 wins)
; - SOS-State (Still Playing)

; [List-of SOS-Letter] -> Number
; Determines the number of moves played given an SOS board
(define (moves board)
  (for/sum ([letter board])
    (if (equal? letter " ") 0 1)))

; SOS-State -> Boolean
; Determines if the game has finished due to a target string on the board.
(module+ test
  (check-equal? (contains-any-target? (sos '(" " " " "O" " ") '("SOS"))) #false)
  (check-equal? (contains-any-target? (sos '("S" "S" "O" "S") '("SOS"))) #true)
  (check-equal? (contains-any-target? (sos '("O" "S" "S" "O") '("OSO" "OSSO"))) #true))
(define (contains-any-target? state)
  (define board (sos-board state))
  (define targets (sos-targets state))
  ; [List-of SOS-Letter] [List-of SOS-Letter] -> Boolean
  ; Checks if the given target string is on the board
  (define (contains-target? b target)
    (define ts-length (length target))
    (cond
      [(<= (length b) ts-length) (equal? b target)]
      [else (or (equal? (take b ts-length) target)
                (contains-target? (rest b) target))]))
  ; String -> [List-of 1String]
  ; Explodes the string into a list of strings of length 1
  (define (explode s) (map string (string->list s)))
  ; Checks all target strings with contains-target?
  (ormap (lambda (target) (contains-target? board (explode target))) targets))

; SOS-State -> Result
; Computes the Result of the current board state.
(module+ test
  (check-equal? (result-of (sos '("S" "S" "O" "S") '("SOS"))) LOSS)
  (check-equal? (result-of (sos '("S" "S" "O" "O") '("SSS" "OOO"))) DRAW)
  (check-equal? (result-of (sos '("O" "O" "S") '("SOO" "OOS"))) WIN)
  (check-equal? (result-of (sos '(" " "S" "S" "O" "O") '("SOS" "OSO")))
                (sos '(" " "S" "S" "O" "O") '("SOS" "OSO"))))
(define (result-of state)
  (define board (sos-board state))
  (define number-of-moves (moves board))
  (define board-filled? (equal? number-of-moves (length board)))
  (cond
    [(contains-any-target? state) (if (even? number-of-moves) LOSS WIN)]
    [board-filled? DRAW]
    [else state]))

; SOS-State -> Result
; Finds all possible moves, sees which one is best with result-of,
; and then takes the best one for the player whose move it is.
(module+ test
  (check-equal? (best-next-move-result (sos '(" " " " " " " ") '("SOS"))) DRAW)
  (check-equal? (best-next-move-result (sos '("O" " " " " "O") '("SOS" "OSO"))) LOSS)
  (check-equal? (best-next-move-result (sos '("S" "O" " ") '("SOS"))) WIN))
(define (best-next-move-result state)
  (define board (sos-board state))
  (define targets (sos-targets state))
  ; [List-of SOS-Letter] Number -> [List-of [List-of SOS-Letter]]
  ; Computes the list of all possible next moves when
  ; the player plays on board positions from beginning up to index n
  (define (next-moves board n)
    (cond
      [(zero? n) '()]
      [(positive? n)
       (if (equal? (list-ref board (sub1 n)) " ")
           (let ([before (take board (sub1 n))]
                 [after (drop board n)])
             (cons (append before '("S") after)
                   (cons (append before '("O") after)
                         (next-moves board (sub1 n)))))
           (next-moves board (sub1 n)))]))
  (define all-next-moves (next-moves board (length board)))
  (define all-next-moves* (map (lambda (b) (sos b targets)) all-next-moves))
  (define results-of-all-next-moves (map result-of all-next-moves*))
  ; [List-of SOS-Letter] -> [Result Result -> Result]
  ; Determines which player it should optimize for
  ; Produces max if it's player 1, min if it's player 2
  (define parity-based-function
    (if (even? (moves board))
        max
        min))
  (result-max/min results-of-all-next-moves
                  parity-based-function))

; [List-of Result] [Result Result -> Result] -> Result
; Determines whether a player can win. If so, returns a win for that player
; If not, it continues analysis with best-next-move-result.
(module+ test
  (check-equal? (result-max/min (list WIN LOSS) max) WIN)
  (check-equal? (result-max/min (list WIN LOSS) min) LOSS))
(define (result-max/min alor C)
  (define looking-for (if (equal? C max) WIN LOSS))
  ; Result -> Result
  ; runs best-next-moves-result if the game should be continued
  (define (maybe-continue-game r)
    (if (sos? r)
        (best-next-move-result r)
        r))
  (if (member looking-for alor)
      looking-for
      (apply C (map maybe-continue-game alor))))

; [SOS-State -> Result] -> [SOS-State -> Result]
; Returns a memoized version of single parameter function
(define (memoize f)
  (define mem (make-hash))
  (define (g state)
    (if (hash-has-key? mem state)
        (hash-ref mem state)
        (let ([result (f state)])
          (hash-set! mem state result)
          result)))
  g)

; Since there are multiple ways to get to the same position,
; we want it to avoid recomputing positions, so we memoize it
(set! best-next-move-result (memoize best-next-move-result))
(set! result-of (memoize result-of))

; Number [List-of String] -> Result
; Creates an empty board of size n and evaluates the result with best play.
(module+ test
  (check-equal? (solver 7 '("SOS")) WIN)
  (check-equal? (solver 5 '("SOS")) DRAW)
  (check-equal? (solver 6 '("SOS" "OSO")) LOSS))
(define (solver n ts)
  (best-next-move-result (sos (make-list n " ") ts)))