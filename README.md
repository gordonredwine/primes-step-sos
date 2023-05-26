# PRIMES STEP SOS Solver

This SOS game solver was written for the 2022-2023 PRIMES STEP research project.

The SOS solver, when given a board size and a list of target strings, is able to give the result if both player play optimally. The solver does a complete search of possible moves, which means that larger boards can take quite some time to run.

## Usage
To use it, first you need to install the [Racket](https://racket-lang.org) programming language. Then you can run the Racket interpreter and load the program. You can do this either in the DrRacket interface, or on the command line. Here is one way on the command line:
```bash
racket --repl --eval '(enter! (file "primes-step-sos/sos-solver.rkt"))'
```

At the Racket prompt, you can use the ``solver`` function as such:
```scheme
"primes-step-sos/sos-solver.rkt"> (solver 9 '("SOS"))
1.0
```
Which would give you the result for a 1x9 board with the winning target string of "SOS". You can also use this for multiple target strings:
```scheme
"primes-step-sos/sos-solver.rkt"> (solver 8 '("SSSS" "OOOO"))
0.5
```
This would give you the result for a 1x8 board with two winning target strings: SSSS and OOOO. Note that ``1.0`` means that the first player wins, ``0.0`` means that the second player wins, and ``0.5`` means that it's a draw with best play.