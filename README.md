# Sudoku-Hidato
Sudoku Hidato solved in Haskell, class project for 4th Degree course of Computer Science in Havana University, subject: "Programación Declarativa"\
For more detailed information see [Project Specifications]() (Spanish) and [Final Report]() (Spanish)

Programmed and tested in Windows 10
Haskell compilator ghc 8.10.2

Dependencies:\
package random\
Installation: stack install random\
Usage: import System.Random


tester.hs computes the solution for the sample from the project specifications

**Hidato**
```
000 033 035 000 000 0-1 0-1 0-1
000 000 024 022 000 0-1 0-1 0-1
000 000 000 021 000 000 0-1 0-1
000 026 000 013 040 011 0-1 0-1
027 000 000 000 009 000 001 0-1
0-1 0-1 000 000 018 000 000 0-1
0-1 0-1 0-1 0-1 000 007 000 000
0-1 0-1 0-1 0-1 0-1 0-1 005 000
```

**Solution**

```
032 033 035 036 037 0-1 0-1 0-1
031 034 024 022 038 0-1 0-1 0-1
030 025 023 021 012 039 0-1 0-1
029 026 020 013 040 011 0-1 0-1
027 028 014 019 009 010 001 0-1
0-1 0-1 015 016 018 008 002 0-1
0-1 0-1 0-1 0-1 017 007 006 003
0-1 0-1 0-1 0-1 0-1 0-1 005 004
```

The cells are formatted to size 3 for better display experience:
- -1 cells stands for black cells or holes
- 0 cells stands for white cells or empty spaces

generator.hs main program, generates a random hidato with matrix form (from 3x3 until 10x10) and the solution computed by the solver algorithm on solver.hs