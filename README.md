# OctagonAI
An abstract interpreter and C program analyser in the Octagon abstract domain.

This project was created by Jérôme Boillot, Orégane Desrentes, Siang-Yun Lee, Dewmini Marakkalage for the Formal Verification class (CS-550) of autumn 2020 at EPFL.

# How to execute the project

Please use ocaml version 4.11.0 or later.

We recommend using `opam` for the module management.

Modules `zarith`, `frontc` and `dune` are necessary for this project to run (`opam install zarith frontc dune`)
`make` builds the project.
The first time you build the project with `make` you have to execute the following link command: `ln -s _build/install/default/bin/octagonai main.native` so it is easier to execute the analyser.
Once the project is built, you can analyse a program in the file `program.c` with the command `./main.native program.c`


To compile and run the example analysis written in the report, simply use the command `make run`.

# Tests

The command `./main.native --tests` runs a series of tests, in particular on abstraction and transfer functions, and the DBM (Difference Bound Matrix, which stores the information on variables for the analysis) manipulation.

### Test of DBM implementation
Tests adding constraints to the DBM and testing if they are satisfied.

### Test of Closure Algorithms
Starts with a DBM with a few constraints, and apply different closure algorithms to them : Shortest Path, Strong, Tight and Optimised Tight. This is tested with 3 different DBMs of different sizes.

### Test of Abstract Transfers
Tests the state of the DBM after assigning variables with linear combinations of other variables.

### Test of Timing in closure algorithms
Tests the time taken by the 4 algorithms cited above on DBM of size 25, 50, 100 and 200.


# License

The MIT License (MIT)

Copyright (c) 2020 Jérôme Boillot, Orégane Desrentes, Siang-Yun Lee, Dewmini Marakkalage

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
