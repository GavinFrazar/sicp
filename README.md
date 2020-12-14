# SICP Exercises
#### This is a WIP collection of my solutions to exercises from the book *Structure and Interpretation of Computer Programs* (SICP).

I skipped over some exercises that were too tedious/boring. I will revisit those exercises at a later time.

I used [Racket](https://racket-lang.org/) exclusively for all the exercises. Racket is unique amongst languages (even in LISP) with its `#lang` directive.
It ships with a [`#lang sicp`](https://docs.racket-lang.org/sicp-manual/SICP_Language.html) which I make good use of for most exercises;
it's essentially R5RS with some SICP specific additions to make the environment setup smoother. Notably, racket doesn't like mutation and even removed `set-car!` and friends
from its implementation (actually they just moved it to `mcons` `set-mcar!` etc.).

For chapter 4 I have included a complete data-directed implementation of the initial version of the evaluator.
I also included some tests for that evaluator and I will expand on those tests more later.
My goal with Chapter 4 is to easily modify the evaluator while still covering functionality with a core test suite. Additional tests will be included for each exercise.

My goal with this repo is to compile my solutions for every exercise in the book in one place.
I eventually want to provide a documented approach to working through SICP using Racket.
SICP is a fantastic text which is unfortunately difficult to approach due to environment setup issues.
Projects like [zv's guide](https://github.com/zv/SICP-guile) are extremely useful in getting through the book, but I still find there is a lot of unnecessary friction.
I want to make a quickstart repo which moves all of the hassle out of the way for SICP readers.
