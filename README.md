# LISP Demo Programs

This repository contains a collection of basic LISP programs to help beginners understand the fundamentals of LISP programming. These examples are designed for learning and testing in your local environment.

## What is LISP?

**LISP (LISt Processing)** is one of the oldest high-level programming languages, dating back to 1958. It's known for its unique syntax based on S-expressions and its powerful capabilities in functional programming, symbolic computation, and artificial intelligence applications.

## Installation

### macOS
```bash
brew install sbcl
```

### Windows
1. Download the SBCL installer from [the official website](http://www.sbcl.org/platform-table.html)
2. Run the installer and follow the instructions
3. Add SBCL to your PATH environment variable

## Running LISP Programs

Execute a LISP file with SBCL:
```bash
sbcl --script filename.lsp
```

## LISP Syntax Basics

### S-expressions
LISP code consists of S-expressions (symbolic expressions), which are either atoms or lists:
```lisp
;; Atom examples
42
"hello"
symbol

;; List examples
(+ 1 2)
(print "Hello")
(defun function-name (parameters) (body))
```

### Comments
```lisp
;; This is a comment
```

### Basic Functions
```lisp
(+ 1 2 3)    ; Addition: returns 6
(- 10 5)     ; Subtraction: returns 5
(* 2 3 4)    ; Multiplication: returns 24
(/ 10 2)     ; Division: returns 5
```

### Variables
```lisp
;; Global variables
(defparameter *variable-name* value)
(defvar *variable-name* value)

;; Local variables
(let ((variable-name value)) body)

;; Constants
(defconstant CONSTANT-NAME value)
```

### Function Definition
```lisp
(defun function-name (parameters)
  "Optional documentation string."
  body)
```

## Key Features of LISP

1. **Homoiconicity** - LISP code is written in the same structure as LISP data, allowing code to be manipulated as data.

2. **Functional Programming** - First-class functions, higher-order functions, and pure functions.

3. **Dynamic Typing** - Variables can hold values of any type.

4. **Garbage Collection** - Automatic memory management.

5. **Interactive Development** - REPL (Read-Eval-Print Loop) for interactive programming.

6. **Macros** - Powerful code generation capabilities using macros.

7. **Condition System** - Sophisticated error handling system.

## File Descriptions

### app.lsp
Demonstrates basic LISP syntax and concepts:
- Hello World examples
- Variable types and scope (global, local, constants)
- Basic functions and mathematical operations
- Higher-order functions with `mapcar`
- Loop constructs

Key features demonstrated:
- Variable scoping with `let`, `defparameter`, and `defvar`
- Constants with `defconstant`
- Function definition with `defun`
- List processing
- Built-in functions like `mapcar`

### foodliking.lsp
A simple knowledge representation system using LISP:
- Facts defined as lists
- Rules implemented as functions
- Queries demonstrated through function calls

Key features demonstrated:
- List manipulation
- Conditional logic with `cond`
- Symbol comparison
- Knowledge representation techniques

## Learning Resources

- [Practical Common Lisp](http://www.gigamonkeys.com/book/)
- [Common Lisp: A Gentle Introduction to Symbolic Computation](https://www.cs.cmu.edu/~dst/LispBook/)
- [Land of Lisp](http://landoflisp.com/)
- [The Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/)

## Why Learn LISP?

- Expands your thinking about programming paradigms
- Powerful macro system for metaprogramming
- Excellent for prototyping and AI applications
- Improves your understanding of functional programming concepts
- Historical significance in computer science

Feel free to explore, modify, and expand upon these examples!
