# Mail

Mail is a simple, typed programming language that compiles to LLVM IR, which was created as a project in order to learn about LLVM.
It is implemented with Haskell and uses the [llvm-hs](https://hackage.haskell.org/package/llvm-hs) and [llvm-hs-pure](https://hackage.haskell.org/package/llvm-hs-pure) packages to generate LLVM IR.

## A Quick Example

```scheme
; Compute the nth fibonacci number (indexing from 0)
(define fib ([n Int]) -> Int
  (define curr 1)
  (define prev 1)
  (define i 1)
  (while (< i n)
    (define next (+ curr prev))
    (set! prev curr)
    (set! curr next)
    (set! i (+ i 1)))
  curr)

(fib 6) ; evaluates to 13
```

## More Examples

There are many example Mail programs located in the `./examples` directory.
For each example, the compiled program as LLVM IR is also included in the `./examples` directory.

## Language Spec

### Syntax

A Mail program is a series of s-expressions which conform to the below grammar.
S-expressions may be enclosed using (), [], or {}.
Single line comments may be defined using `;`, and nested block comments can be defined using `#|` and `|#`.

```bnf
program := def ... e

def := tdef
     | fdef

type-def := (struct <sym> (<sym> type) ...)
          | (union <sym> (<sym> type) ...)

fun-def := (define <sym> ((<sym> type) ...) -> type eseq)

type := Int
      | Float
      | Bool
      | Unit
      | (Ptr type)
      | <sym>

e := <int>
  | <float>
  | #true
  | #false
  | unit
  | <sym>
  | (begin eseq)
  | (let (<sym> e) eseq)
  | (set! <sym> e)
  | (set-ptr! e e)
  | (get-ptr e)
  | (alloc e)
  | (dealloc e)
  | (if e e e)
  | (while e eseq)
  | (match e ((<sym> <sym>) eseq) ...)
  | (<sym> e ...)

eseq := e
      | e eseq
      | (define <sym> e) eseq
```

### Type System and Semantics

#### Program Structure
A program consists of a series of type and function definitions, followed by an expression.
The expression is expected to be of type `Int`.
The compiler creates an executable that simply computes the expression and prints its result to stdout.

#### Primitives
The primitive types of Mail are `Int`, `Float`, `Bool`, and `Unit`.
`Int` represents 64-bit integers, and `Float` represents 64-bit floating point numbers.
Integers, floats, booleans, and units can be created with literal expressions, of the forms `(+|-)?[0-9]+`, `(+|-)?[0-9]+.[0-9]+`, `(#true|#false)`, and `unit`, respectively.

#### Pointers
Pointers are created by calling `alloc`.
`(alloc e)` allocates memory on heap, stores `e` inside it, and returns a pointer to that memory slot.
If `e` has type `t`, `(alloc e)` has type `(Ptr t)`.
(`e` may be any expression of any type.)
The value inside the pointer can be retrieved via `(get-ptr p)` and updated via `(set-ptr! p e)`, where `p` is a pointer value and `e` is the expression being written into the pointer.
`set-ptr!`, as with all side-effectful operations, has type `unit`.
Mail is not garbage collected, so to avoid memory leaks, pointers must be deallocated.
This may be done with `(dealloc p)`, which frees the memory that p points to.

#### Variables

Variables can be defined via a let binding, such as `(let (x e1) e2)`.
This evaluates `e1` and then stores its value on the stack.
The value can be retrieved within `e2` be referring to `x`.
Variables may be mutated via `(set! x e)`, where `e` is its new value.

#### Functions

Functions are not first-class, and they may be mutually recursive.
Function definitions consist of a list of parameters (each of which has a name and a type), a return type, and a body.
The body's type is expected to match the return type.
Functions can be called with the syntax `(<fun-name> <arg1> <arg2> ...)` (there may be 0 arguments).
Arguments are passed by value rather than by reference, (values within structs and unions are as well).

#### Composite Types
There are two kinds of composite types: structs and unions.
Both of them allow a limited form of mutual recursion.
Within their definitions, types defined before them are allowed to be freely referenced.
All other types may be referenced, but only within pointers.
This restriction avoids the issue of needing infinitely large structs to hold recursive types without needing to implement cycle-checking.

Struct definitions consist of a series of fields, each with a name and a type.
A struct definition implicitly defines many functions.
To create instances of the struct, a function is defined with the name of the struct that takes arguments corresponding to the fields.
For example, instantiating `(struct Foo (a Int) (b Float))` may look like `(Foo 5 10.0)`.
There is also a function generated for each field, which takes in a struct instance and returns the value of the field for that instance.
Using the `Foo` example, `(a (Foo 5 10.0))` would give `5`.
Note that because universal functions are created for each field, field names may not be repeated, even across structs, nor may they conflict with function names.

Union definitions consist of a series of constructors, each with a name and a type.
Unions in Mail are tagged.
Like structs, some functions are implicitly defined.
For each constructor, a function with the constructor's name is created, which takes in a value of the constructor's type and returns an instance of a union.
For example, `(Union Int? (Some Int) (None Unit))` may have instances created via `(Some 10)` or `(None unit)`.
The value of a union must be accessed via a match statement, which is covered in **Control Flow**.

#### Control Flow

Mail offers the control structures of `if` and `while`.
If takes three elements: a condition, a then-body, and an else-body.
If the condition evaluates to `#true`, it evaluates and returns the true-body, and otherwise evaluates and returns the false-body.
The true-body and false-body must have the same type.
While has a condition and a body.
The body is expected to be of type `Unit`.
The body is repeatedly executed while the condition evaluates to `#true`.

In addition, there are match statements, which can pattern match over a union value.
A match statement consists of a series of cases, each of which matches a constructor of the union, defines a variable, and has a body.
The body of whichever case matches the value is executed and returned, with the variable being bound to the contents of the value.
For example,
```scheme
(match (Left 10)
  ([Left x] (+ x 5))
  ([Right x] 20))
```
would return `15`, since the `Left` case of the `match` is matched, `x` is then bound to `10`, and `(+ x 5)` evaluates to `15`.
Matches must be exhaustive and cannot repeat constructors.

#### Expression Sequences

In some places, __sequences__ of expressions are allowed rather than single expressions.
These are in the bodies of functions, while loops, and match cases.
Additionally, there is a special `begin` form that simply allows for an expression sequence to be written (ex: `(begin e1 e2 e3)`).

An expression sequences expect all expressions except for the last to be of type unit, and the entire sequence evaluates to the result of the last expression.
There is also a special syntax for defining variables within an expression sequence.
`(define x v) e ...` is equivalent to `(let (x v) e)`.
This allows writing expressions like:
```scheme
(begin
  (define x 10)
  (set! x 20)
  x) ; evaluates to 20
```

#### Built-Ins

The operators `+`, `-`, `*`, `/`, `%`, `<`, `<=`, `>`, `>=`, `=`, `!=`, `+.`, `-.`, `*.`, `/.`, `<.`, `<=.`, `>.`, `>=.`, `=.`, `!=.`, `and`, `or`, and `not` are all defined.
Operators with a trailing `.` are defined on floats, while numeric operators without a `.` are defined on integers.

## Installation

Mail requires [Cabal](https://www.haskell.org/cabal/) to build and install and has only been tested with GHC 8.8.4.
Additionally, it requires LLVM 9.0.x to be installed.
This specific verison requirement is a result of [llvm-hs](https://hackage.haskell.org/package/llvm-hs), the Haskell library for LLVM bindings, not supporting more recent versions.
`llvm-hs` provides instructions for installing LLVM 9.0.x in its [README](https://github.com/llvm-hs/llvm-hs/tree/llvm-9#installing-llvm).

Once Cabal and LLVM 9 are installed, Mail can be installed by cloning this repository and running `cabal install mail`.

Because installing LLVM 9 is non-trivial, a Docker image is also provided at `liam923/mail:latest`.
This image has Mail pre-installed, and it is capable of building and testing it.

## Usage

Once installed, the compiler can be invoked via `mailc` (or `docker run liam923/mail:latest mailc`).
To compile a program `foo.mail`, simply run `mailc foo.mail`.
This will generate the files `foo.ll`, the LLVM IR translation of the program (note that this does note include the runtime and thus cannot be compiled on its own), and `foo`, an executable.
To also run the program immediately after compiling, the `--run` flag can be included.

Note that compiling a program within the Docker container will a) create an executable for running within the container rather than the host system and b) may require mounting a directory to the container in order to pass files between the host and container.
It may be simpler to launch a shell within the container via `docker run -it liam923/mail:latest` and then create and run the program directly via `mailc`.

## Run the Tests

`cabal test` or `docker run liam923/mail:latest cabal test`.

## Why the name?

I'm uncreative and "Mail" is my name backwards.
