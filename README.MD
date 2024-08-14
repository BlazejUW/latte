# Latte Language Compiler to LLVM

This project is a compiler for the Latte programming language, written in Haskell. The compiler generates LLVM intermediate code, which can be linked with the runtime support code provided in the project. It supports all basic functionalities of the Latte language and includes several optimizations.

## What is Latte?
Latte is a simple programming language designed for educational purposes, similar to C/Java. It focuses on fundamental constructs and functionalities such as loops, conditionals, and functions. You can find the complete language specification here: [Latte Language Specification](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2013/Latte/description.html).



## Features
The compiler supports the following features:
1. Frontend: 4/4 points
2. Backend: 8/8 points
3. Use of registers and phi nodes instead of alloc in LLVM: 2/2 points
4. Local Common Subexpression Elimination (LCSE): 3/3 points
5. Global Common Subexpression Elimination (GCSE): 2/2 points
6. Function inlining: 3/3 points (all functions except main and recursive ones are inlined)
7. Loop induction variables and strength reduction: 2/2 points

## Running the Compiler
To run the compiler, follow these steps:
* Use the `make` command in the project's root directory to create the executable file `latc`.
* Use `./latc filename.lat` to compile a .lat file. Compilation messages will be output to stderr. On success, LLVM code will be printed to stdout.
* Use `./latc_llvm filename.lat` to compile a .lat file. This will create an LLVM file named program.ll and a linked LLVM file program.bc, which can be used with lli.
* The `-no_inline` flag disables function inlining, allowing verification of program correctness without inlining. Use `./latc -no_inline filename.lat` to output LLVM code without inlining, and link it with runtime.bc for execution with lli.
* Alternatively, use `./latc_llvm -no_inline filename.lat` to produce an LLVM file named program.ll and a linked LLVM file program.bc with runtime.bc.
* You can also test the no_inline code directly with the command:

```
./latc -no_inline filename.lat | llvm-link -S ./lib/runtime.bc - | lli
```

### Project Structure
1. **Directory `src/Latte`**: Contains the main source code of the compiler, including essential Haskell modules:
   * `Abs.hs`: Syntax analysis module
   * `Lex.hs`: Lexer module
   * `Par.hs`: Parser module
   * `Print.hs`: Printer module
   * `Helpers.hs`: Helper functions
   * `Typechecker.hs`: Responsible for type checking the syntax tree
   * `Compiler.hs`: Transforms the syntax tree into LLVM intermediate code
   * `RunCompile.hs`: Application entry point managing parsing, type checking, and compilation
2. File  `src/Latte.cf`: The grammar file for the Latte language used by BNFC to generate the lexer and parser.

3. **Directory `lib`**: Contains auxiliary files such as `runtime.bc` (predefined LLVM functions) and `runtime.c` (source code for these functions).
4. **File `latc_llvm`**: A bash script that runs the compiler and links the generated LLVM code with the `lib` directory code, creating `program.ll` and `program.bc`.
5. **File `src/Makefile`**: Compiles the compiler and creates the executable `RunCompile` in `src/Latte`.
6. **File `Makefile`**: Invokes make in `src` and creates the executable `latc` in the root directory.
7. **File `latc`**: The executable file created after running make, linking to `RunCompile` in `src/Latte`.

## Compilation Details
1. **Building the Syntax Tree**:
   The process starts with building the syntax tree using BNFC (Bison Normal Form Converter), which reads the defined Latte grammar. This generates the lexer and parser, transforming the source code into a tree structure for further processing.
2. **Parsing**:
   The lexer and parser convert the source code into a syntax tree, enabling navigation and operations during compilation.
3. **Type Checking**:
   The `Typechecker` module verifies type correctness, checking variable and function declarations and usage. It also ensures the presence of return statements in functions, storing information in the `exprTypes` map for use during compilation.
4. **Compilation**:
   The `Compiler` module transforms the syntax tree into LLVM intermediate code, recursively traversing the tree and generating corresponding LLVM instructions. It avoids using `alloca` for memory allocation, instead utilizing SSA (Static Single Assignment) techniques. Expression types collected during type checking help in this process. Key details include:
   * **Handling While Loops**: Two-pass technique to identify and handle changing variables, using phi nodes and ensuring correct loop compilation.
5. **Compiler Structure**:
   The compilation logic is organized in `Compiler`, `Typechecker`, and `RunCompile` modules, with non-monadic helper functions in `Helpers.hs`.

## Optimization Implementations
***The next subsection describes examples included in the project that visualize the optimizations.***

### LCSE (Local Common Subexpression Elimination)
During compilation, information about expression types collected during type checking is utilized. When a new expression is encountered, the compiler checks if it has been computed before using an implemented expression comparison mechanism. If the expression has been computed before, the compiler inserts an LLVM assignment instruction to the variable holding the result of that expression instead of recomputing it. This avoids redundant computations, speeding up the program. Function calls are not replaced because it is not possible to determine if the function is pure. After an assignment to a variable, all expressions associated with that variable are removed from the expression set to ensure no outdated expressions are reused. The LCSE frame is cleared upon exiting a block in the current version of the compiler.

### GCSE (Global Common Subexpression Elimination)
GCSE works similarly to LCSE, but on a larger scope. If a subexpression is available on every possible path (e.g., computed in both the if and else branches of a CondElse), it is also recorded and reused. If the subexpression is available only in one branch (either if or else), it is not recorded.

### Function Inlining
During type checking, information about which functions are called within each function body is collected. A directed call graph is created, with nodes representing functions and edges representing function calls. Cycles in this graph are identified to prevent inlining recursive and mutually recursive functions. Initially, all functions are assumed to be inlinable. When a cycle is detected, all functions within the cycle are removed from the set of inlinable functions. This information is passed to the compiler, which checks if a function is inlinable when encountering a function call. If so, the function's code is inserted into the LLVM code at the call site; otherwise, a function call instruction is inserted. Inlined function code differs in that it does not return anything via a return statement. When such an instruction is encountered (a slightly different mechanism is used for if statements), a jump instruction to the end of the inlined function is inserted. **Appropriate comments are generated to indicate the start of the inlined function, the registers used as arguments, and the end of the inlined function.**

### Loop Induction Variables and Strength Reduction
- **Loop Induction Variables**:
  To identify loop induction variables, the loop body is traversed to find statements of the form `i = i + C` or `i++` or `i--`, where `i` is the variable identifier and `C` is a constant. According to the lecture definition, assignment to an induction variable can occur exactly once. If a second assignment to an induction variable is encountered, it is removed from the set of induction variables. Additionally, any induction variable appearing in an if or if-else block is removed from the set because it cannot be guaranteed to appear exactly once and be incremented consistently.

- **Strength Reduction**:
  In the compiler, any expression and subexpression that qualifies for strength reduction (i.e., of the form `A*i`, where `A` is a constant and `i` is an induction variable) is reduced. Once the induction variables for a loop are identified, expressions and subexpressions eligible for reduction are searched recursively within the loop body (excluding nested while loops with their own reduction stacks). If a qualifying expression is found, it is stored for processing, assigned a new reduction variable, and replaced with an `EVar` expression containing the new variable. After traversing the entire loop body, a block of reduction variable declarations is created and placed before the while loop. Reduction variables are initialized to the expressions they replaced. For example, if `i*4` is found, a new reduction variable `k` is created and initialized as:
``` int k = i*4; ```

The reduced multiplication by addition is recorded immediately after the induction variable increment. If the induction variable was incremented as `i = i + A` and the reduction variable `k = i*B`, then after encountering `i = i + A`, the assignment `k = k + A*B` is inserted, where `A*B` is a compile-time constant.

## Example Programs Demonstrating Optimizations
**All example programs demonstrating the optimizations are located in the `optimizations_examples/` directory.**

### LCSE: `lcse.lat`
This example demonstrates the reuse of previously computed subexpressions. The program begins with variable declarations and initializations, followed by a series of computations containing repeated subexpressions. The main focus is identifying and optimizing these repeating code fragments. The example includes several subexpressions that are ideal candidates for LCSE, such as `a + b` and `a - b`, used in various parts of the program. The compiler detects these repeating subexpressions, computes them once at the beginning of the code block, and reuses the results multiple times. When the value of variable `a` changes, the compiler recomputes the subexpressions and does not use outdated results, ensuring accurate calculations.

### GCSE: `gcse.lat`
Similar to LCSE, GCSE reuses previously computed subexpressions, but it applies to entire functions rather than single code blocks. This is illustrated by reusing a subexpression computed in both branches of an if-else statement. It is important to note that GCSE deals with subexpressions, not assignments. In the example, the subexpression `a-b+1` is assigned to the register `%phi_value_1` after the loop, and subsequent uses of this subexpression are replaced by this register. The subexpression `b+c` is not reused because it was not computed in both branches of the if-else statement.

### Function Inlining: `inline_functions.lat`
This example demonstrates function inlining. The program consists of three inlinable functions (`do_nothing_and_sum`, `square`, and `root`) and three non-inlinable functions (`factorial`, `even`, `odd`). Inlining is also applied to nested functions called within other functions. For example, when the `do_nothing_and_sum` function is called, the `square` and `root` functions are also inlined within the `main` function. The `factorial` function is recursive, so inlining is not applied to it. Mutual recursion is detected, so the `even` function is not inlined, but the `square` function within the `odd` function is inlined because non-recursive functions can be inlined within recursive functions.

### Loop Induction Variables and Strength Reduction: `loop_reduction.lat`
This example demonstrates **the detection of various loop induction variables** and **strength reduction in loops for different types of expressions and subexpressions**. All used variables are declared before the loop. The while loop contains two induction variables: one incremented using `++` and another that changes by a negative value each iteration. The example also shows strength reduction for each subexpression type eligible for reduction: in assignments, function calls, and simple expressions. In general, strength reduction works in every case according to the algorithm presented in the lecture.



## Environment
* **Haskell**: Written in Haskell. Requires GHC (Glasgow Haskell Compiler) to compile and run.
* **LLVM**: Requires LLVM tools such as `clang`, `llvm-config`, and `lli` to generate and run the LLVM code.
* **Make**: Requires `make` to use the provided Makefile for compilation.
* **BNFC, Happy, Alex**: Tools needed for generating the lexer and parser from the Latte grammar.

## Required Tools and Versions
To compile and run the Latte compiler, ensure you have the following tools installed:

* **Clang**: Tested with Debian clang version 14.0.6
* **LLVM**: Tested with LLVM version 14.0.6
* **GHC**:  Compiles Haskell code. Tested with The Glorious Glasgow Haskell Compilation System, version 9.0.2
* **Make**: Tested with GNU Make 4.3
* **BNFC**: Generates lexer and parser from Latte grammar. Tested with BNFC version 2.9.4
* **Happy**: Parser generator for Haskell. Tested with Happy Version 1.20.0
* **Alex**: Lexer generator for Haskell. Tested with Alex version 3.2.7.1

Ensure all the above tools are installed and accessible from the command line.


### Haskell Libraries:
* **Control.Monad**: Monadic functions for state and error handling.
* **Data.Map**: Manages maps (dictionaries) for storing variable, type, and function information.
* **Latte.Abs, Latte.Lex, Latte.Par, Latte.Print**: BNFC-generated modules for parsing and syntax tree building.
* **System.Environment, System.Exit, Control.Monad**: Handle system interaction, command-line arguments, and process control.
* **Prelude**: Basic Haskell library with common functions and operators.

## Contact Information
The compiler was created by me, Błażej Pałkus (Faculty of Mathematics, Informatics, and Mechanics at the University of Warsaw). The code is entirely my own work. Please do not copy it without permission. If you have any questions or need help, feel free to contact me via email: [blazej.palkus@gmail.com](mailto:blazej.palkus@gmail.com)

You can also ask questions by opening a new issue on GitHub. I will be happy to help with any inquiries or problems you might encounter.
