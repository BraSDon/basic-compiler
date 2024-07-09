# Compiler BASIC-like language to C

Compiler for a BASIC-like language, inspired by the [teeny-tiny-compiler](https://austinhenley.com/blog/teenytinycompiler1.html) by Austin Henley.

## Usage Example

1. Write a program in the BASIC-like language and store it in examples/<program_name>.txt. Here we use parse-if.txt
    ```txt
    LET bar = 5
    LET foo = bar * 3 + 2
    IF foo > 0 THEN
        PRINT "yes!"
    ENDIF
    ```
2. Compile and run the program
    ```bash
    $ sh compile.sh parse-if
    Filepath input: examples/parse-if.txt
    Filepath output: examples/c-files/parse-if.c
    Compiling...
        Finished release [optimized] target(s) in 0.00s
         Running `target/release/basic-compiler parse-if`
    Running... 

    yes!
    ```

## Getting Started

### Pre-requisites

1. [Rust + Cargo](https://www.rust-lang.org/tools/install)

### Running the application

1. Clone the repository  
    HTTPS:
    ```bash
    git clone https://github.com/BraSDon/basic-compiler.git
    ```

    SSH:
    ```bash
    git clone git@github.com:BraSDon/basic-compiler.git
    ```
2. Change into project directory
    ```bash
    cd basic-compiler
    ```
3. Build the project
    ```bash
    cargo build --release
    ```
4. Compile and run one of the example programs; or create your own in examples/
    ```bash
    sh compile.sh parse-if
    ```

## Language Grammar

```
program ::= {statement}  
statement ::= "PRINT" (expression | string) nl  
    | "IF" comparison "THEN" nl {statement} "ENDIF" nl  
    | "WHILE" comparison "REPEAT" nl {statement} "ENDWHILE" nl   
    | "LABEL" ident nl    
    | "GOTO" ident nl    
    | "LET" ident "=" expression nl    
    | "INPUT" ident nl    
comparison ::= expression (("==" | "!=" | ">" | ">=" | "<" | "<=") expression)+    
expression ::= term {( "-" | "+" ) term}    
term ::= unary {( "/" | "*" ) unary}    
unary ::= ["+" | "-"] primary    
primary ::= number | ident    
nl ::= '\n'+    
```
