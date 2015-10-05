#lang ragg

program: statement*
statement: "import" VARIABLE VARIABLE ";"
         | lvalue assignment-op rvalue ";"
         | call ";"
         | "~ATH" "(" rvalue ")" statement "EXECUTE" "(" statement-or-rvalue ")" ";"
         | "{" statement* "}"

statement-or-rvalue: statement
                   | rvalue-exec-wrapper

rvalue-exec-wrapper: rvalue

lvalue: VARIABLE
      | rvalue "[" rvalue "]"
      | rvalue6 "." VARIABLE

rvalue: rvalue1
      | rvalue1 logic-op rvalue

rvalue1: rvalue2
       | rvalue2 comparison-op rvalue1

rvalue2: rvalue3
       | rvalue3 bin-op rvalue2

rvalue3: rvalue4
       | rvalue4 add-op rvalue3

rvalue4: rvalue5
       | rvalue5 mul-op rvalue4

rvalue5: rvalue6
       | unary-op+ rvalue6

rvalue6: rvalue7
       | call

call: rvalue6 "(" [ rvalue ] ( "," rvalue )* ")"

rvalue7: lvalue | constant | "(" rvalue ")"

constant: INTEGER | boolean | "NULL" | STRING

boolean: "TRUE" | "FALSE"

assignment-op: "=" | logic-op "=" | bin-op "=" | add-op "=" | mul-op "="

logic-op: "&" "&" | "|" "|" | "^" "^"

comparison-op: "=" "=" | "<" "=" | ">" "=" | "<" | ">" | "!" "="

bin-op: "&" | "|" | "^"

add-op: "+" | "-"

mul-op: "*" | "/"

unary-op: "-" | "!"
