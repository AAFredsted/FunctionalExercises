
using System;

//Exercise 6.1
(*
Postfix form is a particular representation of arithmetic expressions where each operator is
preceded by its operand(s), for example:
(x + 7.0) has postfix form x 7.0 +
(x + 7.0) ∗ (x − 5.0) has postfix form x 7.0 + x 5.0 − ∗
Declare an F# function with type Fexpr -> string computing the textual, postfix form of
expression trees from Section 6.2.
*)
type Fexpr = 
        | Const of float
        | X
        | Add of Fexpr * Fexpr
        | Sub of Fexpr * Fexpr
        | Mul of Fexpr * Fexpr
        | Div of Fexpr * Fexpr
        | Sin of Fexpr
        | Cos of Fexpr
        | Log of Fexpr;;

let x = Add(Const 7.0, Const 3.5);;
let y = Add(Mul(Const 5.1, Const 7.5), Div(Const 10.6,  Const 5.5));;

let rec postfixEval (fexpr: Fexpr) = 
                match fexpr  with 
                        | Const(value) -> string value
                        | X -> "x"
                        | Add(expr1, expr2) -> postfixEval expr1 + " " + postfixEval expr2 + " +" 
                        | Sub(expr1, expr2) -> postfixEval expr1 + " " + postfixEval expr2 + " -"
                        | Mul(expr1, expr2) -> postfixEval expr1 + " " + postfixEval expr2 + " *"
                        | Div(expr1, expr2) -> postfixEval expr1 + " " + postfixEval expr2 + " /"
                        | Sin(expr) -> postfixEval expr + " sin"
                        | Cos(expr) -> postfixEval expr + " cos"
                        | Log(expr) -> postfixEval expr + " log";;

postfixEval x;;
postfixEval y;;
        

//6.2
(*
We consider a simple calculator with instructions for addition, subtraction, multiplication and
division of floats, and the functions: sin, cos, log and exp.
The instruction set of the calculator is modelled by the following F# type:


type Instruction = | ADD | SUB | MULT | DIV | SIN | COS | LOG | EXP | PUSH of float

The calculator is a stack machine, where a stack is a list of floats.
The execution of an instruction maps a stack to a new stack:
The execution of ADD with stack a b c · · · yields a new stack: (b + a) c · · · , where the top
two elements a and b on the stack have been replaced by the single element (b + a). Similarly
with regard to the instructions, SUB, MULT and DIV, which all work on the top two elements
of the stack.

The execution of one of the instructions SIN, COS, LOG and EXP applies the correspond-
ing function to the top element of the stack. For example, the execution of LOG with stack
a b c · · · yields the new stack: log(a) b c · · · .
The execution of PUSH r with the stack a b c · · · pushes r on top of the stack, that is, the
new stack is: r a b c · · · .

1. Declare a type Stack for representing the stack, and declare an F# function to interpret the
execution of a single instruction:
        intpInstr: Stack -> Instruction -> Stack

2. A program for the calculator is a list of instructions [i1 , i2 , . . . , in ]. A program is executed
by executing the instructions i1 , i2 , . . . , in one after the other, in that order, starting with an
empty stack. The result of the execution is the top value of the stack when all instructions
have been executed.
Declare an F# function to interpret the execution of a program:
        intpProg: Instruction list -> float

3. Declare an F# function
        trans: Fexpr * float -> Instruction list

where Fexpr is the type for expression trees declared in Section 6.2. The value of the ex-
pression trans(fe, x) is a program prg such that intpProg(prg) gives the float value of
fe when X has the value x. Hint: The instruction list can be obtained from the postfix form of
the expression. (See Exercise 6.2.)
*)


type Instruction = | ADD | SUB | MULT | DIV | SIN | COS | LOG | EXP | PUSH of float

type Stack = S of float List

let push (item: float) (stack: Stack) =
    match stack with
    | S(items) -> S(item :: items)

let intpInstr (s: Stack) (i: Instruction) =
    match (s, i) with
    | (S(x::xs), instruction) when List.length xs > 0 ->
        let newVal =
            match instruction with
            | ADD -> x + (List.head xs)
            | SUB -> x - (List.head xs)
            | MULT -> x * (List.head xs)
            | DIV -> x / (List.head xs)
            | _ -> failwith "Invalid operation: at least two elements must be present in the stack" // These operations require two elements
        S (newVal::(List.tail xs))
    | (S(x::xs), instruction) ->
        let newVal =
            match instruction with
            | SIN -> System.Math.Sin(x)
            | COS -> System.Math.Cos(x)
            | LOG -> System.Math.Log(x)
            | EXP -> System.Math.Exp(x)
            | _ -> failwith "Invalid operation: at least one element must be present in the stack" // These operations require one element
        S (newVal::xs)
    | (s, PUSH f) -> push f s
    | _ -> failwith "stop"

    
               


