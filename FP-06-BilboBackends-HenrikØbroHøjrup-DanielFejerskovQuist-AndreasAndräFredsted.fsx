// Exercise 6.1
type Fexpr = 
    | Const of float
    | X
    | Add of Fexpr * Fexpr
    | Sub of Fexpr * Fexpr
    | Mul of Fexpr * Fexpr
    | Div of Fexpr * Fexpr
    | Sin of Fexpr
    | Cos of Fexpr
    | Log of Fexpr
    | Exp of Fexpr

let rec fexprToString expr = 
    match expr with
    | Const x -> string x
    | X -> "x"
    | Add(x1,x2) -> (fexprToString x1) + " " + (fexprToString x2) + " +"
    | Sub(x1,x2) -> (fexprToString x1) + " " + (fexprToString x2) + " -"
    | Mul(x1,x2) -> (fexprToString x1) + " " + (fexprToString x2) + " *"
    | Div(x1,x2) -> (fexprToString x1) + " " + (fexprToString x2) + " /"
    | Sin x -> (fexprToString x) + " sin"
    | Cos x -> (fexprToString x) + " cos"
    | Log x -> (fexprToString x) + " log"
    | Exp x -> (fexprToString x) + " exp"

// here is an example to show that it works:
fexprToString (Mul(Const 4.0, Const 8.4)) // outputs string = "4 8.4 *"


//6.2

(*
1. Declare a type Stack for representing the stack, and declare an F# function to interpret the
execution of a single instruction:
        intpInstr: Stack -> Instruction -> Stack
*)


type Instruction = | ADD | SUB | MULT | DIV | SIN | COS | LOG | EXP | PUSH of float;;

type Stack = S of float List;;

let push (item: float) (stack: Stack) =
    match stack with
    | S(items) -> S(item :: items);;

//part one
let intpInstr (s: Stack) (i: Instruction) =
    match (s, i) with
    | (S(x::xs), instruction) when List.length xs > 1 ->
        let newVal =
            match instruction with
            | ADD -> (List.head xs) + x
            | SUB -> (List.head xs) - x
            | MULT -> (List.head xs) * x
            | DIV ->  (List.head xs) / x
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
    | _ -> failwith "stop";;
(*
    2. A program for the calculator is a list of instructions [i1 , i2 , . . . , in ]. A program is executed
by executing the instructions i1 , i2 , . . . , in one after the other, in that order, starting with an
empty stack. The result of the execution is the top value of the stack when all instructions
have been executed.
Declare an F# function to interpret the execution of a program:
        intpProg: Instruction list -> float


*)
let pull (s: Stack) = 
        match s with
            | S(x::xs) -> x
            | _ -> 0.0;;


let intpProg (input: Instruction List)  = 
        let rec performOprec (input: Instruction List) (s: Stack) = 
                        match input with
                                | x::xs ->  intpInstr s x |> performOprec  xs
                                | [] -> pull s
        performOprec input (S[]);;


let intpProgfold (input: Instruction List) = 
        let s = (S[])
        pull (List.fold (fun s l -> intpInstr s l) s input);;

(*
3. Declare an F# function
    trans: Fexpr * float -> Instruction list

    where Fexpr is the type for expression trees declared in Section 6.2. The value of the ex-
    pression trans(fe, x) is a program prg such that intpProg(prg) gives the float value of
    fe when X has the value x. Hint: The instruction list can be obtained from the postfix form of
    the expression. (See Exercise 6.2.
*)

let trans (input: Fexpr*float) = 
        match input with 
            | (fe, f) ->
                let fs = Seq.toList ((fexprToString fe).Split(' '))

                let getInsList (fl: float) (si: string) =
                            match si with 
                                | "+"-> ADD 
                                | "-" -> SUB
                                | "/" -> DIV
                                | "*" -> MULT
                                | "x" -> PUSH fl
                                | "sin" -> SIN
                                | "cos" -> COS
                                | "log" -> LOG
                                | x -> PUSH (float x)
                                | _ -> failwith "Fexpr cannot be evaluated"

                List.map (getInsList f) fs
            | _ -> failwith "Wrong input types provided";;


trans ((Mul(Add(X, Const 5.0), X)), 3.0);;



// Exercise 6.3

// ComplexNumber signature:
// module ComplexNumber
// type ComplexNumber
// val ( .+) : ComplexNumber -> ComplexNumber -> ComplexNumber
// val ( .*) : ComplexNumber -> ComplexNumber -> ComplexNumber
// val ( .-) : ComplexNumber -> ComplexNumber -> ComplexNumber
// val ( ./) : ComplexNumber -> ComplexNumber -> ComplexNumber
// val ( ../) : ComplexNumber -> ComplexNumber -> ComplexNumber
// val make : float * float -> ComplexNumber
// val toTup : ComplexNumber -> float * float

// ComplexNumber implementation:
module ComplexNumber
type ComplexNumber = C of float * float
let ( .+) C(a,b) C(c,d) = C(a+c,b+d)
let ( .*) C(a,b) C(c,d) = C(a*c-b*d, b*c+a*d)
let ( .-) C(a,b) C(c,d) = C(a-c,b-d)
let ( ./) C(a,b) C(c,d) = C(a*(c/(c*c+d*d))-b*(-d/(c*c+d*d)), b*(c/(c*c+d*d))+a*(-d/(c*c+d*d)))
let ( ../) C(a,b) C(c,d) = 
                        let invC = c/(c*c+d*d)
                        let invD = -d/(c*c+d*d)
                        C(a*invC-b*invD, b*invC+a*invD)
let make (a,b) = C(a,b)
let toTup C(a,b) = (a,b)