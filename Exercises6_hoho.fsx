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



// Exercise 6.2
type Stack = S of float list
type Instruction = | ADD | SUB | MULT | DIV | SIN | COS | LOG | EXP | PUSH of float

let intpInstr (S l) = function
    | ADD -> 
        match l with
        | a::b::rest -> S((b+a)::rest)
        | _ -> failwith "Need at least two arguments"
    | SUB ->
        match l with
        | a::b::rest -> S((b-a)::rest)
        | _ -> failwith "Need at least two arguments"
    | MULT ->
        match l with
        | a::b::rest -> S((b*a)::rest)
        | _ -> failwith "Need at least two arguments"
    | DIV ->
        match l with
        | a::b::rest -> S((b/a)::rest)
        | _ -> failwith "Need at least two arguments"
    | SIN ->
        match l with
        | a::rest -> S(System.Math.Sin(a)::rest)
        | _ -> failwith "Need at least one argument"
    | COS ->
        match l with
        | a::rest -> S(System.Math.Cos(a)::rest)
        | _ -> failwith "Need at least one argument"
    | LOG ->
        match l with
        | a::_ when a<0.0 -> failwith "Cannot do logarithm on a negative number"
        | a::rest -> S(System.Math.Log(a)::rest)
        | _ -> failwith "Need at least one argument"
    | EXP ->
        match l with
        | a::rest -> S(System.Math.Exp(a)::rest)
        | _ -> failwith "Need at least one argument"
    | PUSH r -> S(r::l)


let intpProg l =
    match List.fold intpInstr (S []) l with
    | S l ->
        match l with
        | [] -> failwith "The stack is empty"
        | _ -> l.[0]

// here is an example to show that it works:
intpProg [PUSH 5.0; PUSH 4.0; MULT] // outputs float = 20.0 as it should

// in the trans-function, we want convert the Fexpr-expression to a string-representation using the
// fexprToString function. Then, we want to convert that string in to a list where each element are the 
// elements from the string separated by spaces. Then, we want to use List.map on every element of the
// list to convert the list into an Instruction list

let convToInstr x e =
    match e with
    | "x" -> PUSH x
    | "+" -> ADD
    | "-" -> SUB
    | "*" -> MULT
    | "/" -> DIV
    | "sin" -> SIN
    | "cos" -> COS
    | "log" -> LOG
    | "exp" -> EXP
    | n -> PUSH (float n)
    | _ -> failwith "Fexpr not valid"

let trans (fe, x) =
    let expStr = fexprToString fe
    let l = Seq.toList (expStr.Split())
    List.map (convToInstr x) l

// here is an example to show that it works:
trans ((Mul(Add(X, Const 5.0), X)), 3.0) // outputs Instruction list = [PUSH 3.0; PUSH 5.0; ADD; PUSH 3.0; MULT]



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