

//Exercise 5.1

//Firstly, we define a BinTree datatype for us to start working on:

type 'a BinTree =
    | Leaf
    | Node of 'a * 'a BinTree * 'a BinTree

//we define a function to traverse the tree in order
//the function is based on the preOrder function from slide 32
let rec inOrder tree =
    match tree with
        Leaf -> []
        | Node(n,treeL,treeR) ->
        //as we are need to perform array appending on the elements but cannot use the cons-operator, we put n inside of a list
        inOrder treeL @ [n] @ inOrder treeR;

//to test that the function works, we define a variable of type BinTree and test the function:
let intBinTree =
    Node(43, Node(25, Node(56, Leaf, Leaf), Leaf),
        Node(562, Leaf, Node(78, Leaf, Leaf)))

inOrder intBinTree
//It works !!


//Exercise 5.2

//the mapInOrder function is based on the same recursive logic as inOrder, 
//but maps a Leaf to a Leaf and a Node to a Node instead, where the function is applied to any values stored in the newly constructed nodes.

let rec mapInOrder f = function
    | Leaf -> Leaf
    | Node(x,tl,tr) -> Node(f x,mapInOrder f tl,mapInOrder f tr);;

mapInOrder (fun x -> x + 1) intBinTree;;
//and it works :)

// to evaluate how a reordering of the operations from mapInOrder to mapPostOrder could affect the resulting values of the trees, we will implement mapPostOrder:

let rec mapPostOrder (f: 'a -> 'b) (tree: 'a BinTree) = 
                    match tree with
                        Leaf -> Leaf
                        | Node(n, treeL, treeR) ->
                            let R = mapPostOrder f treeR
                            let N = f n
                            let L = mapPostOrder f treeL
                            Node(N, L, R);;

//as shown, both functions map nodes to nodes with the same ordering of their triplets,
//meaning that the structure of the tree for both functions will be the same.
// As the functions only accept static functions acting only on n, I do not see how a non-associative function can be passed to mapPostOrder and mapInOrder

//Exercise 5.3

//Taking inspiration from the implementation of fold from lecture 4 slide 21 and  the inFoldBack function from lecture 5 slide 9, 
//the following was produced
let foldInOrder f e b = List.fold f e (inOrder b);;

//however, the implementation following the inFoldBack from the lecture required the use of the in-keyword....                                                                        

let floatBinTree = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf),
                                            Node(562.0, Leaf, Node(78.0, Leaf,Leaf)));;

foldInOrder (fun n a -> a + n) 0.0 floatBinTree;;

//it works


//Exercise 5.4

//Setup for implementation
type aExp = (* Arithmetical expressions *)
        | N of int (* numbers *)
        | V of string (* variables *)
        | Add of aExp * aExp (* addition *)
        | Mul of aExp * aExp (* multiplication *)
        | Sub of aExp * aExp (* subtraction *)


type bExp = (* Boolean expressions *)
        | TT (* true *)
        | FF (* false *)
        | Eq of aExp * aExp (* equality *)
        | Lt of aExp * aExp (* less than *)
        | Neg of bExp (* negation *)
        | Con of bExp * bExp (* conjunction *)


type stm = (* statements *)
        | Ass of string * aExp (* assignment *)
        | Skip
        | Seq of stm * stm (* sequential composition *)
        | ITE of bExp * stm * stm (* if-then-else *)
        | While of bExp * stm (* while *)
        | IT of bExp * stm (* if-then*)
        | RU of bExp * stm (* repeat-until *)        


//To implement thefull operability of the above types, we must first have a way to modify our current state 
let update key  value state = Map.add key value state;;

//then, we must be able to

let rec A exp state = 
                            match exp with 
                                | N n -> n
                                | V v -> 
                                    try
                                        Map.find v state
                                    with
                                        | :? System.AccessViolationException as ex -> 
                                            printfn "value not defined in scope. Error message: %s" ex.Message
                                            0
                                        | ex ->
                                            printfn "An error occured: %s" ex.Message
                                            0
                                | Add(exp1, exp2) -> A exp1 state + A exp2 state
                                | Mul(exp1, exp2) -> A exp1 state * A exp2 state
                                | Sub(exp1, exp2) -> A exp1 state - A exp2 state;;
        

let rec B exp state  = 
                            match exp with
                                | TT -> true
                                | FF -> false
                                | Eq(exp1, exp2) -> A exp1  state <> A exp2 state
                                | Lt(exp1, exp2) -> A exp1 state < A exp2 state
                                | Neg(exp) -> not (B exp state)
                                | Con(exp1, exp2) -> B exp1 state && B exp2 state;;
 

 let rec I stm state = 
                            match stm with
                                | Ass(name, exp) -> update name (A exp state) state
                                | Skip -> state  
                                | Seq(stm1, stm2) -> I stm1 state |> I stm2 
                                | ITE(bexp, exp1, exp2) -> if B bexp state then I exp1 state else I exp2 state
                                | While(bool, exp) ->
                                    if B bool state then
                                        let newState = I exp state
                                        I (While(bool, exp)) newState 
                                    else 
                                        state
                                | IT(bexp, exp) -> if B bexp state then I exp state else I Skip state
                                | RU(bool, exp) -> 
                                    if B (Neg(bool)) state then
                                        let newState = I exp state
                                        I (RU(bool, exp)) newState
                                    else
                                        state;;


//with the implementation of statements complete, we can test the solution to exercise 5.4:                                                     

//test0 addition

let stmt0 = Ass("res",(Add(N 10, N 30)));;
let state0 = Map.empty;;

I stmt0 state0;;


//test1 multiplication

let stmt1 = Ass("a", Mul(N 10, N 10));;
let state1 = Map.empty;;

I stmt1 state1;;



//test2 if statements

 let stmt2 = ITE(
                        Lt(V("a"), V("b")),
                        Ass("a", V("b")),
                        Ass("b", V("a"))
                        )
let state2_1 = Map.empty.Add("a", 5).Add("b", 10);;
let state2_2 = Map.empty.Add("a", 15).Add("b", 10);;

I stmt2 state2_1;; //should be 10
I stmt2 state2_2;; //should be 15

//yayyy, they work

//test3: whileLoop

let stmt3 = While(
                        Lt(V "x", N 10),
                        Ass("x", Add(V "x", N 1))
                        )
let state3 = Map.empty.Add("x", 0);

I stmt3 state3;;


//test4: more advanced loop:

let stmt4 = 
                While(
                        Lt( Mul(V "root", V "root"), V "n" ),
                        Ass("root", Add(V "root", N 1))
                    );;
let state4 = Map.empty.Add("n", 16).Add("root", 0);;

I stmt4 state4;;

//test5 

let stmt5 =
        While(
                Lt(V "val", V "toAssess"),
                    Seq(
                        Ass("val", Mul(V "val", N 2) ),
                        Ass("pow2", Add(V "pow2", N 1))
                    )
        );;


let state5 = Map.empty.Add("toAssess", 64).Add("val", 1).Add("pow2", 0);;

I stmt5 state5;;


//Exercise 5.5

(*
    Exercise 5.5 was solved with inspiration from the allready implemented operations. The IT operation is the same as a ITE operation where 
    the else leads to a Skip-operation. Similarly, the RU operation is the same as a while-operation where it runs as long as the boolean expression
    evaluates to false. Thus, the above results have been found.
*)

//Exercise 5.6
(*

In the current setup of aExp, bExp and stm, the only way for a an aExp to affect the state of the stm is through the Ass-statement. 
However, in most programming languages, operations like x++ exist as a shorthand for x = x + 1, which bypass this explicit assignment.
Thus, if an inc()-operation where to be implemented into the aExp type, one of the following solutions would be necessary to implement it:

1: the modification of state through an aExp would be possible if the evaluation performed by the A-function did not return the result of the
evaluation as an integer, but instead as a pair which could then be mapped to the state by the I-function. Thus, any return-value from the B-function would
require the form (string, aExp) where string: char | chars. This would then require the I-function to modify the state after each evaluated aExp through Map.Add()

2: the modification of state would also be possible through the passing of the state-map to the B-function, which would instead return the modified state from each expression evaluation.
This solution seems less efficient than the first one however, as this would require the B-function to manage the state in a similar way to the I-function, which would be redundant.
*)

