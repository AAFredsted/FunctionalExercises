

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

let rec mapInOrder (f: 'a -> 'b) (tree: 'a BinTree) = 
                match tree with 
                    Leaf -> Leaf
                    | Node(n, treeL, treeR) ->
                        let L = mapInOrder f treeL
                        let N = f n
                        let R = mapInOrder f treeR
                        Node(N, L, R);;

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
let rec foldInOrder f e  tree = 
                                            match tree with     
                                                Leaf -> e
                                                | Node(n, treeL, treeR) ->
                                                    let er = foldInOrder f e treeL in
                                                                foldInOrder f (f er n) treeR;;

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



let update x  v s = Map.add x v s;;


let rec Aeval (exp: aExp) (state: Map<string, int>) = 
                            match exp with 
                                | N n -> n
                                | V v -> 
                                        try
                                            Map.find v state
                                        with
                                            | :? System.AccessViolationException as ex -> 
                                                printfn "value not defined in scope. Error message: %s" ex.Message
                                            | ex ->
                                                printfn "An error occured: %s" ex.Message
                                | Add(exp1, exp2) -> Aeval exp1 + Aeval exp2
                                | Mul(exp1, exp2) -> Aeval exp1 * Aeval exp2
                                | Sub(exp1, exp2) -> Aeval exp1 - Aeval exp2
                                        
