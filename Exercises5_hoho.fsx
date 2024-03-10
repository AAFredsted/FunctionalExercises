// Exercise 5.1
type 'a BinTree =
    | Leaf
    | Node of 'a * 'a BinTree * 'a BinTree

let rec inOrder = function
    | Leaf -> []
    | Node(x,tl,tr) -> (inOrder tl) @ [x] @ (inOrder tr)

let intBinTree =
    Node(43, Node(25, Node(56,Leaf, Leaf), Leaf),
        Node(562, Leaf, Node(78, Leaf, Leaf)))

inOrder intBinTree


// Exercise 5.2
let rec mapInOrder f = function
    | Leaf -> Leaf
    | Node(x,tl,tr) -> Node(f x,mapInOrder f tl,mapInOrder f tr)

mapInOrder (fun x -> x+1) intBinTree
(* Post-order works differently than In-order, since post-order traverses the left sub-tree first,
then traverses the right sub-tree, and then the root node in the end. In-order instead traverses
the left sub-tree first, then the root node, and then the right sub-tree in the end. But the
returned tree will be the same in the end using both ways *)


// Exercise 5.3
let foldInOrder f e b = List.fold f e (inOrder b)

let floatBinTree = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf),
                                        Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))

foldInOrder (fun n a -> a + n) 0.0 floatBinTree


// Exercise 5.4
type aExp =                     (* Arithmetical expressions *)
    | N of int                  (* numbers *)
    | V of string               (* variables *)
    | Add of aExp * aExp        (* addition *)
    | Mul of aExp * aExp        (* multiplication *)
    | Sub of aExp * aExp        (* subtraction *)

type bExp =                     (* Boolean expressions *)
    | TT                        (* true *)
    | FF                        (* false *)
    | Eq of aExp * aExp         (* equality *)
    | Lt of aExp * aExp         (* less than *)
    | Neg of bExp               (* negation *)
    | Con of bExp * bExp 

type stm =                      // statements
    | Ass of string * aExp      // assignment
    | Skip
    | Seq of stm * stm          // sequential composition
    | ITE of bExp * stm * stm   // if-then-else
    | While of bExp * stm       // while
    // | IT of ...                 // if-then
    // | RU of ...                 //  repeat until


let update x v s = Map.add x v s

let rec A a s =
    match a with
        | N n -> n
        | V x -> Map.find x s
        | Add(a1, a2) -> A a1 s + A a2 s
        | Mul(a1, a2) -> A a1 s * A a2 s
        | Sub(a1, a2) -> A a1 s - A a2 s


let rec B b s =
    match b with
    | TT -> true
    | FF -> false
    | Eq(a1,a2) -> A a1 s = A a2 s
    | Lt(a1,a2) -> A a1 s < A a2 s
    | Neg b -> not (B b s)
    | Con(b1,b2) -> (B b1 s) && (B b2 s)


let rec I stm s =
    match stm with
    | Ass(x,a) -> update x (A a s) s
    | Skip -> s
    | Seq(stm1, stm2) -> I stm2 (I stm1 s)
    | ITE(b,stm1,stm2) -> if (B b s) then (I stm1 s) else (I stm2 s)
    | While(b, stm) -> 
    // | IT...
    // | RU...