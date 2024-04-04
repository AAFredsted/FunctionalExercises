//Exercise 8.1

(*
Develop a version of the counting function for binary trees
that makes use of an accumulating parameter. Observe that this function is not tail recursive.

countA: int -> BinTree<’a> -> int
*)

//the following is a declaration of a binary tree and a tail-recursive function to count the number of nodes in the tree
type BinTree<'a> = 
        | Leaf
        | Node of BinTree<'a> * 'a * BinTree<'a>;;

let rec count = function
        | Leaf -> 0
        | Node(tl,n,tr) -> count tl + count tr + 1;;


// the following is a declaration of a function counting the nodes using an accumulator
let rec countA acc tree = 
    match tree with 
    | Leaf -> acc 
    | Node(t1, _, t2) ->  1 + countA (countA acc t1) t2;;


//as this function contains two nested recursive calls, it cannot be written as an iterative function in f#, 
//thus not being tail-recursive


//Exercise 8.2
(*
Declare a tail-recursive functions with the type:  countAC : BinTree<’a> -> int -> (int -> ’b) -> ’b, 
such that count t = countAC t 0 id. The intuition with countAC t a c is that a is the
number of nodes being counted so far and c is the continuation.
*)

//this solution combines pattern matching and a continuation to 
let rec countAC tree acc cont =
    if tree = Leaf then 
        cont acc
    else 
        match tree with
        | Node (Leaf, _, Leaf) ->  
            cont (acc + 1)
        | Node (t1, _, Leaf) ->    
            countAC t1 (acc + 1) cont
        | Node (Leaf, _, t2) ->   
            countAC t2 (acc + 1) cont
        | Node (t1, _, t2) ->    
            countAC t1 acc (fun acc' -> countAC t2 acc' cont)
//this is bad, but I have no other idea of what to do..

