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

