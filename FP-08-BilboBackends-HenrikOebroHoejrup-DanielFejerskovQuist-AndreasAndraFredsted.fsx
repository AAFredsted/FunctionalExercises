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
let rec countA n t = 
    match t with
    | Leaf -> n
    | Node(tl,_,tr) -> 1 + (countA n tl) + (countA n tr)

let t = Node(Node(Leaf,3,Node(Leaf,3,Leaf)),1,Node(Leaf,4,Leaf))
countA 0 t

//Exercise 8.2
(*
Declare a tail-recursive functions with the type:  countAC : BinTree<’a> -> int -> (int -> ’b) -> ’b, 
such that count t = countAC t 0 id. The intuition with countAC t a c is that a is the
number of nodes being counted so far and c is the continuation.
*)


let rec countAC t n c = 
    match t with
    | Leaf -> c n
    | Node(tl, _, tr) ->
        countAC tl n ( fun vl -> countAC tr n (fun vr -> c(vl + vr + 1)))

countAC t 0 id
//this is bad, but I have no other idea of what to do..

//Exercise 8.3
(*
        Consider the following list-generating function:
        let rec bigListK n k =
        if n=0 then k []
        else bigListK (n-1) (fun res -> 1::k(res));;
        The call bigListK 130000 id causes a stack overflow. Analyze this problem.
*)
let rec bigListK n k =
        if n=0 then k []
        else bigListK (n-1) (fun res -> 1::k(res))

//this results in a stack overflow
//bigListK 300000 id
(*
        In the recursive call to bigListK, we apply a decremented of 1, so if n starts of with 130000, we will have 130000 calls to bigListK.
        At the same time, we set k to a function that takes a list as an argument and appends 1 to the list . 
        Thus, we will build up the following structure. 1::k(1::k(1::k(1::k(...(res))))) where 1::k will be constructed 130000 times for the call.
        In the world of stacks, we would have an n element large callstack, which would result in the stack overflow.
        Whether this is the case however, depends on the max stack size in the system running the application. 


*)


//Exercise 8.4

(*
        Declare tail-recursive functions leftTree and rightTree. By use of leftTree it should
        be possible to generate a big unbalanced tree to the left containing n + 1 values in the nodes so
        that n is the value in the root, n − 1 is the value in the root of the left subtree, and so on. All
        subtree to the right are leaves. Similarly, using rightTree it should be possible to generate a
        big unbalanced tree to the right.

        1. Use these functions to show the stack limit when using count and countA from Exercise 9.8.
        2. Use these functions to test the performance of countC and countAC from Exercise 9.9.
*)

let rec leftTree n = 
        if n = 0 then Leaf
        else Node(leftTree( n - 1), n, Leaf);;

let rec rightTree n = 
        if n = 0 then Leaf
        else Node(Leaf, n, rightTree (n - 1));;

// part 1
let l  = leftTree 150000
let  r = rightTree 150000

count l //crashes at around 150000
count r //crashes at around 150000

let l2 = leftTree 150000
let r2 = rightTree 150000
countA 0 l2 //crashes at around 150000

countA 0 r2 //crashes at around 150000


//part 2

let rec countC t c =
        match t with
        | Leaf -> c 0
        | Node(tl,n,tr) -> countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)));;


//we will use the #time function in F# to check the running time for a tree of size 50000

let l3 = leftTree 100000
let r3 = rightTree 100000

#time
countC l3 id // 0.005 seconds in runtime
#time


#time
countC r3 id //0.005 seconds in runtime
#time


#time
countAC l3 0 id  //0.002 seconds in runtime
#time

#time
countAC r3 0 id //0.001 seconds in runtime
#time

//Exercise 8.5

(*
        Make a declaration for the sequence of odd numbers.
*)

let oddseq = Seq.initInfinite (fun x -> if x = 0 then 1 else x*2 + 1 )
oddseq |> Seq.item 0

//exercise 8.6

(*
        Make a declaration for the sequence of numbers 1, 1, 2, 6, . . . , n!, . . ..
*)


let rec (!) n =
    if n = 0 then 1
    else n * ! (n - 1)

let factseq = Seq.initInfinite (fun x -> !x)
factseq |> Seq.item 3

//there we go
