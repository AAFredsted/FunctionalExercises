
// Exercise 8.1

type BinTree<'a> =
    | Leaf
    | Node of BinTree<'a> * 'a * BinTree<'a>

let rec countA n t = 
    match t with
    | Leaf -> n
    | Node(tl,_,tr) -> 1 + (countA n tl) + (countA n tr)

let t = Node(Node(Leaf,3,Node(Leaf,3,Leaf)),1,Node(Leaf,4,Leaf))
countA 0 t


// Exercise 8.2
let rec countAC t n c = 
    match t with
    | Leaf -> c n
    | Node(tl,_,tr) ->
        countAC tl n (fun vl -> countAC tr n (fun vr -> c(vl+vr+1)))

countAC t 0 id


// Exercise 8.3
let rec bigListK n k =
    if n=0 then k []
    else bigListK (n-1) (fun res -> 1::k(res))

bigListK 130000 id // this gives me a stack overflow

(* The reason for the stack overflow is that the stack keeps all recursive function calls
   and the values of n in the stack. Since n is decreased by 1 in each recursive function call,
   it stores 130000 different values of n in the stack as well as 130000 recursive function calls
   to the bigListK function. The stack gets exceedingly large when n=130000 which is the reason
   for the stack overflow *)


// Exercise 8.4 (not finished)
// let rec leftTreeC n c = 
// let leftTree n = 


// Exercise 8.5
let oddNumbers = Seq.filter (fun i -> i % 2 = 1) (Seq.initInfinite (fun i -> i))
// This sequence is defined using Seq.filter which takes a predicate (a function returning a boolean)
// and a sequence as arguments. It filters all numbers where i%2=1 returns true from the infinite
// sequence of all natural numbers

Seq.item 3 oddNumbers // outputs int=7 as expected


// Exercise 8.6
let rec fact n = if n=0 then 1 else fact (n-1)*n
let fac = Seq.map fact (Seq.initInfinite (fun i -> i))

Seq.item 4 fac // outputs int=24 as expected