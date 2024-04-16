
// Question 1.1
type Heap<'a when 'a: equality> =
    | EmptyHP
    | HP of 'a * Heap<'a> * Heap<'a>

// Example of a heap that fulfills the heap property:
let ex3 = HP(1, HP(2, HP(3, EmptyHP, EmptyHP), HP(5, EmptyHP, EmptyHP)), HP(4, EmptyHP, EmptyHP))

// Two examples of heaps that do not fulfill the heap property:
let ex4 = HP(1, HP(6, HP(3, EmptyHP, EmptyHP), HP(5, EmptyHP, EmptyHP)), HP(4, EmptyHP, EmptyHP))
let ex5 = HP(6, HP(3, EmptyHP, EmptyHP), HP(5, EmptyHP, EmptyHP))

(* The type of the value ex3 is Heap<int> which is monomorphic. This is because ex3 has to be of type Heap
which has to contain integers, since this is what is defined in the given tree. The nodes in the tree have to
contain integers as shown in Example 3 in the exercise description. *)

let empty = EmptyHP

exception HeapError of string


// Question 1.2
let isEmpty h = if h=EmptyHP then true else false

let rec size h =
    match h with
    | EmptyHP -> 0
    | HP(_, tl, tr) -> 1 + size tl + size tr

let find h = 
    match h with
    | EmptyHP -> failwith "this is an empty heap"
    | HP(n, _, _) -> n

let rec chkHeapProperty h = 
    match h with
    | EmptyHP -> true
    | HP(n, lt, rt) ->
        let isLeftValid = match lt with
                                | EmptyHP -> true
                                | HP(ln, _, _) -> n<=ln
        let isRightValid = match rt with
                                | EmptyHP -> true
                                | HP(rn, _, _) -> n<=rn
        isLeftValid && isRightValid && chkHeapProperty lt && chkHeapProperty rt

chkHeapProperty ex3 // true
chkHeapProperty ex4 // false
chkHeapProperty ex5 // false


// Question 1.3
let rec map f h =
    match h with
    | EmptyHP -> EmptyHP
    | HP(n,tl,tr) -> HP(f n,map f tl,map f tr)

(* The order in which the function f is applied to the values in the heap follows a depth-first traversal of
the heap structure. The depth-first traversal ensures that the function f is applied first to the values at
the root of the heap, then to values in the left subtree, and finally to values in the right subtree.
This traversal order ensures that the function is applied to all values in the heap, starting from the root
and progressing through each level of the heap in a left-to-right manner. *)

// Example test run:
map ((+)1) ex3 // returns the heap with all values in ex3 increased by one

(* The following function f converts all positive values to negative values, and all negative values to positive
values. This function will ensure that mapping it on all values in a heap will make the heap not fulfilling
the heap property, as child nodes will end up having a lower value than their parent nodes *)
let f x = x*(-1)

chkHeapProperty (map f ex3) // false


// Question 3.1
let triNum = Seq.initInfinite (fun i -> (i*(i+1))/2)

Seq.take 4 triNum // Example to test it. It outputs seq [0; 1; 3; 6] which is correct

let triNumC = Seq.cache triNum


// Question 3.2
let rec myFilterOddIndex s =
    Seq.delay (fun() -> Seq.append (Seq.singleton (Seq.item 0 s)) (myFilterOddIndex (Seq.skip 2 s)))

Seq.take 4 (myFilterOddIndex triNum) // Example to test it. It outputs seq [0; 3; 10; 21] which is correct


// Question 3.3
let rec seqZip s1 s2 = 
    seq {
        let e1 = Seq.item 0 s1
        let e2 = Seq.item 0 s2
        yield (e1,e2)
        yield! seqZip (Seq.tail s1) (Seq.tail s2)
        }


Seq.take 4 (seqZip triNum triNum) // Example to test it. It outputs seq [(0, 0); (1, 1); (3, 3); (6, 6)] which is correct