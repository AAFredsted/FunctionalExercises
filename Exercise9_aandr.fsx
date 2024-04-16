// Question 1

//polymorphic data type for assignment:
type Heap<'a when 'a: equality> =
    | EmptyHP
    | HP of 'a * Heap<'a> * Heap<'a>


//1.1
(*
    The following is the construction of the heap data from the given graph
*)
let ex3 = HP(1, HP(2, HP(3, EmptyHP, EmptyHP), HP(5, EmptyHP, EmptyHP)), HP(4, EmptyHP, EmptyHP));;

(*
    the type of ex3 is Heap<Int>, which is monomorphic. The datatype Heap is polymorphic, 
    but for ex3, we insert ints, making it monomorphic to that type,
    as it is a requirement that the datatypes in the Heap can be compared for equality
*)

//my approach would be to just statically type out the type, but I am not sure

let empty: Heap<'a> when 'a: equality = EmptyHP


//define an exception that takes an argument of type string

exception HeapError of string;



//1.2


let isEmpty heap =
    match heap with
    | HP(_, _, _) -> false // Non-empty heap
    | EmptyHP -> true      // Empty heap
    | _ -> raise (HeapError("the provided value was not of type Heap<'a when 'a: equality>"))




isEmpty empty;



//the folloiwng function is declared size : Heap<’a> -> int when ’a : equality
let rec size heap= 
    match heap with 
    | EmptyHP -> 0
    | HP( a, one, two) -> 1 + size one + size two
    | _ -> raise ( HeapError("the provided value was not of type Heap<'a when 'a: equality>"))



//the following function is declared: find : Heap<’a> -> ’a when ’a : equality
let find heap =
    match heap with 
    | HP(a, one, two) -> a
    | _ -> raise ( HeapError("the provided heap is either empty or not of type heap"))


let chkHeapProperty heap = 
    let rec helper heap c = 
        match heap with
        | EmptyHP ->   true
        | HP(vh, lh, rh) -> 
            helper lh (fun () -> vh < find lh ) && 
            helper rh (fun () -> vh < find rh)
    helper heap (fun () -> true)
    
// bad solution but I dont know what else I can do

chkHeapProperty ex3

