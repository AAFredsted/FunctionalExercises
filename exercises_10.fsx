
(* Assignment 2.1 *)
(*
    Declare a function genRandoms n of type int -> int[] that returns an array of n random
    integers. The random integers are larger than or equal to 1 and less than 10000. For instance,
    genRandoms 4 may return [|8803;8686;2936;2521|].
    Hint: You can use below to define a generator random of type unit -> int to generate the
    random numbers
*)

let getRandoms n = 
    let rnd = System.Random()
    let rec helperRand m acc  =
        match m with 
        | 0 -> acc
        | _ -> 
            helperRand (m-1) (acc@[rnd.Next(1, 10000)])
    Array.ofList (helperRand n [])

let random =
    let rnd = System.Random()
    fun () -> rnd.Next(1,10000)
(*
    Declare a function genRandomsP n of type int -> int[] that is similar to genRandom
    except that the numbers are generated in parallel to speed up the process.
    Hint: You may use the Array.Parallel library as explained in Section 13.6 in the book HR.
*)
let getrandoms2 n =
    Array.map random (Array.init n (fun _ -> ()))

let getRandomsP n = 
    Array.Parallel.map random (Array.Parallel.init n (fun _ -> ()))


(*
    Question 2.2
*)
(*
    Mergesort consists of three separate steps: splitting the remaining unsorted elements in two halves
    (split), identifying when the list has at most one element, and thus is trivially sorted (indivisible)
    and merging two already sorted lists together (merge). We implement each step below.
*)


(*
    Declare a function split xs of type ’a list -> ’a list * ’a list which takes a list xs, say
    [e1, . . . , en] and returns two lists with half elements in each: ([e1, . . . , en/2], [en/2+1, . . . , en]).
    For instance split [22;746;931;975;200] returns ([22;746],[931;975;200]).
    Define and explain at least three relevant test cases.
*)

let split xs = 
    let rec appendHelper n xs acc =
        match xs with
        | [] -> acc
        | x::xz -> 
            match acc with 
            | (first, second) ->
                if n = 0 then appendHelper 0 xz (first, second@[x])
                else appendHelper (n-1) xz (first@[x], second)

    appendHelper ((List.length xs)/2) xs ([], [])   

let henrik = [1;2;3;4;5;6]
split henrik;;


(*
    Declare a function indivisible xs of type ’a list -> bool. The function returns true if
    the list is either empty or contains one element only, i.e. the list is trivially sorted; otherwise the
    function returns false. For instance indivisible [23;34;45] returns false.
*)

let andres = []
let indivisible xs =
    let toCompare = List.length xs
    toCompare = 1 || toCompare = 0

indivisible henrik
indivisible andres

(*
    Declare a function merge xs ys of type
    merge : ’a list * ’a list -> ’a list when ’a : comparison
    that returns the sorted merged list of xs and ys. The function merge can assume the two lists are
    sorted and does not have to check for that. For instance merge ([1;3;4;5],[1;2;7;9])
    returns [1;1;2;3;4;5;7;9]. Define and explain at least three relevant test cases
*)

let rec merge (xs, ys) =
    let rec helpMatch (xs, ys) acc =
        match xs, ys with 
        | [], [] -> acc
        | [], y::yi -> helpMatch ([], yi) (y::acc)
        | x::xi, [] -> helpMatch (xi, []) (x::acc)
        | x::xi, y::yi ->
            if x <= y then helpMatch (xi, ys) (x::acc)
            else helpMatch (xs, yi) (y::acc)
    List.rev (helpMatch (xs, ys) [])



let lisandro =  ([1;3;4;5],[1;2;7;9])
merge lisandro

(* Test cases *)

let first = ([], [1; 2; 3; 4; 5; 9])

let second = ([8; 9; 10; 100], [1; 5; 6; 10; 33] )

let third = ([-1; 10; 100], [-100; -10; 2; 5])

let fourth = ([], [])
(*testcase empty*)
merge first
(*testcase intertwining*)
merge second
(*testcase negative*)
merge third
(*testcase both empty*)
//find solution for this one
merge fourth   
    
let divideAndConquer split merge indivisible (p: 'a list) =
    let rec dc p =
        if indivisible p then p
        else 
            let s = split p
            match s with
            xs, ys -> merge ((dc xs), (dc ys))
    dc p

divideAndConquer split merge indivisible [22;746;931;975;200]