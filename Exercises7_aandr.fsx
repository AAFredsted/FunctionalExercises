

// Exericse 7.2
(*
    Show that the gcd function on Page 16 is iterative.
*)

//the following is the gcd function presented on page 16

let rec gcd = function
    | (0,n) -> n
    | (m,n) -> gcd(n % m,m);;

(*
    This function can be defined as tail-recursive, as the recursive call is the final case to be matched by the function.
    As tail-recursive functions do not rely on the call stack to store intermediate results, the function can be considered iterative,
    and be directly translated into a loop.
    A version of the function is presented that more clearly visualizes how tail recursion and iteration are similar.
*)
let gcd_loop m n =
    let rec loop m n =
        match m with
        | 0 -> n
        | _ -> loop (n % m) m
    loop m n;;

//With this, it becomes more clear why the following is considered an iterative function in F#

let rec gcd_itr m n =
    if m = 0 then n
    else gcd_itr (n % m) m ;;

// Exercise 7.3

(*
    Declare an iterative solution to exercise 1.6.
    Exercise 1.6: sum(m, n) = m + (m + 1) + (m + 2) + · · · + (m + (n − 1)) + (m + n)
 *)

 //The recursive function solving exercise 1.6 is:
 let rec sum m n = 
    match n with
    | 0 -> m
    | _ -> sum (m + n) (n - 1);;


//The iterative solution to the problem is as follows:
let sum_itr m n = if n = 0 then m else sum_itr (m + n) (n - 1);;

//another way to visualize the iterative nature of tail-recursion is through a loop
let sum_loop m n = 
    let mutable s = 0;
    let mutable n = n;
    while n > 0 do 
        s <- s + m + n;
        n <- n - 1;
    s;;

//It feels very weird to write loops in F# XD


//Exercise 7.3

(*
    Give iterative declarations of the list function List.length.
*)
List.length [1; 4; 5; 5; 7] //returns int = 5
//this function recursively counts the number of elements in a list by having the recursive call within the context of the else-clause

let length xs =
    let rec count acc xs =
        if List.isEmpty xs then acc
        else let _::tail = xs in count (acc+1) tail
    if List.isEmpty xs then 0
    else count 0 xs

length [1; 4; 5; 5; 7] //returns int = 5

// Exercise 7.4
(*
    Declare a continuation-based version of the factorial function and compare the run time with
    the results in Section 9.4.

    The factorial function is defined as follows:
*)

let rec factA = function
    | (0,m) -> m
    | (n,m) -> factA(n-1,n*m);;

//A continuation-based version of the factorial function is as follows:

let rec facC n c =
    if n=0 then c 1
    else facC (n-1) (fun res -> c(n*res))


// to get an estimate of the time, the following is run:

let xs16 = List.init 1000000 (fun _ -> 16)
#time
for i in xs16 do let _ = facC i id in ()
#time

// the continuation-based solution took 0.510 seconds to execute on this machine, while the solution in section 9.4 took 0.024 seconds.
// The continuation-based solution is significantly slower than the solution in section 9.4.
// this may be due to the overhead of creating and passing around functions compared to the simpler recursive function.

//7.5

(*
    Declare a function for computing Fibonacci numbers Fn (see Exercise 1.5) using a while
    loop. Hint: introduce variables to contain the two previously computed Fibonacci numbers.
*)

let fib_while fn = 
    let mutable it = fn
    let mutable last = 0
    let mutable next = 1
    while fn > 0 do 
        let temp = last + next
        last <- next
        next <- temp
        it <- it - 1
    next


//exercise 7.6
(*
    Develop the following three versions of functions computing Fibonacci numbers Fn (see Exer-cise 1.5):
    1. A version fibA: int -> int -> int -> int with two accumulating parameters n1 and
    n2 , where fibA n n1 n2 = Fn , when n1 = Fn−1 and n2 = Fn−2 . 
    Hint: consider suitable definitions of F−1 and F−2 .
    
    2. A continuation-based version fibC: int -> (int -> int) -> int that is based on the
    definition of Fn given in Exercise 1.5.
    
    Compare these two functions using the directive #time, and compare this with the while-loop
    based solution of Exercise 8.6.
*)