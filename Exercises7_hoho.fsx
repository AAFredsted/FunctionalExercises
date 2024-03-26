
// Exercise 7.1
let xs = [1;2]

let rec g = function
    0 -> xs
  | n -> let ys = n::g(n-1)
         List.rev ys

g 2

// don't know anymooore. Let me out of here


// Exercise 7.2
// My iterative solution is declared in if-then-else form
let rec sum(m,n) =
    if n=0 then m else sum(m,n-1) + m + n

sum(3,4) // int=25
sum(13,7) // int=132


// Exercise 7.3
// If the xs list given as input is empty, then return 0. If the list is not empty, then call the count-function.
// The count-function returns acc if the xs list is empty, otherwise it recursively passes the xs list,
// apart from from the first element in the list, which is called tail, to the count-function, and increments
// acc with 1.
let length xs =
    let rec count acc xs =
        if List.isEmpty xs then acc
        else let _::tail = xs in count (acc+1) tail
    if List.isEmpty xs then 0
    else count 0 xs

length [1; 4; 5; 5; 7] //returns int = 5
length [1; 4; 5] //returns int = 3


// Exercise 7.4
let xs16 = List.init 1000000 (fun _ -> 16)

#time
let rec facC n c =
    if n=0 then c 1
    else facC (n-1) (fun res -> c(n*res))

for i in xs16 do let _ = facC i id in ()
#time

(* This continuation-based solution takes between 0.5 and 1.18 seconds to execute, while the 
   solution in section 9.4 takes 0.024 seconds. I am using a for-loop as they do in the book to
   get a fair comparison of the running time between these two different solutions *)



// Exercise 7.5
#time
let fib n = 
    if n=0 then 0
    else if n=1 then 1
    else
        let mutable p = 0
        let mutable res = 1
        let mutable i = 2
        while i<=n do
            let res' = res
            res <- p + res
            p <- res'
            i <- i + 1
        res

fib 4 // outputs 3 as it should
#time


// Exercise 7.6
#time
let rec fibA n n1 n2 = 
    match n with
    | 0 -> n1
    | 1 -> n2
    | _ -> fibA (n-1) n2 (n1+n2)

fibA 10 0 1 // outputs 55 as it should
#time
// This solution does not even show any milliseconds for it to be executed, since it is fast
(* This works because the recursive calls will look like this:
9 1 1
8 1 2
7 2 3
6 3 5
5 5 8
4 8 13
3 13 21
2 21 34
1 34 55 *)


#time
let rec fibC n c : int =
    if n=0 then c 0
    else if n=1 then c 1
    else fibC (n-1) (fun res1 -> fibC (n-2) (fun res2 -> c(res1+res2)))

fibC 10 id
#time
(* This solution takes between 1-3 milliseconds to be executed, so this continuation-based solution is
a bit slower than the one above with the two accumulating parameters.
Compared to the while-loop based solution further above, which takes between 0 and 2 milliseconds, 
this continuation-based solution is still a bit slower, but is almost as fast as the while-loop
based solution *)