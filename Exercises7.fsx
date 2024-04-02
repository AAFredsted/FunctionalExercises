let xs = [1;2];;

let rec g = function
    | 0 -> xs
    | n -> let ys = n::g(n-1)
    List.rev ys;;
