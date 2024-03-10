

//Exercise 5.1

//Firstly, we define a BinTree datatype for us to start working on:

type 'a BinTree =
    | Leaf
    | Node of 'a * 'a BinTree * 'a BinTree

//we define a function to traverse the tree in order
//the function is based on the preOrder function from slide 32
let rec inOrder tree =
    match tree with
        Leaf -> []
        | Node(n,treeL,treeR) ->
        //as we are need to perform array appending on the elements but cannot use the cons-operator, we put n inside of a list
        inOrder treeL @ [n] @ inOrder treeR;

//to test that the function works, we define a variable of type BinTree and test the function:
let intBinTree =
    Node(43, Node(25, Node(56, Leaf, Leaf), Leaf),
        Node(562, Leaf, Node(78, Leaf, Leaf)))

inOrder intBinTree
//It works !!


//Exercise 5.2

//the mapInOrder function is based on the same recursive logic as inOrder, 
//but maps a Leaf to a Leaf and a Node to a Node instead, where the function is applied to any values stored in the newly constructed nodes.

let rec mapInOrder (f: 'a -> 'b) (tree: 'a BinTree) = 
                match tree with 
                    Leaf -> Leaf
                    | Node(n, treeL, treeR) ->
                        let L = mapInOrder f treeL
                        let N = f n
                        let R = mapInOrder f treeR
                        Node(N, L, R);;

mapInOrder (fun x -> x + 1) intBinTree;;
//and it works :)

// to evaluate how a reordering of the operations from mapInOrder to mapPostOrder could affect the resulting values of the trees, we will implement mapPostOrder:

let rec mapPostOrder (f: 'a -> 'b) (tree: 'a BinTree) = 
                    match tree with
                        Leaf -> Leaf
                        | Node(n, treeL, treeR) ->
                            let R = mapPostOrder f treeR
                            let N = f n
                            let L = mapPostOrder f treeL
                            Node(N, L, R);;

//as shown, both functions map nodes to nodes with the same ordering of their triplets,
//meaning that the structure of the tree for both functions will be the same.
// As the functions only accept static functions acting only on n, I do not see how a non-associative function can be passed to mapPostOrder and mapInOrder

//Exercise 5.3


let rec foldInOrder (f: 'a -> 'b -> 'b) (tree: 'a BinTree) (e: 'b) = 
                                                                    match tree with     
                                                                        Leaf -> e
                                                                        | Node(n, treeL, treeR) ->
                                                                            let er = foldInOrder f treeL e
                                                                                        foldInOrder f treeR (f n er);;

                                                                        
