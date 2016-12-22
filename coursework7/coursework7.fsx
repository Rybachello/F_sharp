(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 7: Tail recursion and laziness

  ------------------------------------------------
  Name:mykola rybak
  Student ID:myryba
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework7.fsx in directory coursework7.

  Please do not upload DLL-s. Just include a readme.txt file containing the
  dependencies required (additional DLLs)

  The deadline for completing the above procedure is Friday, November 18, 2016.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Write a function maxInList : int list -> int that returns the maximum element
  in the given list. Make sure your implementation uses tail recursion.
*)

let maxInList xs =
  let rec worker acc xs =
    match xs with
      | []      -> acc
      | x :: xl -> if acc > x then worker acc xl else worker x xl
  worker (List.head xs) xs // It is bad style to access .head without knowing that the list is non-empty


maxInList [1 .. 1000000]

(*
  Task 2:

  Write a function reverse :: 'a list -> 'a list that works like the function
  List.rev. Make sure your implementation uses tail recursion.
*)
let reverse xs =
    let rec worker acc xs =
        match xs with
        | [] -> acc
        | x :: xl -> worker ([x] @ acc) xl // Why not x::acc?
    worker [] xs 

(*
  Task 3:

  Below you find the definition of a type Tree of leaf-labeled trees. Write a
  function maxInTree : int Tree -> int that returns the maximum label in the
  given tree. Use continuation-passing style in your implementation.
*)

type 'a Tree =
  | Leaf   of 'a
  | Branch of 'a Tree * 'a Tree

let maxInTree (tree: int Tree) =
  let rec worker tree cont =
    match tree with
      | Leaf x               -> cont x
      | Branch (left, right) -> worker left  (fun leftSize  ->
                                worker right (fun rightSize ->
                                if leftSize > rightSize then cont leftSize else cont rightSize))
  worker tree (fun x -> x)

(*
  Task 4:

  Write a function maxInTree' : int Tree -> int that returns the maximum label
  in the given tree, like the function maxInTree from Task 3 does. Use
  continuation-passing style in combination with accumulation in your
  implementation.
*)

let rec fistElementInTree (tree: int Tree) = 
    match tree with
    | Leaf x -> x
    | Branch(l,_) -> fistElementInTree l

let maxInTree' (tree: int Tree) = 
    let rec worker acc tree cont =
        match tree with
        | Leaf x               -> if x > acc then cont x else cont acc
        | Branch (left, right) -> worker acc left  (fun acc ->
                                worker acc right cont)
    worker (fistElementInTree tree) tree (fun x -> x)
(*
  Task 5:

  The function streamMap : ('a -> 'b) -> 'a Stream -> 'b Stream from the lecture
  is the stream analog of the function List.map. Write a function streamFilter :
  ('a -> bool) -> 'a Stream -> 'a Stream that is the stream analog of the
  function List.filter.
*)
type 'a Stream =
  | Stream of 'a * Lazy<'a Stream>

let rec streamFilter f xs =
    match xs with
    | Stream(x, lxs) -> if f x then Stream (x, lazy streamFilter f lxs.Value) else streamFilter f lxs.Value
