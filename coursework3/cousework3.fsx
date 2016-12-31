(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 3: User defined types

  ------------------------------------
  Name: mykola rybak
  TUT Student ID:
  ------------------------------------


  Answer the questions below.  You answers to questions 1--7 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by October 9, 2015.
*)

// 1. Consider expression trees of type ExprTree declared in the lecture.
// Extend the type with if-then-else expression of the form:
// if b then e1 else e2
// where b is a boolean expression and e1 and e2 are expressions.
// An example expression could be:

type ExprTree = | Const  of int
                | Ident of string
                | Minus  of ExprTree
                | Sum    of ExprTree * ExprTree
                | Diff   of ExprTree * ExprTree
                | Prod   of ExprTree * ExprTree
                | Let    of string * ExprTree * ExprTree
                | If     of BoolTree * ExprTree * ExprTree
                //bonus 8
                | Match  of ExprTree * List<(ExprTree * ExprTree)>

and BoolTree = | Bool of bool
               | MoreThan of ExprTree * ExprTree
               | LessThan of ExprTree * ExprTree
               | And of BoolTree * BoolTree

(*
  FEEDBACK:

    You are allowing arbitrary expressions as patterns, although there are
    expressions that are not patterns (for example, 3 * x).
*)

// 2. Extend the function eval defined in the lecture to support the
// if-then-else expressions defined in Q1.
let rec checkBool tree env = 
    match tree with
    | Bool (a)-> a
    | MoreThan (e1,e2) -> eval e1 env >  eval e2 env
    | LessThan (e1,e2) -> eval e1 env <  eval e2 env
    | And(b1,b2) -> checkBool b1 env && checkBool b2 env

and  eval t env =
    match t with
    | Const n        -> n
    | Ident s        -> Map.find s env
    | Minus t        -> - (eval t env)
    | Sum (t1,t2)    -> eval t1 env + eval t2 env
    | Diff (t1,t2)   -> eval t1 env - eval t2 env
    | Prod (t1,t2)   -> eval t1 env * eval t2 env
    | Let (s,t1,t2)  -> let v1 = eval t1 env
                        let env1 = Map.add s v1 env
                        eval t2 env1
    | If(b,ex1,ex2)   -> 
                          if checkBool b env then eval ex1 env else eval ex2 env
    //bonus 9
    | Match(p,list) ->  
                        let pEval = eval p env
                        let res =List.find (fun (x,y) -> eval x env = pEval) list
                        eval (snd res) env
                        
(*
  FEEDBACK:

    If the pattern is a variable, the tested expression is considered matching
    if the result of the tested expression is the same as the current value
    of the variable. However, the tested expression should always be considered
    matching and its result should be assigned to the variable.
*)

//if a+3 > b+c && a>0 then c+d else e
let env : Map<string,int> = Map.ofList ["a",20;"b",3;"c",5;"d",4;"e",100]
let exp = If(MoreThan(Sum(Ident("a"),Const(3)),Sum(Ident("b"),Ident("c"))),Sum(Ident("c"),Ident("d")),Ident("e"))

eval exp env

// 3-4: Given the type definition:
type BList =
  | BEmpty
  | Snoc of BList * int
//
// 3. Make the function filterB: (prop: int -> bool) BList -> BList that will return a list for the elements of which
// the function prop returns true.

let rec filterB (prop: int -> bool) (list : BList) = 
    match list with
    | BEmpty -> BEmpty
    | Snoc (h, t) ->         
        if prop t  then
            Snoc(filterB prop h,t)            
        else
            filterB prop h      

(*
  FEEDBACK:

    OK
*)

let filterBExample = filterB (fun x -> 1<x) (Snoc(Snoc(Snoc(Snoc(BEmpty, 4), -6), 2), 1))
//Snoc (Snoc (BEmpty,4),2)    

// 4. Make the function mapB: (trans: int -> int) BList -> BList that will return a list where the function trans has
// been applied to each element.

let rec mabB (trans: int -> int) (list : BList) =    
    match list with
    | BEmpty -> BEmpty
    | Snoc (h, t) -> 
         Snoc (mabB trans h,
                        trans t)

(*
  FEEDBACK:

    OK
*)

let mapBExample = mabB (fun x -> x+10) (Snoc(Snoc(Snoc(Snoc(BEmpty, 4), -6), 2), 1))
//val mapBExample : BList = Snoc (Snoc (Snoc (Snoc (BEmpty,14),4),12),11)   

    
// 5-7. Given the type definition
type Tree =
  | Nil
  | Branch2 of Tree * int * Tree
  | Branch3 of Tree * int * Tree * int * Tree
//
// 5. Define the value exampleTree : Tree that represents the following
//    tree:
//
//        2
//       / \
//      *  3 5
//        / | \
//       *  *  *

let exampleTree = Branch2(Nil,2,Branch3(Nil,3,Nil,5,Nil))

(*
  FEEDBACK:

    OK
*)

// 6. Define a function sumTree : Tree -> int that computes the sum of
//    all labels in the given tree.

let rec sumTree tree = 
    match tree with
    | Nil -> 0
    | Branch2(ht,i,tt) -> 
        i + sumTree ht + sumTree tt
    | Branch3(ht,i,mt,k,tt) -> i+k + sumTree ht + sumTree tt + sumTree mt

(*
  FEEDBACK:

    OK
*)

let sumTreeResult = sumTree exampleTree
//val sumTreeResult : int = 10

// 7. Define a function productTree : Tree -> int that computes the
//    product of all labels in the given tree. If this function
//    encounters a label 0, it shall not look at any further labels, but
//    return 0 right away.
let rec productTree tree = 
    match tree with
    | Nil -> 1
    | Branch2(ht,i,tt) -> 
        match i with
        | 0 -> 0
        | _ -> i * productTree ht * productTree tt
    | Branch3(ht,i,mt,k,tt) -> 
        match i,k with
        | _,0 -> 0
        | 0,_ -> 0
        | _,_ -> i * k * productTree ht * productTree tt * productTree mt

(*
  FEEDBACK:

    While your implementation does not look at the subtrees if it encounters a
    label 0, it still looks at all other remaining parts of the tree, which it
    should not do.
*)

let productTreeResult = productTree exampleTree
//val productTreeResult : int = 30

// ** Bonus questions **

// 8. Extend the ExprTree type with a pattern match expression
// match p with [p1, ex1; p2,ex2 ...]

// 9. Extend the eval function to support match expressions.
