(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: Higher order functions, option, list

  ------------------------------------
  Name: Mykola Rybak
  Student ID:
  ------------------------------------


  Answer the questions below. You answers to the questions should be
  correct F# code written after the question. This file is an F# script
  file; it should be possible to load the whole file at once. If you
  can't, then you have introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your solution to the repository as file
  coursework4.fsx in directory coursework4.

  The deadline for completing the above procedure is Friday,
  October 21, 2016.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

*)

// 1. Write a function by pattern matching
// 
//   flattenOption : option<option<'a>> -> option<'a>
//
//   which squashes two layers of possible successes or failures into 1
//   E.g. Some Some 1 -> Some 1

let flattenOption b = 
    match b with
    | None -> None
    | Some(x) -> x

flattenOption (Some(Option<int>.None))
//val it : int option = None

// 2. Can flattenOption by implemented using bind? If so, do it!

let flattenOption2 b = b|> Option.bind (fun x -> x) 

//flattenOption2 (Some(Some(1)))
//val it : int option = Some 1

// 3. Write a function
//
//    idealist : list<option<'a>> -> list<'a>
//
//    which collects a list of possible successes or failures into a
//    list containing only the successes. Pay close attention to the type.
let idealist list = list |> List.choose (fun x -> x)
                               
//let testList = [Some(-1);Some(1);None]
//idealist testList
//val it : int list = [-1; 1]

// 4. Write a function
//
//    conservative : list<option<'a>> -> option<list<'a>>
//
//    that takes a list of possible successes or failures and returns
//    a list of successes if everything succeeded or returns failure
//    if 1 or more elements of the list was a failure. Again, pay
//    close attention to the type.

//let checkLenght (l1:list<'a>)(l2:list<option<'a>>) = if l1.Length < l2.Length  then  None else Some(l1) 
let conservative l = l |> idealist |> fun x -> if x.Length < l.Length  then  None else Some(x)

conservative [Some 1; Some 2; Some 3; Some -3];;   
//val it : int list option = Some [1; 2; 3; -3]
conservative [Some 1; None];;   
//val it : int list option = None
conservative ([ ]: int option list);;  
// val it : int list option = Some [ ]
// 5. Write a function
//
//    chars : list<string> -> list<char>
//
//    This function should use List.collect (bind) and have the
//    following behaviour:
//    ["hello";"world"] -> ['h';'e';'l';'l';'o';'w';'o';'r';'l';'d']

let getChars charlist = charlist |> List.collect (fun x -> List.ofSeq x)

getChars ["hello";"world"]
// 6. Write a function
//
//    iprint : list<int> -> string
//
//    This function should use List.foldBack and have the following behaviour:
//    [1 .. 5] |-> "1,2,3,4,5,"


let iprint l = List.foldBack( fun s1 s2 -> s1 + "," + s2) (l|> List.map (fun i -> i.ToString())) ""
        

iprint [1 .. 5] 

//let iprint l = l|> List.map (fun i -> i.ToString()) |>  List.foldBack( fun r s -> r + "," + s) 
//iprint [1..5] ""