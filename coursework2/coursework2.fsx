(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists, recursion

  ------------------------------------
  Name: mykola ryba
  TUT Student ID:myryba
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by September 30, 2016.
*)

// 1. Make a value sl containing empty list of type string list.
let ls = List.empty<List<string>>

// 2. Make a function shuffle: int list -> int list that rearranges the elements of the argument list
// in such a way that the first value goes to first, last to second,
// second to third, last but one to fourth etc.
// E.g.
// shuffle [] -> []
// shuffle [1;2] -> [1;2]
// shuffle [1..4] -> [1;4;2;3]

let rec shuffle (list:List<int>) =
    match list with
    | [] -> []
    | head::tail -> head :: shuffle (List.rev tail)

shuffle[]
shuffle[1;2]
shuffle[1..4]
            
// 3. Make a function segments: int list -> int list list that splits the list passed
// as an argument into list of lists of nondecreasing segments.
// The segments need to be of maximal possible length (the number of segments
// needs to be minimal)
// E.g.
// segments [] ->  []
// segments [1] -> [[1]]
// segments [3;4;5;5;1;2;3] -> [[3;4;5;5];[1;2;3]]

let rec fall list n =
    match list with
    | [] -> 0
    | head::[] -> 1
    | head ::tail ->
        if head> tail.Head
        then n
        else 1 + fall tail n

let rec segments list = 
    match list with
    | [] -> []
    | l ->
        let first, last = List.splitAt (fall l 1) l
        match last with
        | [] -> [first]
        | ll -> first :: (segments last)

segments [1;2;3;4;5;6;2;1]
// 4. Make a function sumSublists : int list list -> int list that will compute the sums of sublists in a list of list of ints.
// Hint: use the function List.fold to compute the sums of lists of ints.

let rec subSum (list:List<List<int>>)=
    match list with 
    | [] -> 0
    | head :: tail -> 
        List.sum head + subSum tail
subSum [[1;4];[31;3]]

// 5. Write a function filterSegments : (int list -> bool) -> int list list -> int list list that will filter out lists of ints
// based on some filter function. Write a filter function for even sum, odd sum, even number of elements, odd number of elements.

let isEven x = (x % 2) = 0
let isOdd x = isEven x = false

let evenSum (list:List<int>) :bool =
    List.sum list |> isEven
    
let oddSum (list:List<int>) :bool = 
    evenSum list = false 
    

let evenNumbers(list:List<int>) :bool = 
    isEven list.Length
    
let oddNumbers(list:List<int>) :bool = 
    //evenNumbers list = false
    isOdd list.Length

//evenNumbers [2;2;4;5;6;4]
List.filter evenSum [[1;3];[2;2];[3;6]]

let filterSegments (op : (List<int>->bool)) (list : List<List<int>>)=
    let res = List.filter op list
    res


filterSegments evenSum [[1;3];[2;2];[3;6]]
filterSegments oddNumbers [[1;3;3];[2;2];[3;6]]



           