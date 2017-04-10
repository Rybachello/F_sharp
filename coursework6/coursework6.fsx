(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 6: Property based testing

  ------------------------------------------------
  Name: mykola rybak
  Student ID:myryba
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as
  file coursework6.fsx in directory coursework6.

  The file that should be compiled to a dll should go into coursework6.fs.

  Please do not upload DLL-s. Just include a readme.txt file containing the 
  dependencies required (additional DLLs)

  The deadline for completing the above procedure is Friday, November 11, 2016.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*

*)

#r @"...\packages\FsCheck.2.6.2\lib\net45\FsCheck.dll"
open FsCheck

(*
    Task 1:

    Define FsCheck properties for the following statements:

      * Taking the lengths of two lists xs and ys and adding them yields the
        same value as concatenating xs and ys and taking the length of the
        result.

      * Reversing two lists xs and ys and concatenating the results yields the
        same value as concatenating ys and xs and reversing the result.
*)


type LenghtSpecification = 
    static member ``Check lenght of concatenating list`` (xs:list<int>) (ys:list<int>) = xs.Length + ys.Length = (xs @ ys).Length    
    static member ``Check Reversing two lists`` (xs:list<int>) (ys:list<int>) = List.rev(xs) @ List.rev(ys) = List.rev ( ys @ xs)
 
//Check.QuickAll<LenghtSpecification>()
(*
    Task 2:

    A palindrome is a list that is equal to its reverse. Below you find a
    function isPalindrome, which checks whether a given list is a palindrome.

     a) Define an FsCheck property that expresses the above definition of a
        palindrome. Use the operator ==> for defining your property.

     b) Define a variant of your property which makes FsCheck show the
        distribution of the lenghts of the lists on which the property was
        tested.
*)

let rec isPalindrome xs =
  match xs with
    | []        -> true
    | (x :: xs) -> match List.rev xs with
                     | []        -> true
                     | (y :: ys) -> x = y && isPalindrome ys

let checkPalindromeA xs =
    isPalindrome xs ==> (List.rev xs |> isPalindrome) // The definition says that a palindrome is equal to its reverse, so you should be checking that
Check.Quick checkPalindromeA
let checkPalindromeB xs =
    checkPalindromeA xs |> Prop.collect (xs.Length)
Check.Quick checkPalindromeB

(*
    Task 3:

    Below you find a function toPalindrome, which converts a given list into a
    palindrome of the same length, more or less by replacing the second half of
    the list with the reverse of the first half.

     a) Define an FsCheck property that expresses the definition of a
        palindrome from the previous task. This time, make sure that FsCheck
        does not generate arbitrary lists from which it uses only the
        palindromes, but generates palindromes directly.

     b) Define a variant of your property which makes FsCheck show the
        distribution of the lenghts of the lists on which the property was
        tested.
*)

let toPalindrome xs =
  let len       = List.length xs
  let suffixLen = len / 2
  let prefixLen = if 2 * suffixLen = len then suffixLen else suffixLen + 1
  let take n xs = Seq.toList (Seq.take n xs)
  take prefixLen xs @ List.rev (take suffixLen xs)


let listIsPalindromeA =
    Prop.forAll (Arb.from<list<int>> |> Arb.mapFilter toPalindrome isPalindrome)  (fun x -> x = List.rev x )

Check.Quick listIsPalindromeA
let listIsPalindromeB =
    Prop.forAll (Arb.from<list<int>> |> Arb.mapFilter toPalindrome isPalindrome)  (fun x -> x = List.rev x |> Prop.collect (x.Length) )
Check.Quick listIsPalindromeB
 
(*
    Task 4:

    Copy all the code into coursework6.fs file, use the appropriate [<TestFixture>],
    [<Property>] attributes so that the tests are runnable using FsCheck.NUnit (v. 2.6.2)

*)




(*  Task 5:

    Take the decision tree code from the lecture notes and write unit and property based tests
    to the extent that you consider the code reasonably well tested.

*)
