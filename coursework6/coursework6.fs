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
module coursework6

(*
    Task 4:

    Copy all the code into coursework6.fs file, use the appropriate [<TestFixture>],
    [<Property>] attributes so that the tests are runnable using FsCheck.NUnit (v. 2.6.2)

*)

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

 let rec isPalindrome xs =
  match xs with
    | []        -> true
    | (x :: xs) -> match List.rev xs with
                     | []        -> true
                     | (y :: ys) -> x = y && isPalindrome ys

let toPalindrome xs =
  let len       = List.length xs
  let suffixLen = len / 2
  let prefixLen = if 2 * suffixLen = len then suffixLen else suffixLen + 1
  let take n xs = Seq.toList (Seq.take n xs)
  take prefixLen xs @ List.rev (take suffixLen xs)

 [<TestFixture>]
    type ``coursework6 tests `` () =
       [<Property>]
       member this. `` Task 1.a Check lenght of concatenating list`` (xs:list<int>) (ys:list<int>) = xs.Length + ys.Length = (xs @ ys).Length  
       [<Property>]
       member this. `` Task 1.b Check Reversing two lists`` (xs:list<int>) (ys:list<int>) = List.rev(xs) @ List.rev(ys) = List.rev ( ys @ xs)
       [<Property>]
       member this. `` Task 2.a checkPalindromeA `` xs = isPalindrome xs ==> (List.rev xs |> isPalindrome)
       [<Property>]
       member this. `` Task 2.b checkPalindromeB `` xs = isPalindrome xs ==> (List.rev xs |> isPalindrome) |> Prop.collect (xs.Length)
       [<Property>]
       member this. `` Task 3.a listIsPalindromeA ``() = Prop.forAll (Arb.from<list<int>> |> Arb.mapFilter toPalindrome isPalindrome)  (fun x -> x = List.rev x )
       [<Property>]
       member this. `` Task 3.b listIsPalindromeB ``() = Prop.forAll (Arb.from<list<int>> |> Arb.mapFilter toPalindrome isPalindrome)  (fun x -> x = List.rev x |> Prop.collect (x.Length))



(*  Task 5:

    Take the decision tree code from the lecture notes and write unit and property based tests
    to the extent that you consider the code reasonably well tested.

*)

 type Client = 
      { 
        Name : string;
        Income : int ;
        YearsInJob : int
        UsesCreditCard : bool;
        CriminalRecord : bool 
       }

    type QueryInfo =
      { 
        Title     : string
        Check     : Client -> bool
        Positive  : Decision
        Negative  : Decision 
      }

    and Decision = 
       | Result of string
       | Query  of QueryInfo

    let rec tree =
       Query  {Title = "More than €40k"
               Check = (fun cl -> cl.Income > 40000)
               Positive = moreThan40
               Negative = lessThan40}
    and moreThan40 =
       Query  {Title = "Has criminal record"
               Check = (fun cl -> cl.CriminalRecord)
               Positive = Result "NO"
               Negative = Result "YES"}
    and lessThan40 =
       Query  {Title = "Years in job"
               Check = (fun cl -> cl.YearsInJob > 1)
               Positive = Result "YES"
               Negative = usesCreditCard}
    and usesCreditCard =
       Query  {Title = "Uses credit card"
               Check = (fun cl -> cl.UsesCreditCard)
               Positive = Result "YES"
               Negative = Result "NO"}

 let rec testClientTree client tree =
        match tree with
        | Result msg  -> printfn " OFFER A LOAN: %s" msg
                         msg
        | Query qinfo -> let result, case = 
                             if qinfo.Check(client) then
                                 "yes", qinfo.Positive
                             else
                                 "no", qinfo.Negative
                         printfn " - %s ? %s" qinfo.Title result
                         testClientTree client case       

//define a client for testing
let max = {Name = "maa"; Income = 20000 ; YearsInJob = 0 ; 
                UsesCreditCard = false ; CriminalRecord = false }

[<TestFixture>]
type ``testing client tree`` () =                                            
       [<Test>]
       member this.
            ``Given a client with income more than 40000 and criminal record true `` () =
                let kiril = {Name = "Kiril van Doe"; Income = 80000 ; YearsInJob = 1 ; 
                UsesCreditCard = false ; CriminalRecord = true }
                Assert.AreEqual (testClientTree kiril tree, "NO")
       [<Test>]
       member this.
            ``Given a client with income more than 40000 and criminal record false `` () =
                let peter = {Name = "Peter Doe"; Income = 50000 ; YearsInJob = 1 ; 
                UsesCreditCard = false ; CriminalRecord = false }
                Assert.AreEqual (testClientTree peter tree, "YES")

       [<Test>]
       member this.
            ``Given a client with income less than 40000 and 2 years in job `` () =
                let anna = {Name = "Anna Domkina"; Income = 20000 ; YearsInJob = 2 ; 
                UsesCreditCard = false ; CriminalRecord = false }
                Assert.AreEqual (testClientTree anna tree, "YES")
       [<Test>]
       member this.
            ``Given a client with income less than 40000 and without years in job, usesCreditCard = true `` () =
                let mykola = {Name = "Mykola Kozak"; Income = 20000 ; YearsInJob = 0 ; 
                UsesCreditCard = true ; CriminalRecord = false }
                Assert.AreEqual (testClientTree mykola tree, "YES")
       [<Test>]
       member this.
            ``Given a client with income less than 40000 and without years in job, usesCreditCard = false `` () =
                let max = {Name = "Max Koez"; Income = 20000 ; YearsInJob = 0 ; 
                UsesCreditCard = false ; CriminalRecord = false }
                Assert.AreEqual (testClientTree max tree, "NO")

       [<Property>]
       member this.
            ``Check client valid name`` name =
                let result = testClientTree {max with Name = name} tree 
                Assert.IsTrue(result = "NO" || result = "YES" )
       [<Property>]
       member this.
            ``Check client valid income`` income =
                let result = testClientTree {max with Income = income} tree 
                Assert.IsTrue(result = "NO" || result = "YES" )
       [<Property>]
       member this.
            ``Check client valid yearsInJob`` yearsInJob =
                let result = testClientTree {max with YearsInJob = yearsInJob} tree 
                Assert.IsTrue(result = "NO" || result = "YES" )      
       [<Property>]
       member this.
            ``Check client valid usesCreditCard`` usesCreditCard =
                let result = testClientTree {max with UsesCreditCard = usesCreditCard} tree 
                Assert.IsTrue(result = "NO" || result = "YES" )     
                
       [<Property>]
       member this.
            ``Check client valid CriminalRecord`` criminalRecord =
               let result = testClientTree {max with CriminalRecord = criminalRecord} tree 
               Assert.IsTrue(result = "NO" || result = "YES" )    