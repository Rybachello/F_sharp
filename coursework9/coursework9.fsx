(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------------------------------------

  Coursework 9: Asynchronous and reactive programming

  ------------------------------------------------------------------------------
  Name:mykola rybak
  Student ID: myryba
  ------------------------------------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework9.fsx in directory coursework9.

  Please do not upload DLL-s. Just include a readme.txt file containing the
  dependencies required (additional DLLs)

  The deadline for completing the above procedure is Friday, December 9, 2016.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)
open System.IO
open System.Net
(*
  Task 1:

  Write a function downloadParallel : string list -> Async<string []> that takes
  a list of URLs and downloads the resources referenced by these URLs in
  parallel. Use the function downloadAsync from the lecture in your
  implementation.
*)
let readToEndAsync (reader : StreamReader) =
  Async.AwaitTask (reader.ReadToEndAsync())

let downloadAsync (url : string) =
  async {
    let  request  = HttpWebRequest.Create(url)
    use! response = request.AsyncGetResponse()
    let  stream   = response.GetResponseStream()
    use  reader   = new StreamReader(stream)
    return! readToEndAsync reader
  }

let dowloadParallel (urlList:string list) = 
        Async.Parallel <| List.map (fun url -> downloadAsync url) urlList

(*
  Task 2:

  Write a function downloadSemiParallel : string list -> Async<string []> that
  takes a list of URLs and downloads the resources referenced by these URLs.
  Resources from URLs with the same domain name shall be downloaded
  sequentially, but otherwise, parallelism shall be used. The order of the
  resources in the resulting array can be chosen by you.
*)


let exp = ["http://www.fssnip.net/22/title/URL-Canonicalization";"http://stackoverflow.com/questions/7942630/splitting-a-list-of-items-into-two-lists-of-odd-and-even-indexed-items";"https://www.dotnetperls.com/sort-fs";"http://www.fssnip.net/22";"https://www.dotnetperls.com/fs";"http://stackoverflow.com/questions/7942630"]

//get domain from url
let getDomain (url:string) =
    System.Uri(url).Host
 
//sorting by domains
let sortByDomains urlList = 
    urlList|> Seq.groupBy (fun url -> getDomain url)

//seq string -> Async<string[]>
let rec downloadSequentially urlList = 
      async
                  {
                    if not (Seq.isEmpty urlList) then 
                        let! first = downloadAsync (Seq.head urlList) 
                        let! rest = downloadSequentially (Seq.tail urlList)                   
                        return first :: rest 
                    else
                        return []
                  }

let rec downloadSemiParallel (urlList:string list) =
    async {    
        let sortedUrl = sortByDomains urlList
        let! res = sortedUrl |> Seq.map (fun (domain, urlList) -> downloadSequentially urlList) |> Seq.toList |> Async.Parallel             
        return res |> List.concat
    }
(*
  Task 3:

  Write an event stream additions : IObservable<string> that emits an event
  everytime a file is created in the current directory. Each such event shall
  carry the name of the created file.

  Furthermore, write an event stream removals : IObservable<string> that emits
  an event everytime a file is removed from the current directory. Each such
  event shall carry the name of the removed file.
*)

open System.IO

let watcher = new FileSystemWatcher(__SOURCE_DIRECTORY__)
watcher.EnableRaisingEvents <- true


let additions = watcher.Created |> Observable.map(fun eventArgs ->
    sprintf "%s\n is created \n" eventArgs.Name)

let removals = watcher.Deleted|> Observable.map(fun eventArgs ->
    sprintf "%s\n is deleted \n" eventArgs.Name)


(*
  Task 4:

  Below you find the definition of a type Change whose values represent changes
  to a directory. Use the event streams from Task 3 to define an event stream
  changes : IObservable<Change> of all file additions and removals in the
  current directory.
*)

type Change =
  | Addition of string
  | Removal  of string

let changes  = 
    let add = additions |> Observable.map (fun eventArgs -> Addition(eventArgs))
    let rem = removals  |> Observable.map(fun eventArgs  -> Removal(eventArgs))   
    Observable.merge add rem

let changesMsg = changes |> Observable.add (fun chg  -> match chg with
                                                        | Addition (msg) ->  printfn "%s" msg
                                                        | Removal  (msg) ->   printfn "%s" msg
                                            )
    
(*
  TaskÂ 5:

  Use the event stream changes from TaskÂ 4 to define an event stream
  turnover : IObservable<int> that tells at every file addition or removal how
  much the number of files in this directory has increased since the beginning
  (with negative numbers signifying a decrease). For example, if two files are
  added and one file is removed afterwards, there should be three events, that
  carry the numbers 1, 2, and 1, respectively.
*)

let turnover = changes |> Observable.scan ( fun count chg -> 
                                                            match chg with
                                                            | Addition (msg) ->  count + 1
                                                            | Removal (msg) ->   count - 1
                                           ) 0


let reactMsgs = turnover |> Observable.add (printfn "files are added/deleted: %d")


               
