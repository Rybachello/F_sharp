// Here are some optional exercises, answer the questions below and
// make sure to write tests.

// 1. implement addition, multiplication, subtraction for Nat as
//    custom operators
type Nat =
  | Zero
  | Suc of Nat

let One = Suc Zero
let Two =Suc(Suc(Zero)) // Suc One
let Three = Suc(Suc(Suc Zero)) //Suc Two
let Four = Suc(Suc(Suc(Suc Zero)))// Suc Three
let Five = Suc(Suc(Suc(Suc(Suc Zero)))) //Suc Five
let Six = Suc(Suc(Suc(Suc(Suc(Suc Zero))))) //Suc Six

let rec add m n = 
   match m with
     | Zero   -> n
     | Suc m1 -> Suc (add m1 n) 

add Two Two 
//val it : Nat = Suc (Suc (Suc (Suc Zero)))

let inc = fun n -> Suc n // = i++
let dec n1 = 
    match n1 with
    | Zero-> Zero
    | Suc n1 -> n1

let rec sub m n =     
  match n with
  | Zero -> m
  | Suc n1 -> dec (sub m n1)
                    
sub Six One         
// subtract 1 from x, subtract 1 from y: 8 - 3 = 7 - 2 = 6 - 1 = 5 - 0 = 5          
         
let rec mult m n =
        match m,n with
        |(_,Zero) -> Zero
        |(Zero,_) -> Zero
        |(m1,Suc(n1)) -> add m1 (mult m1 n1)  

// Operators?
let (.+) = add
Four .+ Six

mult Two Three
//val it : Nat = Suc (Suc (Suc (Suc (Suc (Suc Zero)))))

// 2. Write a converstion function from Nat to int

let rec NalToInt n =
    match n with
    | Zero -> 0
    | Suc n1 -> 1 + NalToInt n1

NalToInt (add Four Six)
//val it : int = 10
// 3. Write an evaluator for the following language of aritmetic expressions:
// 4. Extend the language and the evaluator to support Sub, and Mult

type Exp =
  | Val of Nat
  | Add of Exp * Exp  
  | Sub of Exp * Exp
  | Mult of Exp * Exp  

let rec eval exp = 
    match exp with
    | Val n -> NalToInt n
    | Add (n1,n2) ->  eval n1 + eval n2
    | Sub (n1,n2) -> eval n1 - eval n2
    | Mult (n1,n2) ->  
        match n1,n2 with
        | (Val(Zero),_)-> 0
        | (_,Val(Zero)) -> 0
        | (_,_)-> eval n1 * eval n2
    

let testSum = Add(Add(Val(One),Val(Two)),Val(Two))
eval testSum

let tesSub = Sub(Add(Val(Zero),Val(One)),Val(Two))
eval tesSub

let testMult = Mult(Val(Six),Val(Two))
eval testMult

// 5. Write an evaluator for this language which has variables as well.

type Exp<'t> =
  | Val of Nat
  | Var of 't
  | Add of Exp<'t> * Exp<'t>

//    The evaluator should take an lookup function too:
//    eval : ('t -> int) -> Exp<'t> -> int
let rec ev (lookup: 't -> int) exp = 
    match exp with
    | Val(n) -> NalToInt(n)
    | Var(t) -> lookup t
    | Add(t1,t2) -> ev lookup t1  + ev lookup t2


// 6. Write a map function for Exp<'t>, it can be thought of a
//    'renaming' function that renames variables.

// The type of your function is too restrictive and your function does the wrong thing
let rec map func exp = 
    match exp with
    | Val(n) -> Val(func n) // Is this renaming a variable?
    | Var(t) -> Var(func t)
    | Add(t1,t2) -> Add((map func t1),(map func t2))

// 7. Write a bind function (see section 6.8.2) for Exp<'t>, it can be
//    thought of as a substitution function that replaces variables with
//    expressions.
