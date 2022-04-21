// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module Eval

    open StateMonad

    (* Code for testing *)

    let hello = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    let vowels = Set.ofList ['A'; 'E'; 'I'; 'O'; 'U'; 'Æ'; 'Ø'; 'Å']

    let add a b = a >>= fun x -> b >>= fun y -> ret (x + y)
    let sub a b = a >>= fun x -> b >>= fun y -> ret (x - y)
    let mul a b = a >>= fun x -> b >>= fun y -> ret (x * y)
    let div a b = a >>= fun x -> b >>= fun y -> if y <> 0 then ret (x / y) else fail DivisionByZero
    let modulo a b = a >>= fun x -> b >>= fun y -> if y <> 0 then ret (x % y) else fail DivisionByZero

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> = 
        match a with
        | N n -> ret n
        | V v -> lookup v
        | WL -> wordLength
        | PV p -> arithEval p >>= pointValue
        | Add (a1, a2) -> add (arithEval a1) (arithEval a2)
        | Sub (a1, a2) -> sub (arithEval a1) (arithEval a2)
        | Mul (a1, a2) -> mul (arithEval a1) (arithEval a2)
        | Div (a1, a2) -> div (arithEval a1) (arithEval a2)
        | Mod (a1, a2) -> modulo (arithEval a1) (arithEval a2)
        | CharToInt c -> charEval c >>= (fun x -> ret (int x))

    and charEval c : SM<char> = 
        match c with
        | C c -> ret c
        | CV p -> arithEval p >>= characterValue
        | ToUpper c -> charEval c >>= (fun x -> ret (System.Char.ToUpper x))
        | ToLower c -> charEval c >>= (fun x -> ret (System.Char.ToLower x))
        | IntToChar a -> arithEval a >>= (fun x -> ret (char x))

    let rec boolEval b : SM<bool> =
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (a1, a2) -> (arithEval a1) >>= fun x -> (arithEval a2) >>= fun y -> ret (x = y)
        | ALt (a1, a2) -> (arithEval a1) >>= fun x -> (arithEval a2) >>= fun y -> ret (x < y)
        | Not b -> boolEval b >>= fun x -> ret (not x)
        | Conj (a1, a2) -> (boolEval a1) >>= fun x -> (boolEval a2) >>= fun y -> ret (x && y)
        | IsVowel c -> charEval c >>= fun x -> ret ((Set.contains x vowels))
        | IsLetter c -> charEval c >>= fun x -> ret (System.Char.IsLetter x)
        | IsDigit c -> charEval c >>= fun x -> ret (System.Char.IsDigit x)

    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = 
        match stmnt with
        | Declare v -> declare v
        | Ass (v, a) -> (arithEval a) >>= update v 
        | Skip -> ret ()
        | Seq (s1, s2) -> stmntEval s1 >>= fun _ -> stmntEval s2
        | ITE (b, s1, s2) -> (boolEval b) >>= fun x ->
            if x then
                push >>>=
                stmntEval s1 >>>=
                pop
            else
                push >>>=
                stmntEval s2 >>>=
                pop
        | While (b, s) -> 
            let rec loop (b_aux, s_aux) =
                (boolEval b_aux) >>= fun x ->
                if x then
                    stmntEval s_aux >>>=
                    loop (b_aux, s_aux)
                else
                    ret ()
            push >>>=
            loop (b, s) >>>=
            pop

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = fun w pos acc -> 
        let state = mkState [("_pos_", pos); ("_acc_", acc); ("_result_",0)] w ["_acc_"; "_pos_"; "_result_"]
        stmntEval stm >>>= lookup "_result_" |> evalSM state




    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = fun coord -> 
        let state = mkState [("_x_", fst coord); ("_y_", snd coord); ("_result_",0)] [] ["_x_";"_y_";"_result_"]
        let res = stmntEval stm >>>= lookup "_result_" |> evalSM state
        match res with
        | Success result_val -> Map.tryFind result_val m |> Success
        | Failure err -> Failure err

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"