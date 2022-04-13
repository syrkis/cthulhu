// Insert your StateMonad.fs from Assignment 6 here. All modules must be internal.

module internal StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)

    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> = 
        S (fun s -> Success ((), {s with vars = List.tail s.vars}))

    let wordLength : SM<int> = 
        S (fun s -> Success (List.length s.word, s))

    let characterValue (pos : int) : SM<char> = 
        S (
            fun s -> 
                match pos with
                | x when x >= 0 && x < List.length s.word -> Success ((fst s.word.[pos]), s)
                | _ -> Failure (IndexOutOfBounds pos)
        )

    let pointValue (pos : int) : SM<int> = 
        S (
            fun s -> 
                match pos with
                | x when x >= 0 && x < List.length s.word -> Success ((snd s.word.[pos]), s)
                | _ -> Failure (IndexOutOfBounds pos)
        )

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let declare (var : string) : SM<unit> = 
        S (
            fun s -> 
                match var with
                | x when Set.contains x s.reserved -> Failure (ReservedName x)
                | x when Map.containsKey x (s.vars.[0]) -> Failure (VarExists x)
                | x -> Success ((), {s with vars = (Map.add x 0 (s.vars.[0])) :: s.vars})
        )

    let update (var : string) (value : int) : SM<unit> =
        let rec aux (suf : Map<string, int> list) (pre : Map<string, int> list) =
            match suf with
            | []      -> Some(pre)
            | m :: ms -> 
                match Map.tryFind var m with
                | Some _ -> Some (pre @ [Map.add var value m] @ ms)
                | None   -> aux ms (pre @ [m])

        S (
            fun s -> 
                match aux (s.vars) [] with
                | Some v -> Success ((), {s with vars = v})
                | None   -> Failure (VarNotFound var)
        )
