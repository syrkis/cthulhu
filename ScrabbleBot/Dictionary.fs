module Dictionary
    type Dict = 
        | Leaf of bool * char
        | Node of bool * char * Map<char, Dict>
    
    let empty (_: unit) = Leaf (false, ' ')

    let rec insert (x: string) =
        function
        | Leaf (_, s) when x.Equals "" -> Leaf (true, s)
        | Node (_, s, d) when x.Equals "" -> Node (true, s, d)

        | Leaf (b, s) -> 
            Node (b, s, Map.add x.[0] (insert x.[1..] (Leaf (false, x.[0]))) Map.empty)
        | Node (b, s, d) ->
            let existing = if Map.containsKey x.[0] d then Map.find x.[0] d else Leaf (false, x.[0])
            Node (b, s, Map.add x.[0] (insert x.[1..] existing) d)

    let rec lookup (x: string) =
        function
        | Leaf (b, _) when x.Equals "" -> b
        | Node (b, _, _) when x.Equals "" -> b

        | Leaf _ -> false
        | Node (_, _, d) -> if Map.containsKey x.[0] d then lookup x.[1..] (Map.find x.[0] d) else false

    let step (x: char) =
        function
        | Leaf _ -> None
        | Node (_, _, d) ->
            if Map.containsKey x d then
                let existing = (Map.find x d)
                match existing with
                | Leaf (b, _) -> Some (b, existing)
                | Node (b, _, _) -> Some (b, existing)
            else
                None
