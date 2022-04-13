module internal MultiSet
    open System
    // Green Exercises
    type MultiSet<'a> when 'a : comparison = M of Map<'a, uint32>

    let empty: MultiSet<'a> = M Map.empty

    let isEmpty = function | M mset -> Map.isEmpty mset

    let size: MultiSet<'a> -> uint32 = function | M mset -> Map.fold (fun acc _ count -> acc + count) 0u mset

    let contains: 'a -> MultiSet<'a> -> bool = fun key mset ->
        match mset with
        | M mset -> Map.containsKey key mset

    let numItems: 'a -> MultiSet<'a> -> uint32 = fun key mset ->
        match mset with
        | M mset -> Map.tryFind key mset |> Option.defaultValue 0u

    let add: 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a> = fun ele count mset -> 
        let value = numItems ele mset |> (+) count
        match mset with
        | M mset when value > 0u -> M (Map.add ele value mset)
        | M mset -> M mset

    let addSingle: 'a -> MultiSet<'a> -> MultiSet<'a> = fun key mset -> add key 1u mset


    let remove: 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a> = fun key count mset ->
        let left = (int (numItems key mset)) - (int count)
        match mset with
        | M mset when left <= 0 ->  M (Map.remove key mset)
        | M mset -> M (Map.add key (uint32 left) mset)
    
    let removeSingle: 'a -> MultiSet<'a> -> MultiSet<'a> = fun key mset -> remove key 1u mset


    let fold: ('b -> 'a -> uint32 -> 'b) -> 'b -> MultiSet<'a> -> 'b = fun folder state mset ->
        match mset with
        | M mset -> Map.fold folder state mset
    
    let foldBack: ('a -> uint32 -> 'b -> 'b) -> MultiSet<'a> -> 'b -> 'b = fun folder mset acc ->
        match mset with
        | M mset -> Map.foldBack folder mset acc
    
    // Yellow 
    let ofList = fun ls -> List.foldBack (fun x mset -> addSingle x mset) ls empty

    let toList = fun mset ->
        match mset with
        | M mset ->
            let inted = Map.map (fun key value -> (int value)) mset
            let rec adder = fun key value ls ->
                match value with
                | 0 -> ls
                | c -> adder key (c-1) (key::ls)
            Map.foldBack adder inted []

    let map = fun mapping mset -> 
        let intern = fun new_mset key value -> add (mapping key) value new_mset
        fold intern empty mset

    let union = fun mset1 mset2 ->
        let intern = fun final_mset key value -> 
            let in_final = numItems key final_mset
            let diff = uint32 (Math.Max(value, in_final) - in_final)
            add key diff final_mset
        fold intern mset1 mset2

    let sum = fun mset1 mset2 -> foldBack add mset1 mset2
    
    let subtract = fun mset1 mset2 -> foldBack remove mset2 mset1

    let intersection = fun mset1 mset2 ->
        let folder final_mset key value = 
            let count = Math.Min(value, (numItems key mset2))
            add key count final_mset
        fold folder empty mset1
