namespace Cthulhu

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

module internal Heuristic =

    let charMapNoJoker = 
        let nums = List.map (fun x -> uint32 x) [1..26]
        Map.ofList (List.zip nums nums)
    let charMap = 
        let nums = List.map (fun x -> uint32 x) [0..26]
        let chars = 'Ø'::['A'..'Z']
        Map.ofList (List.zip nums chars)
    
    let charIdMap = 
        let nums = List.map (fun x -> uint32 x) [0..26]
        let chars = 'Ø'::['A'..'Z']
        Map.ofList (List.zip chars nums)

    let charPointMap =
        let nums = List.map (fun x -> uint32 x) [0..26]  // TODO fix fix
        let points = [0; 1; 3; 3; 2; 1; 4; 2; 4; 1; 8; 5; 1; 3; 1; 1; 3; 10; 1; 1; 1; 1; 4; 4; 8; 4; 10]
        Map.ofList (List.zip nums points)

    let letterFreq = ['E'; 'M'; 'A'; 'H'; 'R'; 'G'; 'I'; 'B'; 'O'; 'F'; 'T'; 'Y'; 'N'; 'W'; 'S'; 'K'; 'L'; 'V'; 'C'; 'X'; 'U'; 'Z'; 'D'; 'J'; 'P'; 'Q'; 'Ø']

    let getChar cid = Map.find cid charMap
    let getCharId c = Map.find c charIdMap
    let getCharPoints cid = Map.find cid charPointMap

    type direction = 
        | Right = 0
        | Down = 1

    // Adds placement of word into placedChars
    let registerPlacement : list<coord * (uint32 * (char * int))> -> Map<coord, uint32> -> Map<coord, uint32> = 
        fun move placedChars -> 
            let folder = fun acc ele -> Map.add (fst ele) (fst (snd ele)) acc
            List.fold folder placedChars move

    let findAnchorPoints =
        fun placedChars -> 

            // Map<coord, uint32 * uint32 * direction> 
            // value is cid * length * dir
            let anchorCandidates = Map.empty
            
            // Determines length of word that can be placed
            let dingDongSize = fun co direc ->

                let inProxy (x0,y0) d =
                    match direc with
                    | direction.Down -> 
                        Map.containsKey (x0+1,y0+d) placedChars ||
                        Map.containsKey (x0-1,y0+d) placedChars ||
                        Map.containsKey (x0,y0+d+1) placedChars
                    | direction.Right -> 
                        Map.containsKey (x0+d,y0+1) placedChars ||
                        Map.containsKey (x0+d,y0-1) placedChars ||
                        Map.containsKey (x0+d+1,y0) placedChars
                    | _ -> failwith "Invalid direction"

                let rec whenHit = fun (x0,y0) delta ->
                    match delta with
                    | 8 -> 8
                    | d -> 
                        match inProxy (x0,y0) d with
                        | true -> d
                        | false -> whenHit (x0,y0) (d+1)

                (whenHit co 1) - 1 
                
            let checkCoord = fun cands co cid ->
                // printfn "chechCoord co: %A" co
                match co with
                    | co when cid = 0u -> cands
                    | (x0, y0) when not (Map.containsKey (x0+1,y0) placedChars) && not (Map.containsKey (x0-1,y0) placedChars) -> 
                        // add with bool Right
                        match dingDongSize (x0,y0) direction.Right with
                        | 0 -> cands
                        | len -> Map.add (x0, y0) (cid, len, direction.Right) cands

                    | (x0, y0) when not (Map.containsKey (x0,y0+1) placedChars) && not (Map.containsKey (x0,y0-1) placedChars) -> 
                        // add with bool Down   
                        match dingDongSize (x0,y0) direction.Down with
                        | 0 -> cands
                        | len -> Map.add (x0, y0) (cid, len, direction.Down) cands
                    | _ -> cands
            Map.fold checkCoord anchorCandidates placedChars
            

    let chooseWord anchorPoints (dic: Dictionary.Dict) hand work =  

        let maxWord ls1 ls2 = if List.length ls1 > List.length ls2 then ls1 else ls2

        let longestWord anc_cid len = 
            let rec search c (isWord, subd) subhand depth =
                let compare best_found cid _ = 
                    match cid with
                    | 0u -> best_found
                    | cid -> 
                        let sub_c = getChar cid
                        match Dictionary.step sub_c subd with
                        | Some (b, d') ->
                            let subsubhand = MultiSet.removeSingle cid subhand
                            let found = search sub_c (b, d') subsubhand (depth+1)
                            maxWord found best_found
                        | None -> best_found

                match depth with
                | d when d < len  -> 
                    //do
                    match MultiSet.fold compare [] subhand with
                    | [] when isWord -> [c]
                    | [] -> []
                    | ls -> c::ls
                | _ -> []

            let anc_c = getChar anc_cid
            match Dictionary.step anc_c dic with
            | Some (_, subd) -> search anc_c (false, subd) hand 0 // either 0 or 1
            | None -> failwith "massive error, dic should contain all letters"

        let anchorFolder best_word co (anc_cid, len, direc) = 
            // printfn "anchorFolder co: %A" co
            let best_anchor_word = longestWord anc_cid len
            match best_anchor_word with
            | [] -> best_word
            | anc_word -> 
                match best_word with
                | Some (b_co, word, b_direc) -> 
                    if List.length anc_word  > List.length word then 
                        Some (co, anc_word, direc)
                    else 
                        best_word 
                | None -> Some (co, anc_word, direc)

        match work with
        | true -> Map.fold anchorFolder None anchorPoints
        | false -> None

    let addPair (x0, y0) (x1, y1) = (x0+x1, y0+y1)

    let formatMove coord (word: list<char>) direction includeFirst = 
        let dir_vec =
            match direction with
            | direction.Right -> (1,0)
            | direction.Down -> (0,1)
            | _ -> failwith "Invalid direction"
        
        let tile_placements = 
            let rec aux co wo move =
                match wo with
                | x::xs -> 
                    let new_co = addPair co dir_vec
                    let cid = (getCharId x)
                    let points = getCharPoints cid
                    aux new_co xs (
                        (
                            new_co,
                            (cid,
                                (x, points)
                            )
                        )::move
                    )
                | [] -> move
            match includeFirst with
            | true -> aux (addPair coord (-1, 0)) word []
            | false -> aux coord word.[1..] []

        tile_placements

    let playFirstTurn (centerCoord: int*int) hand' (dic: Dictionary.Dict) = 
        let hand = MultiSet.remove 0u 7u hand'

        let folderFunc best_word cid _ = 
            let anchorPoint = Map.ofList [(centerCoord, (cid, 7, direction.Right))]
            let best_anchor_word = chooseWord anchorPoint dic (MultiSet.removeSingle cid hand) true
            match best_anchor_word with
            | None -> best_word
            | Some (co, word, direc) -> 
                match best_word with 
                | None -> Some (co, word, direc)
                | Some (b_co, b_word, b_direc) ->
                    if List.length word >= List.length b_word then 
                        Some (co, word, direc)
                    else 
                        best_word

        MultiSet.fold folderFunc None hand

    let changeHand hand =
        List.fold (
            fun acc letter -> 
                let letterId = getCharId letter
                match MultiSet.contains letterId hand with
                | true -> letterId
                | false -> acc
            ) 69u letterFreq
    
    // Just attempt to place any tile anywhere
    let panicFind hand placedChars dic =

        let rec traverseLeft x y acc =
            match Map.tryFind (x,y) placedChars with
            | Some cid -> traverseLeft (x-1) y ((getChar cid)::acc)
            | None -> acc 

        let rec traverseRight x y acc =
            match Map.tryFind (x,y) placedChars with
            | Some cid -> traverseRight (x+1) y ((getChar cid)::acc)
            | None -> acc

        let rec traverseUp x y acc =
            match Map.tryFind (x,y) placedChars with
            | Some cid -> traverseUp x (y-1) ((getChar cid)::acc)
            | None -> acc

        let rec traverseDown x y acc =
            match Map.tryFind (x,y) placedChars with
            | Some cid -> traverseDown x (y+1) ((getChar cid)::acc)
            | None -> acc
        
        let tryPlace cid prev co = 
            match prev with
            | Some x -> Some x
            | None -> 
                let lw = traverseLeft ((fst co) - 1) (snd co) []
                let rw = traverseRight ((fst co) + 1) (snd co) [] |> List.rev
                let w_l1 = lw @ ((getChar cid)::rw)
                let w1 = if (List.length w_l1) > 1 then System.String.Concat(Array.ofList(w_l1))  else "YAWS"
                match Dictionary.lookup w1 dic with
                | true -> 
                    let uw = traverseUp (fst co) ((snd co) - 1) []
                    let dw = traverseDown (fst co) ((snd co) + 1) [] |> List.rev
                    let w_l2 = uw @ ((getChar cid)::dw)
                    let w2 = if (List.length w_l2) > 1 then System.String.Concat(Array.ofList(w_l2))  else "HELLO"
                    match Dictionary.lookup w2 dic with
                    | true -> 
                        Some co
                    | false -> None
                | false -> None
                
        let allCoord =
            let above = Map.keys placedChars |> Seq.map (fun (x,y) -> (x,y-1)) |> List.ofSeq
            let below = Map.keys placedChars |> Seq.map (fun (x,y) -> (x,y+1)) |> List.ofSeq
            let right = Map.keys placedChars |> Seq.map (fun (x,y) -> (x+1,y)) |> List.ofSeq
            let left  = Map.keys placedChars |> Seq.map (fun (x,y) -> (x-1,y)) |> List.ofSeq
            let all = above @ below @ right @ left
            List.filter (fun co -> not (Map.containsKey co placedChars)) all

        let rec tryChar prev (cid: uint32) _  =
            match prev with
            | Some (co,c_id,is_joker) -> Some (co, c_id, is_joker)
            | None ->
                match cid with
                | 0u -> 
                    let found = Map.fold tryChar None charMapNoJoker
                    match found with
                    | Some (co, c_id, _) -> Some (co, c_id, true)
                    | None -> None
                | cid -> 
                    match List.fold (tryPlace cid) None allCoord with
                    | Some co -> Some (co, cid, false)
                    | None -> None

        let ans = MultiSet.fold tryChar None hand
        match ans with
        | None -> None
        | Some (co, cid, is_joker) ->
            let points = if is_joker then 0 else (getCharPoints cid)
            let c = getChar cid
            let real_cid = if is_joker then 0u else cid
            Some [(co,(real_cid, (c, points)))]


            



