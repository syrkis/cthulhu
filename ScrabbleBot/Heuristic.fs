namespace Cthulhu

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

module internal Heuristic =

    let charMap = 
        let nums = List.map (fun x -> uint32 x) [1..26]
        let chars = ['A'..'Z']
        Map.ofList (List.zip nums chars)
    
    let getChar cid = Map.find cid charMap

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
                match co with
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
            

    let chooseWord anchorPoints (dic: Dictionary.Dict) hand =  

        let maxWord ls1 ls2 = if List.length ls1 > List.length ls2 then ls1 else ls2

        
        let longestWord anc_cid len = 
            let rec search c (isWord, subd) subhand depth =
                let compare best_found cid _ = 
                    let sub_c = getChar cid
                    match Dictionary.step sub_c subd with
                    | Some (b, d') ->
                        let subsubhand = MultiSet.removeSingle cid subhand
                        //printfn "%A" subhand
                        //printfn "%A" subsubhand
                        //printfn ""
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


        Map.fold anchorFolder None anchorPoints

        
        


