namespace Cthulhu

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> printf "%d -> (%A, %d)\n" x (Map.find x pieces) i) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        placedChars   : Map<coord, uint32> 
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        playerTurn    : uint32
        playerCount   : uint32
        active        : bool
    }

    let mkState b d pn h pt pc = {placedChars = Map.empty;
                                    board = b;
                                    dict = d;
                                    playerNumber = pn;
                                    hand = h;
                                    playerTurn = pt;
                                    active = true;
                                    playerCount = pc;
                                    }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

module Scrabble =
    open System.Threading
    
    let incTurn (st: State.state) = {st with playerTurn = (if st.playerTurn = st.playerCount then 1u else st.playerTurn + 1u)}

    let playTurn (st: State.state) cstream pieces = 
    
        st.board.center |> printfn "center: %A"
        Print.printHand pieces st.hand

        let chosen_word =
            match st.placedChars.IsEmpty with
            | true -> Heuristic.playFirstTurn st.board.center st.hand st.dict
            | false ->
                let candidates = Heuristic.findAnchorPoints st.placedChars
                Heuristic.chooseWord candidates st.dict st.hand true

        printfn "Chosen word: %A" chosen_word        
        //let _ = System.Console.ReadLine()
        

        match chosen_word with
        | None ->
            System.Threading.Thread.Sleep 100
            let ans = Heuristic.panicFind st.hand st.placedChars st.dict
            printf "Panicked and found %A" ans
            let _ = System.Console.ReadLine()
            match ans with
            | None -> 
                send cstream (SMChange [(Heuristic.changeHand st.hand)])
            | Some m-> 
                send cstream (SMPlay m)
                printf "Player %d -> Server:\n%A\n" (State.playerNumber st) m // keep the debug lines. They are useful.

        | Some (co, wo, dir) -> 
            let move = Heuristic.formatMove co wo dir st.placedChars.IsEmpty
            send cstream (SMPlay move)
            printf "Player %d -> Server:\n%A\n" (State.playerNumber st) move // keep the debug lines. They are useful.

        let msg = recv cstream

        match msg with
        | RCM (CMPlaySuccess(ms, points, newPieces)) ->
            (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
            let removedTiles = List.map snd ms
            let auxRemove = fun state tile -> MultiSet.removeSingle (fst tile) state
            let auxAdd = fun state (uid, count) -> MultiSet.add uid count state
            let new_hand = List.fold auxRemove st.hand removedTiles
            let new_hand2 = List.fold auxAdd new_hand newPieces
            let st' = {st with hand = new_hand2; placedChars = (Heuristic.registerPlacement ms st.placedChars) } 
            st'
        | RCM (CMPlayFailed (pid, ms)) ->
            (* Failed play. Update your state *)
            let st' = st // This state needs to be updated
            st'
        | RCM (CMChangeSuccess ls) ->
            let st_tile_removed = {st with hand = MultiSet.removeSingle ((Heuristic.changeHand st.hand)) st.hand} 
            let st_tile_add = {st_tile_removed with hand = MultiSet.addSingle (fst ls.[0]) st_tile_removed.hand} 
            st_tile_add
        | RCM (CMGameOver _) -> {st with active=false}
        | RCM _ -> st
        //| RCM a -> failwith (sprintf "not implmented: %A" a)
        | RGPE err -> printfn "Gameplay Error:\n%A" err; st

    let passTurn (st: State.state) cstream pieces = 
        let msg = recv cstream
        printf "Player %d <- Server:\n%A\n" (State.playerNumber st) msg // keep the debug lines. They are useful.

        match msg with
        | RCM (CMPlayed (pid, ms, points)) ->
            let st' = {st with placedChars = (Heuristic.registerPlacement ms st.placedChars)} 
            (* Successful play by other player. Update your state *)
            st'
        | RCM (CMPlayFailed (pid, ms)) ->
            (* Failed play. Update your state *)
            st
        | RCM (CMGameOver _) -> {st with active=false}
        | RCM (CMChange _) -> st
        | RCM _ -> st
        //| RCM a -> failwith (sprintf "not implmented: %A" a)
        | RGPE err -> printfn "Gameplay Error:\n%A" err; st
        

    let playGame cstream pieces (st : State.state) =
        let rec aux (st : State.state) =
            match st.active with
            | true -> 
                match st.playerTurn with
                | pt when pt = st.playerNumber -> playTurn st cstream pieces |> incTurn |> aux
                | _ -> passTurn st cstream pieces |> incTurn |> aux
            | false -> ()

        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        
        printf "Starting game!
                    number of players = %d
                    player id = %d
                    player turn = %d
                    hand =  %A
                    timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet playerTurn numPlayers)
        