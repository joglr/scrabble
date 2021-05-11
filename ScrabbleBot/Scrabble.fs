namespace JScrapelt

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            Some(List.tail [ for g in m.Groups -> g.Value ])
        else
            None

    let parseMove ts =
        let pattern =
            @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"

        Regex.Matches(ts, pattern)
        |> Seq.cast<Match>
        |> Seq.map
            (fun t ->
                match t.Value with
                | Regex pattern [ x; y; id; c; p ] -> ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)")
        |> Seq.toList


module State =
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    open PlayersState

    type state =
        { board: Parser.board
          boardState: Map<coord, (uint32 * (char * int))>
          playersState: PlayersState
          dict: Dictionary.Dict
          playerNumber: uint32
          hand: MultiSet.MultiSet<uint32>
          legalTiles: Set<coord>
          tiles: Map<uint32, tile> }

    let updateHand
        (toRemove: (uint32 * uint32) list)
        (toAdd: (uint32 * uint32) list)
        (hand: MultiSet.MultiSet<uint32>)
        =
        (List.fold (fun hand (id, count) -> MultiSet.remove id count hand) hand toRemove
         |> List.fold (fun hand (id, count) -> MultiSet.add id count hand))
            toAdd

    let lookupTile s id =
        match s.tiles.TryFind id with
        | Some (t) -> t.MinimumElement
        // TODO: Handle wildcard
        | None -> failwith "this should not happen"

    // let generateWords (c: coord) (s: state) (dir : bool) =

    let idListToString s ulist =
        List.fold (fun acc next -> acc + (next |> (lookupTile s) |> fst |> string)) "" ulist


    // Look at the rest of hand. Map each letter from hand to a dictionary Lookup

    let lookup (s : string) (gdg : Dictionary.Dict) : bool =
        let rec aux (cs : char list) (gd : Dictionary.Dict) = 
            match Dictionary.step cs.Head gd with
            | None      -> false
            | Some(b,d) when cs.Length = 1 -> 
                match Dictionary.reverse d with 
                | None -> false
                | Some(b,d) -> b
            | Some(b,d) -> aux cs.Tail d
        aux (List.rev (Seq.toList s)) gdg

    let rec filterSingle pred list =
        match list with
        | x :: xs when pred x -> xs
        | x :: xs -> x :: (filterSingle pred xs)
        | [] -> []

    let generateWords s partialWord dict0 =

        let listOfTiles = MultiSet.toList s.hand

        let rec foldGenLefts (partialWord: uint32 list) (hand : uint32 list) dict1 acc =
            List.fold
                (fun acc t ->
                    match Dictionary.step (t |> (lookupTile s) |> fst) dict1 with
                        | Some(b,dict2) ->
                            let w = partialWord @ [ t ]
                            let newHand = filterSingle (fun x -> x = t) hand
                            match Dictionary.reverse dict2 with
                            | Some(b,dict3) ->
                                // printfn "Found: %A" w
                                foldGenLefts w newHand dict2 ((w,(dict3,newHand))::acc)
                            | None ->
                                // printfn "Pword: %A" w
                                foldGenLefts w newHand dict2 acc
                        | None -> acc
                )
                acc
                hand

        let lefts = foldGenLefts partialWord listOfTiles dict0 []

        let rec genWord ((dict1,hand) : (Dictionary.Dict * uint32 list)) (acc : uint32 list) : uint32 list =
            List.fold
                (fun acc t ->
                    let newHand = filterSingle (fun x -> x = t) hand
                    match Dictionary.step (t |> (lookupTile s) |> fst) dict1 with
                    | Some(b,dict2) ->
                        if b then
                            // Complete word
                            t::acc
                        else
                            // Partial word
                            genWord (dict2,newHand) (t::acc)
                    | None -> []
                )
                acc
                hand

        let wholeWords = 
            List.fold
                (fun acc l ->
                    match (genWord (snd l) []) with
                    | [] -> acc
                    | x -> (fst l, x)::acc
                ) [] lefts
         
        List.filter (fun (l,r) -> lookup (idListToString s ((List.rev l) @ r)) s.dict) wholeWords


    let generateMove s (l,r) (x,y) isRight =
        let lmoves =
            List.mapi
                (fun i c ->
                    let (char, point) = lookupTile s c
                    let coord = if isRight then (x - i, y) else (x, y - i)
                    (coord, (c, (char, point)))
                ) l
        let rmoves =
            List.mapi
                (fun i c ->
                let (char, point) = lookupTile s c
                let coord = if isRight then (x + (i+1), y) else (x, y + (i+1))
                (coord, (c, (char, point)))
            ) r
        lmoves @ rmoves



    // TODO: Keep direction as well
    let updateLegalTiles (state: state) (ms: list<coord * (uint32 * (char * int))>) =
        List.fold
            (fun legalTilesAcc tile ->
                let crd = fst tile

                let adjacent : coord list =
                    [ (fst crd + 1, snd crd)
                      (fst crd - 1, snd crd)
                      (fst crd, snd crd + 1)
                      (fst crd, snd crd - 1) ]

                let validNearbyMoves =
                    List.filter
                        (fun crd ->
                            match state.board.squares crd with
                            | Some (_) ->
                                match state.boardState.TryFind crd with
                                | Some (_) -> false
                                | None -> true

                            | None -> false)
                        adjacent

                List.fold (fun acc item -> Set.add item acc) legalTilesAcc validNearbyMoves

                )
            state.legalTiles
            ms


    let updateBoardState (prev: Map<coord, (uint32 * (char * int))>) tiles =
        List.fold (fun acc next -> Map.add (fst next) (snd next) acc) prev tiles

    let mkState b d pn pa pt h t =
        { board = b
          boardState = Map.empty<coord, (uint32 * (char * int))>
          playersState = create pt pa
          dict = d
          playerNumber = pn
          hand = h
          legalTiles = Set.empty<coord>
          tiles = t }

    let board st = st.board
    let dict st = st.dict
    let playerNumber st = st.playerNumber
    let hand st = st.hand

module Print =

    let printHand pieces hand =
        hand
        |> MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

    let printLegalTiles (legalTiles) =
        forcePrint (sprintf "LegalTiles: %A" legalTiles)
        printfn ""
        ()


module Scrabble =

    let longestWord (l : ('a list * 'a list)list) =
        let rec aux acc ls =
            match ls with 
            | [] -> acc
            | x::xs -> if ((fst x) @ (snd x)) > ((fst acc) @ (snd acc)) then aux x xs  else aux acc xs
        aux l.Head l

    let wordPoints s w =
        let rec aux ls acc =
            match ls with
            | [] -> acc
            | x::xs -> aux xs (acc + snd (State.lookupTile s x))
        aux ((fst w) @ (snd w)) 0

    let mostPoints s (l : (uint32 list * uint32 list)list) =
        let rec aux acc ls =
            match ls with
            | [] -> acc
            | x::xs -> if (wordPoints s x) > (wordPoints s acc) then aux x xs else aux acc xs
        aux l.Head l

    let checkMove (s : State.state) (anchor : coord) (word : (uint32 list * uint32 list)) isRight =
        let checkSqr sqr isRight = 
            match s.board.squares sqr with
            | Some(x) -> 
                match s.boardState.TryFind sqr with 
                | Some(x) -> true
                | None -> false
            | None -> false

        let rec checkLeft amt anchor =
            if amt = 0 then
                true
            else
                let c = if isRight then ((fst anchor)-amt, snd anchor) else (fst anchor, (snd anchor)-amt)
                match checkSqr c isRight with
                | true -> checkLeft (amt+1) anchor
                | false -> false

        let rec checkRight amt anchor =
            if amt = 0 then
                true
            else
                let c = if isRight then ((fst anchor)+amt, snd anchor) else (fst anchor, (snd anchor)+amt)
                match checkSqr c isRight with
                | true -> checkLeft (amt-1) anchor
                | false -> false

        (checkLeft (fst word).Length anchor) && (checkRight (snd word).Length anchor)


    let playGame cstream pieces (st: State.state) =

        let rec aux (st: State.state) =

            Print.printHand pieces (State.hand st)
            Print.printLegalTiles st.legalTiles

            //let tilesToChange : (uint32 * uint32) list = []
            //Our turn check
            if (st.playerNumber = PlayersState.current st.playersState) then

                // forcePrint
                //     "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
                
                //TODO: fix else case, fix getting stuck on changing tiles
                if st.boardState.IsEmpty then
                    let isRight = true
                    let anchor = (0, 0)
                    let possibleWords = List.sortByDescending (fun w -> wordPoints st w) (State.generateWords st [] st.dict) |> Set.ofList
                    let words = Set.map (fun (l,r) -> (State.idListToString st (List.rev l),State.idListToString st r)) possibleWords
                    forcePrint ("WORDS: " + (string words.Count) + " " + (string words))
                    if possibleWords.IsEmpty then send cstream (SMChange (MultiSet.toList st.hand))
                    let word = possibleWords.MinimumElement
                    let move = State.generateMove st word anchor isRight
                    send cstream (SMPlay (move))
                else
                    let m =
                        List.map
                            (fun t ->
                                let steppedDict = Dictionary.step (t |> snd |> snd |> fst) st.dict
                                if steppedDict.IsNone then None
                                else
                                    match List.sortByDescending (fun w -> wordPoints st w) (State.generateWords st [t |> snd |> fst] (snd steppedDict.Value)) with
                                    | [] -> None
                                    | x -> 
                                        if checkMove st (fst t) x.Head true then Some ((fst t, true), x.Head)
                                        elif checkMove st (fst t) x.Head false then Some ((fst t, false), x.Head)
                                        else None
                            )
                            (Map.toList st.boardState)
                    forcePrint (string m)
                    let fm = List.filter (fun x -> Option.isSome x) m
                    let word = fm.Head
                    let move = State.generateMove st (snd word.Value) (fst (fst word.Value)) (snd (fst word.Value))
                    send cstream (SMPlay (move))

                    // debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) msg) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess (ms, _points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)

                // Add played tiles to boardState
                let updatedBoardState = State.updateBoardState st.boardState ms

                let mappedMs =
                    List.map (fun (_, (id, _tile)) -> (id, 1u)) ms

                // Update hand
                let updatedHand =
                    State.updateHand mappedMs newPieces st.hand

                // Give turn to next player
                let updatedPlayersState = PlayersState.next st.playersState
                let updatedLegalTiles = State.updateLegalTiles st ms

                let st' =
                    { st with
                          hand = updatedHand
                          boardState = updatedBoardState
                          playersState = updatedPlayersState
                          legalTiles = updatedLegalTiles }

                aux st'
            | RCM (CMPlayed (_pid, ms, _points)) ->
                (* Successful play by other player. Update your state *)

                // Add played tiles to boardState
                let updatedBoardState = State.updateBoardState st.boardState ms

                let updatedLegalTiles = State.updateLegalTiles st ms

                let st' =
                    { st with
                          boardState = updatedBoardState
                          playersState = PlayersState.next st.playersState
                          legalTiles = updatedLegalTiles }

                aux st'
            | RCM (CMPlayFailed (_pid, _ms)) ->
                (* Failed play. Update your state *)
                let st' = st // TODO: This state needs to be updated

                aux st'

            | RCM (CMChangeSuccess (newTiles)) ->
                let st' =
                    { st with
                          hand = (State.updateHand [] newTiles st.hand) }

                aux st'

            | RCM (CMForfeit (_)) ->
                let st' =
                    { st with
                          playersState = PlayersState.forfeit st.playersState }

                aux st'

            | RCM (CMPlayFailed (_))
            | RCM (CMChange (_))
            | RCM (CMPassed (_))
            | RCM (CMTimeout (_)) ->
                let st' =
                    { st with
                          playersState = PlayersState.next st.playersState }

                aux st'


            | RCM (CMGameOver _) -> ()
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                aux st


        aux st

    let startGame
        (boardP: boardProg)
        (dictf: bool -> Dictionary.Dict)
        (numPlayers: uint32)
        (playerNumber: uint32)
        (playerTurn: uint32)
        (hand: (uint32 * uint32) list)
        (tiles: Map<uint32, tile>)
        (timeout: uint32 option)
        (cstream: Stream)
        =
        debugPrint (
            sprintf
                "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n"
                numPlayers
                playerNumber
                playerTurn
                hand
                timeout
        )

        let dict = dictf true // Uncomment if using a gaddag for your dictionary
        //let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.parseBoardProg boardP

        let handSet =
            List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber numPlayers playerTurn handSet tiles)
