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

    let lookup (s: string) (gdg: Dictionary.Dict) : bool =
        let rec aux (cs: char list) (gd: Dictionary.Dict) =
            match Dictionary.step cs.Head gd with
            | None -> false
            | Some (b, d) when cs.Length = 1 ->
                match Dictionary.reverse d with
                | None -> false
                | Some (b, d) -> b
            | Some (b, d) -> aux cs.Tail d

        aux (List.rev (Seq.toList s)) gdg

    let rec filterSingle pred list =
        match list with
        | x :: xs when pred x -> xs
        | x :: xs -> x :: (filterSingle pred xs)
        | [] -> []

    let generateWords s partialWord dict0 =

        let listOfTiles = MultiSet.toList s.hand

        let rec foldGenLefts (partialWord: uint32 list) (hand: uint32 list) dict1 acc =
            List.fold
                (fun acc t ->
                    match Dictionary.step (t |> (lookupTile s) |> fst) dict1 with
                    | Some (b, dict2) ->
                        let w = partialWord @ [ t ]
                        let newHand = filterSingle (fun x -> x = t) hand

                        match Dictionary.reverse dict2 with
                        | Some (b, dict3) ->
                            // printfn "Found: %A" w
                            if b then
                                foldGenLefts w newHand dict2 (fst acc, w :: (snd acc))
                            else
                                foldGenLefts w newHand dict2 ((w, (dict3, newHand)) :: (fst acc), snd acc)
                        | None ->
                            // printfn "Pword: %A" w
                            foldGenLefts w newHand dict2 acc
                    | None -> acc)
                acc
                hand

        let lefts =
            foldGenLefts partialWord listOfTiles dict0 ([], [])

        let completeLefts =
            List.map (fun x -> (x, List.empty<uint32>)) (snd lefts)

        let partialLefts = fst lefts

        let rec genWord ((dict1, hand): (Dictionary.Dict * uint32 list)) (acc: uint32 list) : uint32 list =
            List.fold
                (fun acc t ->
                    let newHand = filterSingle (fun x -> x = t) hand

                    match Dictionary.step (t |> (lookupTile s) |> fst) dict1 with
                    | Some (b, dict2) ->
                        if b then
                            // Complete word
                            t :: acc
                        else
                            // Partial word
                            genWord (dict2, newHand) (t :: acc)
                    | None -> [])
                acc
                hand

        let wholeWords =
            List.fold
                (fun acc l ->
                    match (genWord (snd l) []) with
                    | [] -> acc
                    | x -> (fst l, x) :: acc)
                []
                partialLefts

        let allWords = wholeWords @ completeLefts
        // forcePrint ("wholeWords: " + (string wholeWords.Length) + "\n")
        let filteredWords =
            List.filter
                (fun (l, r) ->
                    let word = idListToString s ((List.rev l) @ r)
                    let isValidWord = lookup (word) s.dict
                    // forcePrint(word + " is " + (if isValidWord then "valid" else "invalid") + "\n")
                    isValidWord)
                allWords

        // forcePrint ("Filtered: " + (string filteredWords.Length) + "\n")
        filteredWords


    let generateMove s ((l, r): uint32 list * uint32 list) (x, y) isRight =
        let lmoves =
            List.mapi
                (fun i c ->
                    let (char, point) = lookupTile s c

                    let coord =
                        if isRight then
                            (x - (i + 1), y)
                        else
                            (x, y - (i + 1))

                    (coord, (c, (char, point))))
                l

        let rmoves =
            List.mapi
                (fun i c ->
                    let (char, point) = lookupTile s c

                    let coord =
                        if isRight then
                            (x + (i + 1), y)
                        else
                            (x, y - (i - 1))

                    (coord, (c, (char, point))))
                r

        lmoves @ rmoves

    let generateFirstMove s ((l, r): uint32 list * uint32 list) =
        let lmoves =
            List.mapi
                (fun i c ->
                    let (char, point) = lookupTile s c

                    ((0 - i, 0), (c, (char, point))))
                l

        let rmoves =
            List.mapi
                (fun i c ->
                    let (char, point) = lookupTile s c
                    ((0 + (i + 1), 0), (c, (char, point))))
                r

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

    let keepers =
        [ 'E'
          'A'
          'R'
          'I'
          'O'
          'T'
          'N'
          'S'
          'L' ]

    let findTilesToChange (s: State.state) =
        let rec aux h acc =
            match h with
            | [] -> acc
            | x :: xs ->
                if (snd x > 2u) then
                    aux xs ((fst x) :: acc)
                else
                    aux xs acc

        let res = aux (MultiSet.toTupleList s.hand) []

        if res.Length < 3 then
            MultiSet.toList s.hand
        else
            res


    let longestWord (l: ('a list * 'a list) list) =
        let rec aux acc ls =
            match ls with
            | [] -> acc
            | x :: xs ->
                if ((fst x) @ (snd x)) > ((fst acc) @ (snd acc)) then
                    aux x xs
                else
                    aux acc xs

        aux l.Head l

    let wordPoints s w =
        let rec aux ls acc =
            match ls with
            | [] -> acc
            | x :: xs -> aux xs (acc + snd (State.lookupTile s x))

        aux ((fst w) @ (snd w)) 0

    let mostPoints s (l: (uint32 list * uint32 list) list) =
        let rec aux acc ls =
            match ls with
            | [] -> acc
            | x :: xs ->
                if (wordPoints s x) > (wordPoints s acc) then
                    aux x xs
                else
                    aux acc xs

        aux l.Head l

    let checkMove (s: State.state) (anchor: coord) (word: (uint32 list * uint32 list)) isHorizontal =
        let isEmptySquare sqr =
            match s.board.squares sqr with
            | Some (_) ->
                match s.boardState.TryFind sqr with
                | None -> true
                | Some (_) -> false
            | None -> false

        // Dir:
        //  1: Right
        // -1: Left
        let rec checkDir amt anchor dir =
            if amt = 0 then
                true
            else
                let c1 =
                    if isHorizontal then
                        ((fst anchor) + dir * amt, snd anchor + 1)
                    else
                        (fst anchor + 1, (snd anchor) + dir * amt)

                let c =
                    if isHorizontal then
                        ((fst anchor) + dir * amt, snd anchor)
                    else
                        (fst anchor, (snd anchor) + dir * amt)

                let c3 =
                    if isHorizontal then
                        ((fst anchor) + dir * amt, snd anchor - 1)
                    else
                        (fst anchor - 1, (snd anchor) + dir * amt)

                match isEmptySquare c1
                      && isEmptySquare c
                      && isEmptySquare c3 with
                | true -> checkDir (amt - 1) anchor dir
                | false -> false

        let checkStart =
            isEmptySquare (
                if isHorizontal then
                    ((fst anchor) - List.length (fst word), snd anchor)
                else
                    (fst anchor, (snd anchor) - List.length (fst word))
            )

        let checkEnd =
            isEmptySquare (
                if isHorizontal then
                    ((fst anchor) + 1 + List.length (snd word), snd anchor)
                else
                    (fst anchor, (snd anchor) + 1 + List.length (snd word))
            )

        (checkDir (fst word).Length anchor -1)
        && (checkDir (snd word).Length anchor 1)
        && checkStart
        && checkEnd

    let checkMoves st t x =
        List.fold
            (fun acc m ->
                if checkMove st (fst t) m false then
                    ((fst t, false), m) :: acc
                elif checkMove st (fst t) m true then
                    ((fst t, true), m) :: acc
                else
                    acc)
            []
            x


    let playGame cstream pieces (st: State.state) =
        let gameStopWatch = System.Diagnostics.Stopwatch.StartNew()

        let rec aux (st: State.state) =

            // Print.printLegalTiles st.legalTiles

            let mutable tilesToChange : (uint32 * uint32) list = []
            let mutable isTimedOut = false
            //Our turn check
            if (st.playerNumber = PlayersState.current st.playersState) then

                forcePrint (
                    "Player: "
                    + string (PlayersState.current st.playersState)
                    + "\n"
                )

                Print.printHand pieces (State.hand st)
                // forcePrint
                //     "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"

                forcePrint "Generating move... \n"

                let stopWatch = System.Diagnostics.Stopwatch.StartNew()
                let move =
                    if st.boardState.IsEmpty then
                        let possibleWords =
                            List.sortByDescending (fun w -> wordPoints st w) (State.generateWords st [] st.dict)
                            |> List.filter
                                (fun m ->
                                    (let wordLength = (fst m).Length + (snd m).Length
                                     let offset = wordLength / 2
                                     let anchor = (-offset, 0)
                                     checkMove st anchor m true))

                        let words =
                            List.map
                                (fun (l, r) -> (State.idListToString st (List.rev l), State.idListToString st r))
                                possibleWords

                        forcePrint (
                            "WORDS: "
                            + (string words.Length)
                            + " "
                            + (string words)
                            + "\n"
                        )

                        if possibleWords.IsEmpty then
                            None
                        else
                            let word = possibleWords.Head
                            Some(State.generateFirstMove st word)
                    else
                        let moves =
                            (Map.toList st.boardState)
                            |> Seq.map
                                (fun t ->
                                    async {
                                        let steppedDict =
                                            Dictionary.step (t |> snd |> snd |> fst) st.dict

                                        if steppedDict.IsNone then
                                            return []
                                        else
                                            let words =
                                                State.generateWords st [ t |> snd |> fst ] (snd steppedDict.Value)

                                            if words.Length = 0 then
                                                return []
                                            else
                                                match (State.generateWords st [ t |> snd |> fst ] (snd steppedDict.Value)) with
                                                | [] -> return []
                                                | x ->
                                                    return checkMoves st t x
                                    }
                                )

                            // |> Async.Sequential // Swap this an below to test parallelism
                            |> Async.Parallel
                            |> Async.RunSynchronously
                            |> Seq.fold (@) []
                            |> List.sortByDescending (fun w -> wordPoints st (snd w))

                        if moves.Length = 0 then
                            None
                        else
                            let wordInfo = moves.Head
                            let word = snd wordInfo
                            Some(State.generateMove st ((fst word).Tail, snd word) (fst (fst wordInfo)) (snd (fst wordInfo)))

                stopWatch.Stop()
                forcePrint $"Time: {string (stopWatch.Elapsed.TotalMilliseconds / 1000.0)} seconds \n"

                if isTimedOut then forcePrint "Timed out \n"
                else
                    match move with
                    | Some(m) ->
                        forcePrint ("MOVE: " + (string move))
                        send cstream (SMPlay(m))
                    | None ->
                        forcePrint "No moves available, changing hand \n"
                        let tcc = st |> findTilesToChange
                        tilesToChange <- tcc |> MultiSet.ofList |> MultiSet.toTupleList
                        send cstream (SMChange(tcc))

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

            | RCM (CMChangeSuccess (newTiles)) ->
                let st' =
                    { st with
                          hand = (State.updateHand tilesToChange newTiles st.hand)
                          playersState = PlayersState.next st.playersState }

                aux st'

            | RCM (CMForfeit (_)) ->
                let st' =
                    { st with
                          playersState = PlayersState.forfeit st.playersState }

                aux st'

            | RCM (CMPlayFailed (_))
            | RCM (CMChange (_))
            | RCM (CMPassed (_)) ->
                let st' =
                    { st with
                          playersState = PlayersState.next st.playersState }

                aux st'

            | RCM (CMTimeout (id)) ->
                if st.playerNumber = id then
                    ignore (isTimedOut <- true)

                let st' =
                    { st with
                          playersState = PlayersState.next st.playersState }

                aux st'

            | RCM (CMGameOver _) -> ()
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                aux st


        aux st

        gameStopWatch.Stop()
        forcePrint $"Total Game Time: {string (gameStopWatch.Elapsed.TotalMilliseconds / 1000.0 |> round)} seconds \n"

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
