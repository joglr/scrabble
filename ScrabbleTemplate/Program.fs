// Learn more about F# at http://fsharp.org

open System

let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)

let readLines filePath = System.IO.File.ReadLines(filePath)

let spawnMultiples name bot =
    let rec aux =
        function
        | 0 -> []
        | x -> (sprintf "%s%d" name x, bot) :: aux (x - 1)

    aux >> List.rev

[<EntryPoint>]
let main argv =
    ScrabbleUtil.DebugPrint.toggleDebugPrint true // Change to false to supress debug output

    System.Console.BackgroundColor <- System.ConsoleColor.White
    System.Console.ForegroundColor <- System.ConsoleColor.Black
    System.Console.Clear()


    let board = ScrabbleUtil.StandardBoard.standardBoard ()
    // let board      = ScrabbleUtil.InfiniteBoard.infiniteBoard ()

    // let board      = ScrabbleUtil.RandomBoard.randomBoard ()
    // let board      = ScrabbleUtil.RandomBoard.randomBoardSeed (Some 42)
    // let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoard ()
    // let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoardSeed (Some 42)

    // let board      = ScrabbleUtil.HoleBoard.holeBoard ()
    // let board      = ScrabbleUtil.InfiniteHoleBoard.infiniteHoleBoard ()

    let words = readLines "./Dictionaries/English.txt"
    // let words = readLines "./Dictionaries/Short.txt"

    let handSize = 14u
    let timeout = Some (60u * 1000u)
    let tiles = ScrabbleUtil.English.tiles 1u
    let seed = None
    //let seed = Some(422)
    // let seed = Some (Random().Next())
    // ScrabbleUtil.DebugPrint.forcePrint ("SEED: " + (string seed) + "\n")
    let port = 13001

    // let players = spawnMultiples "OxyphenButazone" Oxyphenbutazone.Scrabble.startGame 2
    // Uncomment this line to call your client
    let players =
        [ ("JScrapelt", JScrapelt.Scrabble.startGame) ]
    // let players = spawnMultiples "JScrapelt" JScrapelt.Scrabble.startGame 2

    let (dictionary, time) =
        time
            (fun () ->
                ScrabbleUtil.Dictionary.mkDict
                    Dictionary.empty
                    Dictionary.insert
                    Dictionary.step
                    (Some Dictionary.reverse)
                    words)

    ScrabbleUtil.DebugPrint.forcePrint $"Dictionary generated in {time.TotalSeconds |> round} seconds\n"

    do ScrabbleServer.Comm.startGame board dictionary handSize timeout tiles seed port players

    ScrabbleUtil.DebugPrint.forcePrint ("Server has terminated. Press Enter to exit program.\n")
    System.Console.ReadLine() |> ignore

    0
