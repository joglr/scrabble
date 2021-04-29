module internal PlayersState

    type PlayersState =
        | PS of (uint32 * List<uint32>)
        // current player index * list of players


    let create (startP : uint32) (numP : uint32) = 
        let rec aux n acc =
            match n with
            | 0u -> acc
            | x -> aux (n-1u) (n::acc)
        PS (startP-1u, aux numP [])

    let next (PS (i, l)) =
        if (i+1u >= uint32 l.Length) then PS (0u, l) else PS (i + 1u, l)

    let current (PS (i, l)) =
        l.[int i]

    let forfeit (PS (i, l)) =
        let newPlayerList = List.filter (fun x -> x <> l.[int i]) l
        PS (i, newPlayerList)
