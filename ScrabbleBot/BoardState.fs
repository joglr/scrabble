module internal BoardState

    type BoardState =
        | BS of Map<(int * int), uint32>

    let empty = BS (Map.empty<(int * int), uint32>)

    let isEmpty (BS (m)) = m.IsEmpty

    let addTile (BS (m)) t = 
        BS (m.Add(fst t, snd t))

    let addTiles (BS (m)) tl =
        let rec aux (m : Map<(int * int), uint32>) l =
            match l with
            | [] -> m
            | x::xs -> aux (m.Add(fst x, snd x)) xs
        BS (aux m tl)

    let getTile (BS (m)) c =
        m.TryFind(c)