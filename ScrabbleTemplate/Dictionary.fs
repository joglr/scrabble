module internal Dictionary
    type Dict =
        | D of Map<(char * bool), Dict>

    //Helpers
    let extract (D (m)) = m

    let getMap c m =
        let l = Map.toList m
        let rec go c l =
            match l with
            | [] -> None
            | x::xs -> if (fst (fst x)) = c then Some(extract(snd x)) else go c xs
        go c l

    let getTruth c m =
        let l = Map.toList m
        let rec go c l =
            match l with
            | [] -> None
            | x::xs -> if (fst (fst x)) = c then Some(snd (fst x)) else go c xs
        go c l

    let getTruthAndMap c m =
        let l = Map.toList m
        let rec go c l =
            match l with
            | [] -> None
            | x::xs -> if (fst (fst x)) = c then Some(snd (fst x), extract(snd x)) else go c xs
        go c l
    //Helpers

    let empty () = D (Map.empty<(char * bool), Dict>)

    let innersert (x : string) (D (m)) =
        let emt = Map.empty<(char * bool), Dict>
        let rec go w (m : Map<(char * bool), Dict>) =
            match w with
            | [] -> m
            | x::xs when w.Length = 1 ->
                let truthAndMap = getTruthAndMap x m
                if truthAndMap.IsSome
                then m.Remove(x, (fst truthAndMap.Value)).Add((x, true), D (snd truthAndMap.Value))
                else m.Add((x, true), (empty ()))
            | x::xs ->
                let truthAndMap = getTruthAndMap x m
                if truthAndMap.IsSome
                then m.Add((x, (fst truthAndMap.Value)), D (go xs (snd truthAndMap.Value)))
                else m.Add((x, false), D (go xs emt))
        D (go (Seq.toList x) m)

    let insert (s : string) (d : Dict) =
        let rec aux acc reversed rest =
            match rest with
            | x::xs ->
                let reversed' = ([x] @ reversed)
                let str = ((new System.String (reversed' |> List.toArray)) + "<" + (new System.String (xs |> List.toArray)))
                let result = innersert str acc
                aux result reversed' xs
            | [] -> acc

        aux (d) [] (Seq.toList s)

    let step c (D (m)) =
        let truthAndMap = getTruthAndMap c m
        match truthAndMap with
        | Some(t, m) -> Some(t, D (m))
        | None -> None

    let reverse (D (m)) =
        match getTruthAndMap '<' m with
        | Some(b,d) -> Some(b, D (m))
        | None -> None 

    let lookupDebug (x : string) (D (m)) =
        let rec go w (m : Map<(char * bool), Dict>) =
            match w with
            | [] -> false
            | x::xs when w.Length = 1 -> defaultArg (getTruth x m) false
            | x::xs ->
                let charsMap = getMap x m
                if charsMap.IsSome
                then go xs charsMap.Value
                else false
        go (Seq.toList x) m

    let lookup (s: string) (d: Dict) : bool =
        let rec aux (cs: char list) (gd: Dict) =
            match step cs.Head gd with
            | None -> false
            | Some (b, d) when cs.Length = 1 ->
                match reverse d with
                | None -> false
                | Some (b, d) -> b
            | Some (b, d) -> aux cs.Tail d

        aux (List.rev (Seq.toList s)) d

