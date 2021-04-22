module internal Dictionary
    type Node =
        | Char of (char * bool)
        | Hook of (bool)

    type Dict =
        | D of Map<Node, Dict>


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
        let rec go n l =
            match l with
            | [] -> None
            | x::xs ->
                match (fst x) with
                | Char (c, b) -> if (c = n) then Some(b, extract(snd x)) else go n xs
                | Hook (b) -> Some(b, extract(snd x))
        go c l


    // Helpers

    let empty () = D (Map.empty<Node, Dict>)

    // let insert (x : string) (D (m)) = empty()

    let innersert (x : string) (D (m)) =
        let emt = Map.empty<Node, Dict>
        let rec go w (m : Map<(Node), Dict>) =
            match w with
            | [] -> m
            | [x] when x = '#' ->
                let truthAndMap = getTruthAndMap x m
                if truthAndMap.IsSome
                then m.Remove(Char(x, (fst truthAndMap.Value))).Add(Hook(true), D (snd truthAndMap.Value))
                else m.Add(Hook(true), empty ())
            | x::xs when x = '#' ->
                let truthAndMap = getTruthAndMap x m
                if truthAndMap.IsSome
                then m.Add(Hook(false), D (go xs (snd truthAndMap.Value)))
                else m.Add(Hook(false), D (go xs emt))
            | [x] ->
                let truthAndMap = getTruthAndMap x m
                if truthAndMap.IsSome
                then m.Remove(Char(x, (fst truthAndMap.Value))).Add(Char((x, true)), D (snd truthAndMap.Value))
                else m.Add(Char((x, true)), (empty ()))
            | x::xs ->
                let truthAndMap = getTruthAndMap x m
                if truthAndMap.IsSome
                then m.Add(Char(x, (fst truthAndMap.Value)), D (go xs (snd truthAndMap.Value)))
                else m.Add(Char(x, false), D (go xs emt))
        D (go (Seq.toList x) m)

    let insert (s: string) (d : Dict) =
        let rec aux acc reversed rest =
            match rest with
            | x::xs ->
                let reversed' = ([x] @ reversed)
                let str = ((new System.String (reversed' |> List.toArray)) + "#" + (new System.String (xs |> List.toArray)))
                let result = innersert str acc
                aux result reversed' xs
            | [] -> acc

        aux (d) [] (Seq.toList s)



    let step (ch : char) (D (m)) = 
        let rec aux c l =
            match l with
            | [] -> None
            | x::xs -> 
                match (fst x) with
                | Char(c, b) -> if (c = ch) then Some(b, snd x) else aux c xs
                | Hook(b) -> aux c xs
        
        aux ch (Map.toList m)
        
    let reverse (D (m)) =
        let rec aux l =
            match l with
            | [] -> None
            | x::xs -> 
                match (fst x) with
                | Char(_, _) -> aux xs
                | Hook(b) -> Some(b, snd x)
        
        aux (Map.toList m)
