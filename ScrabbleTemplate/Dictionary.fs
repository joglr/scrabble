module internal Dictionary
    type Dict =
        | D of Map<char, (bool * Dict)>

    //Helpers
    let extract (D (m)) = m
    //Helpers

    let empty () = D (Map.empty<char, (bool * Dict)>)

    let innersert (x : string) (D (m)) =
        let rec go word (map : Map<char, (bool * Dict)>) =
            match word with
            | [] -> map
            | x::xs->
                let b = word.Length = 1
                match Map.tryFind x map with 
                | Some(b,d) -> Map.add x (b, D (go xs (extract d))) map
                | None -> Map.add x (b, D (go xs Map.empty<char, (bool * Dict)>)) map
        D (go (Seq.toList x) m)

    let insert (s : string) (d : Dict) =
        let rec aux acc reversed rest =
            match rest with
            | x::xs ->
                let reversed' = ([x] @ reversed)
                let str = ((new System.String (reversed' |> List.toArray)) + "<" + (new System.String (xs |> List.toArray)))
                // printfn "%A" str
                let result = innersert str acc
                aux result reversed' xs
            | [] -> acc

        aux (d) [] (Seq.toList s)

    let step c (D (m)) =
        Map.tryFind c m

    let reverse (D (m)) =
        Map.tryFind '<' m

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

