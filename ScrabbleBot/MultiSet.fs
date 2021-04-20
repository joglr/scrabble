// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison> =
        | S of Map<'a, uint32>
        override q.ToString() =
            let rec printmultiset ml =
                match ml with
                | [] -> sprintf "{}"
                | x::xs when ml.Length = 1 -> sprintf "(%A, #%u)" (fst x) (snd x)
                | x::xs -> sprintf "(%A, #%u), %s" (fst x) (snd x) (printmultiset xs)
            match q with
            | S (m) -> sprintf "{%s}" (printmultiset (Map.toList m))


    let empty = S (Map.empty<'a, uint32>)

    let isEmpty (S (m)) = m.IsEmpty

    let size (S (m)) = Map.fold (fun c a b -> c+b) 0u m

    let contains a (S (m)) =
        let exists = m.TryFind a
        match exists with
        | Some(_) -> true
        | None -> false

    let numItems a (S (m)) =
        let exists = m.TryFind a
        match exists with
        | Some(x) -> x
        | None -> 0u

    let add a num (S (m)) =
        let exists = m.TryFind a
        match exists with
        | Some(x) -> S (m.Add(a, x+num))
        | None -> S (m.Add(a, num))

    let addSingle a (S (m)) =
        let exists = m.TryFind a
        match exists with
        | Some(x) -> S (m.Add(a, x+1u))
        | None -> S (m.Add(a, 1u))

    let remove a num (S (m)) =
        let exists = m.TryFind a
        match exists with
        | Some(x) when x <= num -> S (m.Remove a)
        | Some(x) -> S (m.Add(a, x-num))
        | None -> S (m)

    let removeSingle a (S (m)) =
        let exists = m.TryFind a
        match exists with
        | Some(x) when x = 1u -> S (m.Remove a)
        | Some(x) -> S (m.Add(a, x-1u))
        | None -> S (m)

    let fold f state (S (m)) =
        let rec go f state ml =
            match ml with
            | [] -> state
            | x::xs -> go f (f state (fst x) (snd x)) xs
        go f state (Map.toList m)

    let foldBack f (S (m)) state =
        let rec go f ml state =
            match ml with
            | [] -> state
            | x::xs -> f (fst x) (snd x) (go f xs state)
        go f (Map.toList m) state

    let map f (S (m)) =
        let rec go f l acc =
            match l with
            | [] -> acc
            | x::xs -> go f xs (add (f (fst x)) (snd x) acc)
        go f (Map.toList m) empty

    let ofList l =
        let rec go li (acc : MultiSet<'a>) =
            match li with
            | [] -> acc
            | x::xs -> go xs (addSingle x acc)
        go l empty

    let toList (S (m)) =
        let rec addn a n =
            match n with
            | 0u -> []
            | x -> a::(addn a (n-1u))
        let rec collect l acc =
            match l with
            | [] -> acc
            | x::xs -> collect xs (acc @ (addn (fst x) (snd x)))
        collect (Map.toList m) []

    let union (S (m)) (S (_m)) =
        let rec go a b acc =
            match (a, b) with
            | ([],[]) -> acc
            | ([], x::xs) -> go [] xs (add (fst x) (snd x) acc)
            | (x::xs, []) -> go xs [] (add (fst x) (snd x) acc)
            | (x::xs, y::ys) when x >= y -> go xs ys (add (fst x) (snd x) acc)
            | (x::xs, y::ys) when x < y -> go xs ys (add (fst y) (snd y) acc)
            | _ -> failwith "not sure what went wrong"
        go (Map.toList m) (Map.toList _m) empty

    let sum (S (m)) (S (_m)) =
        let go s acc =
            (toList s) @ acc
        (go (S (m)) []) |> (go (S (_m))) |> ofList

    let subtract (S (m)) (S (_m)) =
        let rec go a b =
            match (a, b) with
            | (y, []) -> y
            | (y, x::xs) -> go (remove (fst x) (snd x) y) xs
        go (S (m)) (Map.toList _m)

    let intersection (S (m)) (S (_m)) =
        let rec go a b acc =
            match (a, b) with
            | ([],[]) -> acc
            | ([], x::xs) -> acc
            | (x::xs, []) -> acc
            | (x::xs, y::ys) when x <= y -> go xs ys (add (fst x) (snd x) acc)
            | (x::xs, y::ys) when x > y -> go xs ys (add (fst y) (snd y) acc)
            | _ -> failwith "not sure what went wrong"

        go (Map.toList m) (Map.toList _m) empty
