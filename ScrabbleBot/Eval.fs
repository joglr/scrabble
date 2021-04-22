// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    let hello = [
        ('H', 4);
        ('E', 1);
        ('L', 1);
        ('L', 1);
        ('O', 1);
    ]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []

    let add a b = a >>= fun x -> b >>= fun y -> ret ((+) x y)
    let div a b = a >>= fun x -> b >>= fun y -> if (y = 0) then fail DivisionByZero else ret ((/) x y)

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *)

    let (.=.) a b = AEq (a, b)
    let (.<.) a b = ALt (a, b)
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

    let rec arithEval a : SM<int> =
        match a with
        | N(n) -> ret n
        | V(v) -> lookup v
        | WL -> wordLength
        | PV(ae) -> arithEval ae >>= fun a -> pointValue a
        | Add(x, y) -> arithEval x >>= fun a -> arithEval y >>= fun b -> ret (a + b)
        | Sub(x, y) -> arithEval x >>= fun a -> arithEval y >>= fun b -> ret (a - b)
        | Mul(x, y) -> arithEval x >>= fun a -> arithEval y >>= fun b -> ret (a * b)
        | Div(x, y) -> arithEval x >>= fun a -> arithEval y >>= fun b -> if (b = 0) then fail DivisionByZero else ret (a / b)
        | Mod(x, y) -> arithEval x >>= fun a -> arithEval y >>= fun b -> if (b = 0) then fail DivisionByZero else ret (a % b)
        | CharToInt(ce) -> charEval ce >>= fun a -> ret ((int) a)

    and charEval c : SM<char> =
        match c with
        | C(c) -> ret c
        | CV(ae) -> arithEval ae >>= fun a -> characterValue a
        | ToUpper(ce) -> charEval ce >>= fun cev -> ret (System.Char.ToUpper cev)
        | ToLower(ce) -> charEval ce >>= fun cev -> ret (System.Char.ToLower cev)
        | IntToChar(ae) -> arithEval ae >>= fun a -> ret ((char) a)

    let rec boolEval b : SM<bool> =
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq(x, y) -> arithEval x >>= fun a -> arithEval y >>= fun b -> ret (a = b)
        | ALt(x, y) -> arithEval x >>= fun a -> arithEval y >>= fun b -> ret (a < b)
        | Not(be) -> boolEval be >>= fun a -> ret (not a)
        | Conj(x, y) -> boolEval x >>= fun a -> boolEval y >>= fun b -> ret (a && b)
        | IsLetter(ce) -> charEval ce >>= fun a -> ret (System.Char.IsLetter a)
        | IsDigit(ce) -> charEval ce >>= fun a -> ret (System.Char.IsDigit a)


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        | Declare(var) -> declare var
        | Ass(var, value) -> arithEval value >>= fun a -> update var a
        | Skip -> ret ()
        | Seq(stm1, stm2) -> stmntEval stm1 >>>= stmntEval stm2
        | ITE(I, T, E) -> push >>>= boolEval I >>= fun b -> if b then stmntEval T else stmntEval E >>>= pop
        | While(WC, WS) -> push >>>= boolEval WC >>= fun b -> if b then stmntEval (Seq (WS, While(WC, WS))) else pop

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let prog = new StateBuilder()

    let rec arithEval2 a =
        match a with
        | N(n) -> prog {
            return n
            }
        | V(v) -> prog {
            let! res = lookup v
            return res
            }
        | WL -> prog {
            return! wordLength
            }
        | PV(ae) -> prog {
            let! a = arithEval ae
            return! pointValue a
            }
        | Add(x, y) -> prog {
            let! a = arithEval x
            let! b = arithEval y
            return a + b
            }
        | Sub(x, y) -> prog {
            let! a = arithEval x
            let! b = arithEval y
            return a - b
            }
        | Mul(x, y) -> prog {
            let! a = arithEval x
            let! b = arithEval y
            return a * b
            }
        | Div(x, y) -> prog {
            let! a = arithEval x
            let! b = arithEval y
            if (b = 0) then return! (fail DivisionByZero) else return a / b
            }
        | Mod(x, y) -> prog {
            let! a = arithEval x
            let! b = arithEval y
            if (b = 0) then return! (fail DivisionByZero) else return a % b
            }
        | CharToInt(ce) -> prog {
            let! c = charEval2 ce
            return (int c)
        }
    and charEval2 c =
        match c with
        | C(c) -> prog {
            return c
            }
        | CV(ae) -> prog {
            let! i = arithEval2 ae
            return! characterValue i
            }
        | ToUpper(ce) -> prog {
            let! c = charEval2 ce
            return (System.Char.ToUpper c)
            }
        | ToLower(ce) -> prog {
            let! c = charEval2 ce
            return (System.Char.ToLower c)
            }
        | IntToChar(ae) -> prog {
            let! i = arithEval2 ae
            return (char i)
            }
    let rec boolEval2 b =
        match b with
        | TT -> prog {
            return true
            }
        | FF -> prog {
            return false
            }
        | AEq(x, y) -> prog {
            let! a = arithEval2 x
            let! b = arithEval2 y
            return (a = b)
            }
        | ALt(x, y) -> prog {
            let! a = arithEval2 x
            let! b = arithEval2 y
            return (a < b)
            }
        | Not(be) -> prog {
            let! b = boolEval2 be
            return (not b)
            }
        | Conj(x, y) -> prog {
            let! a = boolEval2 x
            let! b = boolEval2 y
            return (a && b)
            }
        | IsLetter(ce) -> prog {
            let! c = charEval2 ce
            return (System.Char.IsLetter c)
            }
        | IsDigit(ce) -> prog {
            let! c = charEval2 ce
            return (System.Char.IsDigit c)
            }

    let rec stmntEval2 stm =
        match stm with
        | Declare(var) -> prog {
            do! declare var
            }
        | Ass(var, value) -> prog {
            let! v = arithEval2 value
            do! update var v
            }
        | Skip -> prog {
            return ()
            }
        | Seq(stm1, stm2) -> prog {
            do! stmntEval2 stm1
            do! stmntEval2 stm2
            }
        | ITE(I, T, E) -> prog {
            do! push
            let! b = boolEval2 I
            if b then do! stmntEval2 T else do! stmntEval2 E
            do! pop
            }
        | While(WC, WS) -> prog {
            do! push
            let! b = boolEval2 WC
            if b then do! stmntEval2(While(WC, WS)) else do! pop
        }

(* Part 4 *)

    type word = (char * int) list
    type squareFun = word -> int -> int -> int

    let stmntToSquareFun stm =
        let sqf (w : word) pos acc =
            let s = mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0);] w ["_pos_"; "_acc_"; "_result_";]
            let sm = stmntEval stm >>>= arithEval (V "_result_") >>= fun b -> ret b
            match (evalSM s sm) with
                | Success(x) -> x
                | Failure(_) -> 0
        sqf

    type coord = int * int

    // type boardFun = coord -> Result<squareFun option, Error>

    let stmntToBoardFun stm m =
        let bdf (x, y) =
            let s = mkState [("_x_", x); ("_y_", y); ("_result_", 0)] [] ["_x_"; "_y_"; "_result_";]
            let sm = stmntEval stm >>>= lookup "_result_" >>= fun id -> ret (Map.tryFind id m)
            match (evalSM s sm) with
                | Success(x) -> x
                | Failure(_) -> None
        bdf

    // type board = {
    //     center        : coord
    //     defaultSquare : squareFun
    //     squares       : boardFun
    // }

    // let mkBoard c defaultSq boardStmnt ids =
    //     let sqrs = List.map (fun a -> (fst a, stmntToSquareFun (snd a))) ids |> Map.ofList
    //     {
    //         center = c;
    //         defaultSquare = (stmntToSquareFun defaultSq);
    //         squares = (stmntToBoardFun boardStmnt sqrs);
    //     }
