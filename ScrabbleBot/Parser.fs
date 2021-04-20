// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

module internal Parser

    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.


    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"

    let pif       = pstring "if" <?> "string"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "string"
    let pletter        = satisfy System.Char.IsLetter <?> "string"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "string"

    let spaces         = many whitespaceChar
    let spaces1        = many1 whitespaceChar

    let (.>*>.) a b = a .>> spaces .>>. b <?> "string"
    let (.>*>) a b  = a .>> spaces .>> b <?> "string"
    let (>*>.) a b  = a >>. spaces >>. b <?> "string"

    let betweenChars a b p = pchar a >*>. p .>*> pchar b

    let parenthesise p = betweenChars '(' ')' p
    let aprost p = betweenChars ''' ''' p
    let bracket p = betweenChars '{' '}' p

    let pid = pchar '_' <|> pletter .>>. many (palphanumeric <|> pchar '_') |>> (fun (h, t) -> System.String.Concat(h::t)) <?> "string"

    let unop op a = op >*>. a <?> "string"
    let binop op a b = a .>*> op .>*>. b <?> "string"

    (*AExp non-terminals*)
    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    (*CExp non-terminals*)
    let CTermParse, ctref = createParserForwardedToRef<cExp>()
    (*BExp non-terminals*)
    let BLogiParse, blref = createParserForwardedToRef<bExp>()
    let BCompParse, bcref = createParserForwardedToRef<bExp>()
    let BAtomParse, baref = createParserForwardedToRef<bExp>()
    (*StmExp non-terminals*)
    let SSeqTermParse, ssref = createParserForwardedToRef<stm>()
    let STermParse, saref = createParserForwardedToRef<stm>()

    (*TermParse*)
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "string"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "string"
    (*ProdParse*)
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "string"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "string"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "string"
    (*AtomParse*)
    let NParse   = pint32 |>> N <?> "Int"
    //let NegParse = unop (pchar '-')  pint32 |>> (fun x -> N -x) <?> "Negate"
    let NegParse = unop (pchar '-')  pint32 |>> (fun x -> Mul (N -1, N x)) <?> "string"
    let PVParse = pPointValue >*>. AtomParse |>> (fun x -> PV x) <?> "string"
    let VarParse = pid |>> V <?> "Variable"
    let CTIParse = pCharToInt >*>. parenthesise CTermParse |>> CharToInt <?> "string"
    let ParParse = parenthesise TermParse

    (*CTermParse*)
    let CharParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "string"
    let TUParse = pToUpper >*>. parenthesise CTermParse |>> ToUpper <?> "string"
    let TLParse = pToLower >*>. parenthesise CTermParse |>> ToLower <?> "string"
    let CVParse = pCharValue >*>. ParParse |>> CV <?> "string"
    let ITCParse = pIntToChar >*>. ParParse |>> IntToChar <?> "string"

    (*BLogiParse*)
    let AndParse = binop (pstring "/\\") BCompParse BLogiParse |>> Conj <?> "string"
    let OrParse = binop (pstring "\\/") BCompParse BLogiParse |>> (fun (a, b) -> Not(Conj(Not(a), Not(b)))) <?> "string"
    (*BCompParse*)
    let EqParse = binop (pchar '=') TermParse TermParse |>> AEq <?> "string"
    let NEqParse = binop (pstring "<>") TermParse TermParse |>> (fun (a, b) -> Not(AEq(a, b))) <?> "string"
    let LTParse = binop (pchar '<') TermParse TermParse |>> ALt <?> "string"
    let LTEqParse = binop (pstring "<=") TermParse TermParse |>> (fun (a, b) -> Not(Conj(Not(ALt(a, b)), Not(Not(Not(AEq(a, b))))))) <?> "string"
    let GTParse = binop (pchar '>') TermParse TermParse |>> (fun (a, b) -> (Conj(Not(AEq(a, b)), Not(ALt(a, b))))) <?> "string"
    let GTEqParse = binop (pstring ">=") TermParse TermParse |>> (fun (a, b) -> Not(ALt(a, b))) <?> "string"
    (*BAtomParse*)
    let ILParse = pIsLetter >>. parenthesise CTermParse |>> IsLetter <?> "string"
    let TrueParse = pTrue |>> (fun x -> TT) <?> "string"
    let FalseParse = pFalse |>> (fun x -> FF) <?> "string"
    let NotParse = unop (pchar '~') BLogiParse |>> Not <?> "string"
    let BParParse = parenthesise BLogiParse

    (*SSeqTermParse*)
    let SeqParse = binop (pchar ';') STermParse SSeqTermParse |>> (fun (a, b) -> Seq(a, b))
    (*STermParse*)
    let DeclareParse = pdeclare >>. spaces1 >>. pid |>> Declare
    let AssParse = binop (pstring ":=") pid TermParse |>> (fun (a, b) -> Ass(a, b))
    let ITEParse = pif >*>. parenthesise BLogiParse .>*> pthen .>*>. bracket SSeqTermParse .>*> pelse .>*>. bracket SSeqTermParse |>> (fun ((i, t), e) -> ITE(i, t, e))
    let IFTParse = pif >*>. parenthesise BLogiParse .>*> pthen .>*>. bracket SSeqTermParse |>> (fun (i, t) -> ITE(i, t, Skip))
    let WhParse = pwhile >*>. parenthesise BLogiParse .>*> pdo .>*>. bracket SSeqTermParse |>> (fun (w, d) -> While(w, d))

    let tParse = pif >*>. parenthesise (pstring "b") .>*> pthen .>*>. bracket (pstring "do this")

    do tref := choice [AddParse; SubParse; ProdParse]
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]
    do aref := choice [CTIParse; PVParse; NegParse; NParse; VarParse; ParParse]

    do ctref := choice [ITCParse; CVParse; TLParse; TUParse; CharParse]

    do blref := choice[AndParse; OrParse; BCompParse]
    do bcref := choice[EqParse; NEqParse; LTParse; LTEqParse; GTParse; GTEqParse; BAtomParse]
    do baref := choice[TrueParse; FalseParse; NotParse; BParParse]

    do ssref := choice[SeqParse; STermParse]
    do saref := choice[WhParse; ITEParse; IFTParse; DeclareParse; AssParse]


    let AexpParse = TermParse

    let CexpParse = CTermParse

    let BexpParse = BLogiParse

    let stmntParse = SSeqTermParse

    (* These five types will move out of this file once you start working on the project *)
    type coord      = int * int
    type squareProg = Map<int, string>
    type boardProg  = {
        prog       : string;
        squares    : Map<int, squareProg>
        usedSquare : int
        center     : coord

        isInfinite : bool   // For pretty-printing purposes only
        ppSquare   : string // For pretty-printing purposes only
    }

    type word   = (char * int) list
    type square = Map<int, word -> int -> int -> int>

    let parseSquareFun _ = failwith "not implemented"

    let parseBoardFun _ = failwith "not implemented"

    type boardFun = coord -> square option
    type board = {
    center        : coord
    defaultSquare : square
    squares       : boardFun
    }

    let parseBoardProg (bp : boardProg) = failwith "not implemented"

