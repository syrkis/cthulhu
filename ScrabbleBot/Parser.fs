// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    //open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar" <?> "intToChar"
    let pPointValue = pstring "pointValue" <?> "pointValue"

    let pCharToInt  = pstring "charToInt" <?> "CharToInt"
    let pToUpper    = pstring "toUpper" <?> "ToUpper"
    let pToLower    = pstring "toLower" <?> "ToLower"
    let pCharValue  = pstring "charValue" <?> "CharValue"

    let pTrue       = pstring "true" <?> "True"
    let pFalse      = pstring "false" <?> "False"
    let pIsDigit    = pstring "isDigit" <?> "IsDigit"
    let pIsLetter   = pstring "isLetter" <?> "IsLetter"
    let pIsVowel   = pstring "isVowel" <?> "IsVowel"

    let pif       = pstring "if" <?> "if"
    let pthen     = pstring "then" <?> "then"
    let pelse     = pstring "else" <?> "else"
    let pwhile    = pstring "while" <?> "while"
    let pdo       = pstring "do" <?> "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "spaces"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = p1 .>>. (spaces >>. p2)
    let (.>*>) p1 p2  = (p1 .>*>. p2) |>> fst
    let (>*>.) p1 p2  = (p1 .>*>. p2) |>> snd

    // let parenthesise p = (pchar '(' .>> spaces) >>. p .>> (spaces >>. pchar ')')
    let parenthesise p = ((pchar '(' >*>. p) .>*> pchar ')')
    let brackethise p = ((pchar '{' >*>. p) .>*> pchar '}')
    let pid = 
        let parser = ((pchar '_') <|> pletter) .>>. (many (palphanumeric <|> pchar '_'))
        let collectChars = fun x -> List.fold (fun acc char -> acc + (string char)) "" ((fst x) :: (snd x))
        parser |>> collectChars

    let unop op a = op >*>. a
    let binop op p1 p2 = (p1 .>*>  op) .>*>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let CharExpParse, cref = createParserForwardedToRef<cExp>()

    let BoolExpParse, bref = createParserForwardedToRef<bExp>()
    let Bool2ExpParse, b2ref = createParserForwardedToRef<bExp>()
    let Bool3ExpParse, b3ref = createParserForwardedToRef<bExp>()

    let StmExpParse, sref = createParserForwardedToRef<stm>()
    let Stm1ExpParse, s1ref = createParserForwardedToRef<stm>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"

    let PVParse = pPointValue >*>. AtomParse |>> PV <?> "PV"
    let NParse   = pint32 |>> N <?> "Int"
    let VParse   = pid |>> V <?> "Var"
    //let NegParse = unop (pchar '-') NParse <?> "Neg"
    let NegParse = (unop (pchar '-') pint32 |>> fun x -> (N -1, N x)) |>> Mul <?> "Neg"
    let ParParse = parenthesise TermParse

    let CParse = (pchar ''') >>. (palphanumeric <|> whitespaceChar) .>> (pchar ''')  |>> C <?> "Char"
    let CVParse = pCharValue >*>. AtomParse |>> CV <?> "CV"
    let toUpperParse = pToUpper >*>. (parenthesise CharExpParse) |>> ToUpper <?> "toUpper"
    let toLowerParse = pToLower >*>. (parenthesise CharExpParse) |>> ToLower <?> "toLower"
    
    let intToCharParse = pIntToChar >*>. AtomParse |>> IntToChar <?> "intToChar"
    let charToIntParse = pCharToInt >*>. (parenthesise CharExpParse) |>> CharToInt <?> "charToInt"

    let logicalOr = fun (b1, b2) -> Not (Conj (Not (b1), Not (b2)))
    let lte = fun b -> logicalOr (AEq (b), ALt (b))

    let trueParse = pstring "true" |>> (fun _ -> TT) <?> "true"
    let falseParse = pstring "false"  |>> (fun _ -> FF) <?> "false"
    let andParse = binop (pstring @"/\") Bool2ExpParse BoolExpParse |>> Conj <?> "logical and"
    let orParse = binop (pstring @"\/") Bool2ExpParse BoolExpParse |>> logicalOr <?> "logical or"
    let notParse = unop (pchar '~') Bool3ExpParse |>> Not <?> "not"
    let eqParse = binop (pchar '=') AtomParse TermParse |>> AEq <?> "equality"
    let notEqParse = binop (pstring "<>") AtomParse TermParse |>> (fun a1 -> Not (AEq (a1)))  <?> "not equality"
    let ltParse = binop (pchar '<') AtomParse TermParse |>> ALt <?> "less than"
    let lteParse = binop (pstring "<=") AtomParse TermParse |>> lte <?> "less than or equal"
    let gtParse = binop (pchar '>') AtomParse TermParse |>> (fun b -> Not (lte b)) <?> "greater than"
    let gteParse = binop (pchar '>') AtomParse TermParse |>> (fun b -> Not (ALt (b))) <?> "greater than or equal"
    let isDigitParse = unop pIsDigit CharExpParse |>> IsDigit
    let isLetterParse = unop pIsLetter CharExpParse |>> IsLetter
    let isVowelParse = unop pIsVowel CharExpParse |>> IsVowel
    let bParParse = parenthesise BoolExpParse

    let assignParse = (pid .>*> pstring ":=") .>*>. TermParse |>> Ass
    let declareParse = (pdeclare >>. spaces1) >>. pid |>> Declare
    let simcolParse = binop (pchar ';') Stm1ExpParse StmExpParse |>> Seq

    let iteParse =  ((((pstring "if" >*>. bParParse) .>*> pstring "then") .>*>. 
                    (brackethise Stm1ExpParse)) .>*> pstring "else") .>*>. 
                    (brackethise Stm1ExpParse) |>> fun ((b,s1),s2) -> ITE (b,s1,s2)

    let itParse =   ((pstring "if" >*>. bParParse) .>*> pstring "then") .>*>. 
                    (brackethise Stm1ExpParse) |>> fun (b,s) -> ITE (b,s, Skip)

    let whileParse = ((pstring "while" >*>. bParParse) .>*> pstring "do") .>*>. 
                     (brackethise Stm1ExpParse) |>> While
    
// a := 7 + 5
    do tref.Value <- choice [AddParse; SubParse; ProdParse]
    do pref.Value <- choice [MulParse; DivParse; ModParse; AtomParse]
    do aref.Value <- choice [PVParse; NegParse; NParse; charToIntParse; VParse;  ParParse]

    do cref.Value <- choice [toUpperParse; toLowerParse; intToCharParse; CVParse; CParse]

    do bref.Value <- choice [andParse; orParse; Bool2ExpParse]
    do b2ref.Value <- choice [eqParse; notEqParse; ltParse; lteParse; gtParse; gteParse; Bool3ExpParse] 
    do b3ref.Value <- choice [trueParse; falseParse; notParse;
                             isDigitParse; isLetterParse; isVowelParse; bParParse]
    do sref.Value <- choice [simcolParse; Stm1ExpParse]
    do s1ref.Value <- choice [assignParse; declareParse; iteParse; itParse; whileParse]

    let AexpParse = TermParse 

    let CexpParse = CharExpParse 

    let BexpParse = BoolExpParse 

    let stmParse = StmExpParse
    (* The rest of your parser goes here *)
    type coord = int * int
    type word   = (char * int) list
    type squareProg = Map<int, string>
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>

(*
    type boardProg = {
        prog : string;
        squares : Map<int, squareProg>
        usedSquare : int
        center : coord
        isInfinite : bool // For pretty-printing purposes only
        ppSquare : string // For pretty-printing purposes only
    }
*)
    let parseSquareProg sqp = 
        let aux = fun _ prog -> run stmParse prog |> getSuccess |> stmntToSquareFun 
        match sqp with
        | m -> Map.map aux m
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    let parseBoardProg s sqs = 
        let res = run stmParse s |> getSuccess 
        stmntToBoardFun res sqs
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    //let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
    let mkBoard (bp: ScrabbleUtil.boardProg) = 
        let mprime = Map.map (fun _ sqp -> parseSquareProg sqp) bp.squares
        {center = bp.center;
            defaultSquare = Map.find bp.usedSquare mprime;
            squares = parseBoardProg bp.prog mprime}
