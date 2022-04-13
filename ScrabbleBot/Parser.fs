// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
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
    let pdeclare  = pstring "not implemented"

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
    let pid = 
        let parser = ((pchar '_') <|> pletter) .>>. (many (palphanumeric <|> pchar '_'))
        let collectChars = fun x -> List.fold (fun acc char -> acc + (string char)) "" ((fst x) :: (snd x))
        parser |>> collectChars

    let unop op a = (op .>>. spaces) >>. a
    let binop op p1 p2 = (p1 .>> (spaces >>. op .>>. spaces)) .>>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharExpParse, cref = createParserForwardedToRef<cExp>()

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

    do tref.Value <- choice [AddParse; SubParse; ProdParse]
    do pref.Value <- choice [MulParse; DivParse; ModParse; AtomParse]
    do aref.Value <- choice [PVParse; NegParse; NParse; charToIntParse; VParse;  ParParse]
    do cref.Value <- choice [toUpperParse; toLowerParse; intToCharParse; CVParse; CParse]

    let AexpParse = TermParse 

    let CexpParse = CharExpParse

    let BexpParse = pstring "not implemented"

    let stmParse = pstring "not implemented"

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
