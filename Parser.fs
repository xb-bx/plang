module Parser
open FParsec
type Number =
| Integer of value: int64
| Float of value: float

type Op =
| Plus
| Minus
| Star
| Slash
| And
| Or
| GT
| LT
| GTEQ
| LTEQ
| EQEQ
| NEQ

type Type =
| SimpleType of modul: string option * name: string
| ReferenceType of name: Type 
| BoxedType of name: string
| ArrayType of underlaying: Type 
type Expression =
| Variable of name: string
| String of value: string
| Char of value: char
| Number of num: Number
| Binary of left: Expression * op: Op * right: Expression
| Negate of value: Expression
| Not of value: Expression
| MemberAccess of value: Expression * mem: string
| Indexer of value: Expression * index: Expression
| CommaSeperated of value: Expression list
| Call of value: Expression * arg: Expression
| New of name: string
| NewArray of length: Expression * typ: Type
| Box of value: Expression
| AnyExpression of obj: obj
type Statement =
| LetStatement of name: string * typ: Type option * value: Expression
| Assign of target: Expression * value: Expression
| If of condition: Expression * body: Statement list * elseBody: Statement list option
| While of condtion: Expression * body: Statement list
| CallStatement of call: Expression 
| Return of value: Expression option

type Argument = { name: string; typ: Type }
let argument (name, typ): Argument = { name = name; typ = typ }
type Field = { name: string; typ: Type }
let field(name, typ): Field = { name = name; typ = typ }
type Function = { name: string; args: Argument list; retType: Type; body: Statement list } 
type Struct = { name: string; fields: Field list }
type FunctionOrStruct = 
| Function of Function
| Struct of Struct

type Definition =
| FunctionDefinition of Function
| StructDefinition of Struct
| ImportDefinition of moduleName: string
type Program = Definition list 


    
let rec printExpression expr = 
    match expr with
    | Variable(name) -> printf "%s" name
    | Number(num) ->
        match num with
        | Float(f) -> printf "%f" f
        | Integer(i) -> printf "%i" i
    | Binary(left, op, right) -> 
        printf "("
        printExpression left
        printf " %A " op
        printExpression right
        printf ")"
    | Negate(expr) -> 
        printf "-"
        printExpression expr
    | Not(expr) -> 
        printf "|"
        printExpression expr
    | MemberAccess(expr, memb) -> 
        printf "(" 
        printExpression expr
        printf ".%s)" memb
    | Indexer(expr, index) -> 
        printf "(" 
        printExpression expr
        printf "["
        printExpression index
        printf "])"
        
    | x -> printfn "%A" x
    


        
let parser = ParserCombinator()
let integer = 
    parser {
        let! res = pint64
        return Integer(res)
    }
let real = 
    parser {
        let! res = manyChars digit .>>. pchar '.' .>>. manyChars digit 
        let res = fst (fst res) + ((snd (fst res)).ToString()) + (snd res)
        return Float(float res)
    }
let (<&&>) f g = (fun x -> f x && g x)
let (<||>) f g = (fun x -> f x || g x)
let identifier = many1Satisfy2 isLetter (isLetter <||> isDigit) .>> spaces



let variable = 
    parser {
        let! res = identifier 
        return Variable(res)
    }
let number = 
    parser {
        let! res = 
            choice [ 
                attempt real;
                attempt integer; ] .>> spaces
        return Number(res)
    }
let binary op left right =
    Binary(left, op, right)
let betweenParenthesis pstart pend = between (pchar pstart .>> spaces) (pchar pend .>> spaces)

let result f p  = 
    parser {
        let! res = p
        return f res
    }



let opp = new OperatorPrecedenceParser<Expression, Unit, Unit>()

let commaSeperated = sepBy opp (pchar ',' .>> spaces) |>>CommaSeperated 
//let stringLiteral = pchar '"' >>. manyChars (noneOf "\"")
let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                        | 'n' -> '\n'
                                                        | 'r' -> '\r'
                                                        | 't' -> '\t'
                                                        | c   -> c)
let charLiteral =
    let normal = (satisfy (fun c -> c <> '\\' && c <> '\''))
    between (pchar '\'') (pchar '\'') (normal <|> escapedChar) |>> Char
let stringLiteral =
    let normalCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    between (pstring "\"") (pstring "\"")
            (manyStrings (normalCharSnippet <|> (escapedChar |>> string))) |>> String
let psimpletype = 
    choice [
        attempt ((identifier |>> Some) .>> pchar '.' .>>. identifier |>> SimpleType)
        attempt (identifier |> result (fun name -> SimpleType(None, name)))
    ]
let rec simpletype = 
    (pchar '^' >>. identifier |> result (fun name -> BoxedType(name))) 
    <|> psimpletype
let foldTypes p =
    parser {
        let rec foldTypes' strs (typ: Type) =
            let matchSingle x =
                match x with 
                | "[]" -> ArrayType(typ)
                | "*" -> ReferenceType(typ)
                | _ -> raise (System.Exception())
            match strs with 
            | [] -> typ
            | [x] -> matchSingle x
            | x :: tail -> foldTypes' tail (matchSingle x)

        let! res = p
        let (strs: string list, typ: Type) = res
        return foldTypes' (strs |> List.rev) typ
    }
let word x = pstring x .>> spaces
let ptype = 
    choice [
        attempt (many1 (word "*" <|> word "[]") .>>. simpletype |> foldTypes)
        attempt simpletype
    ]

let pnew = 
    choice [
        attempt (word "new" >>. pchar '[' >>. opp .>> pchar ']' .>>. ptype) |>> NewArray 
        attempt (word "new" >>. identifier) |>>  New
    ]
let pbox =
    word "box" >>. opp |>> Box
let simpleExpression = number <|> pnew <|> pbox <|> variable <|> stringLiteral <|> charLiteral <|> betweenParenthesis '(' ')' opp
let foldTrailing p = 
    parser {
        let rec foldTrailing' (rest: (string * Expression) list) expr = 
            let matchSingle elem = 
                match elem with
                | (".", Variable(name)) -> MemberAccess(expr, name)
                | ("[", index) -> Indexer(expr, index)
                | ("(", exprs) -> Call(expr, exprs)
                | _ -> System.Exception() |> raise
            match rest with
            | [] -> expr
            | [x] -> matchSingle x 
            | x :: tail -> foldTrailing' tail (matchSingle x)

        let! res = p
        let (exprs, expr) = res
        return foldTrailing' expr exprs
    }
let (%>>.) left right = 
    parser {
        let! left = left
        let! right = right
        return (left.ToString(), right)
    }
let expression = 
    simpleExpression 
    .>>. many (
        choice [ 
            attempt (pchar '.' %>>. variable );
            attempt (pchar '(' %>>. commaSeperated .>> pchar ')');
            attempt (pchar '[' %>>. opp .>> pchar ']')
            attempt (word "is" .>>. variable)
        ])
    |> foldTrailing

let plet = 
    choice [
        attempt (word "let" >>. identifier .>> word ":" .>>. ptype .>> word "=" .>>. opp |>> (fun ((name, typ), value) -> LetStatement (name, Some(typ), value)))
        attempt (word "let" >>. identifier .>> word "=" .>>. opp |>> (fun (name, value) -> LetStatement (name, None, value)))
    ]
let passign =
    expression .>> word "=" .>>. opp |>> Assign
let mutable statement, statementRef = createParserForwardedToRef()

let body = pchar '{' .>> spaces >>. sepBy statement (pchar ';' .>> spaces) .>> spaces .>> pchar '}' .>>  spaces
let pwhile = word "while" >>. opp .>>. body |>> While
let pif = 
    choice [
        word "if" >>. opp .>>. body .>> word "else" .>>. body |>> fun ((cond, body), elsebody) -> If(cond, body, Some elsebody)
        word "if" >>. opp .>>. body |>> fun (cond, body) -> If(cond, body, None)
    ]

let call =
    let isCall x =
        match x with
        | Call(_,_) -> preturn (CallStatement(x))
        | _ -> fail "Expected call expression"
    expression >>= isCall

let preturn = 
    choice [
        attempt (word "return" >>. opp) |>> Some |>> Return  
        attempt (word "return") |>> fun _ -> Return(None) 
    ]

statementRef := 
    choice [
        attempt pif;
        attempt pwhile;
        attempt passign;
        attempt call;
        attempt plet;
        attempt preturn;
    ]
let arg = identifier .>> pchar ':' .>> spaces .>>. ptype |>> argument
let commaSeperatedArgs = sepBy arg (pchar ',')
let pfunction = word "fn" >>. identifier .>>. betweenParenthesis '(' ')' commaSeperatedArgs .>> pchar ':' .>> spaces .>>. ptype .>>. body |>> fun ((((name, args), rettyp), body)) -> { name = name; args = args; retType = rettyp; body = body}
let pfield = identifier .>> pchar ':' .>> spaces .>>. ptype |>> field 
let fieldlist = sepBy pfield (pchar ';' .>> spaces)
let pstruct = word "struct" >>. identifier .>>. betweenParenthesis '{' '}' fieldlist |>> fun (name, fields) -> {name = name; fields = fields}
let splitBy f items =
    let rec splitBy' items left right =
        match items with
        | [] -> (left, right)
        | x :: rest -> 
            if f x then 
                splitBy' rest (left @ [x]) right
            else 
                splitBy' rest left (right @ [x]) 
    splitBy' items [] []
let isfn = function  
        | Function(_) -> true
        | Struct(_) -> false
let fn = function
        | Function(x) -> x
        | _ -> System.Exception() |> raise
let strct = function
        | Struct(x) -> x
        | _ -> System.Exception() |> raise
let pimport = word "import" >>. identifier 
let pprogram = 
    many ((pstruct |>> StructDefinition) <|> (pfunction |>> FunctionDefinition) <|> (pimport |>> ImportDefinition))



//|> result (fun (expr, memb) -> MemberAccess(expr, memb)) 
opp.TermParser <- expression
opp.AddOperator(InfixOperator("&&", spaces, 1, Associativity.Left, binary Op.And)) 
opp.AddOperator(InfixOperator("||", spaces, 1, Associativity.Left, binary Op.Or)) 
opp.AddOperator(InfixOperator("<", spaces, 2, Associativity.Left, binary Op.LT)) 
opp.AddOperator(InfixOperator("<=", spaces, 2, Associativity.Left, binary Op.LTEQ)) 
opp.AddOperator(InfixOperator(">", spaces, 2, Associativity.Left, binary Op.GT)) 
opp.AddOperator(InfixOperator(">=", spaces, 2, Associativity.Left, binary Op.GTEQ)) 
opp.AddOperator(InfixOperator("==", spaces, 3, Associativity.Left, binary Op.EQEQ)) 
opp.AddOperator(InfixOperator("!=", spaces, 3, Associativity.Left, binary Op.NEQ)) 
opp.AddOperator(InfixOperator("+", spaces, 4, Associativity.Left, binary Op.Plus)) 
opp.AddOperator(InfixOperator("-", spaces, 4, Associativity.Left, binary Op.Minus))
opp.AddOperator(InfixOperator("*", spaces, 5, Associativity.Left, binary Op.Star))
opp.AddOperator(InfixOperator("/", spaces, 5, Associativity.Left, binary Op.Slash))
opp.AddOperator(PrefixOperator("-", spaces, 6, true, Negate))
opp.AddOperator(PrefixOperator("!", spaces, 6, true, Not))
