namespace FSharpParser

module Parser =
    open System
    open FSharpParser.Helper

    type ParserResult<'a> = ('a * string)
    type Parser<'a> = string -> ParserResult<'a> list

    let toParser (f: string -> ParserResult<'a> list): 'a Parser = f

    (* Operators *)
    let (>>=) (p: 'a Parser) (f: 'a -> 'b Parser): 'b Parser = toParser (fun input ->
        match p input with
        | [] -> []
        | results -> results |> List.map (fun (v, out) -> (f v) out) |> flatten
    )

    let (<|>) (p1: 'a Parser) (p2: 'a Parser) = toParser (fun input ->
        match p1 input with
        | [] -> p2 input
        | results -> results
    )

    let (>>%) (p: 'a Parser) (v: 'b) = p >>= (fun _ ->
        toParser (fun i -> [(v, i)])
    )

    let (|>>) (p: 'a Parser) (f: 'a -> 'b) = p >>= (fun v ->
        toParser (fun i -> [(f v, i)])
    )

    let (.>>) (p1: 'a Parser) (p2: 'b Parser) = p1 >>= (fun res ->
        p2 >>= (fun _ ->
            toParser (fun i -> [(res, i)])
        )
    )

    let (>>.) (p1: 'a Parser) (p2: 'b Parser) = p1 >>= (fun _ ->
        p2 >>= (fun res ->
            toParser (fun i -> [(res, i)])
        )
    )


    let parseValue v = toParser (fun input -> [(v, input)])
    let parseEmpty<'a> = parseValue<'a list> []
    let parseEmptyString = parseValue ""
    let parseNothing<'a> = toParser<'a> (fun _ -> [])

    (* Builder *)
    type ParseBuilder() =
        member _.Bind(p, f) = p >>= f

        member _.Return(v) = parseValue v
        member _.ReturnFrom(p: 'a Parser) = p
        member _.Zero() = parseNothing

    let parse = ParseBuilder()

    (* Primitives *)
    let anyChar = toParser (fun input ->
        if input.Length > 0 then
            [(input.[0], input.Substring(1))]
        else
            []
    )

    let satisfy p = parse {
        let! c = anyChar
        if p c then return c
    }

    let rec satisfyMany p = parse {
        let! c = anyChar

        if p c then
            let! rest = satisfyMany p <|> parseEmptyString
            return String.Concat(c, rest)
    }

    let rec satisfyManyOrNone p = satisfyMany p <|> parseEmptyString

    let rec many p = parse {
        let! head = p
        let! tail = many p <|> parseEmpty

        return head :: tail
    }

    let pchar c = satisfy (fun v -> v.Equals(c))

    let pdigit = satisfy Char.IsDigit
    let pletter = satisfy Char.IsLetter

    let pupper = satisfy Char.IsUpper
    let plower = satisfy Char.IsLower

    let pspace = satisfyManyOrNone Char.IsWhiteSpace
    let pspaced p = pspace >>. p .>> pspace

    let pint = satisfyMany Char.IsDigit
    let pword = satisfyMany (Char.IsWhiteSpace >> not)
