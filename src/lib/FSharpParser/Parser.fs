namespace FSharpParser

module Parser =
    open FSharpParser.Helper

    type ParserResult<'a> = ('a * string)
    type Parser<'a> = string -> ParserResult<'a> list

    let toParser (f: string -> ParserResult<'a> list): 'a Parser = f

    let parseValue v = toParser (fun input -> [(v, input)])
    let parseEmpty<'a> = parseValue<'a list> []
    let parseEmptyString = parseValue ""
    let parseNothing<'a> = toParser<'a> (fun _ -> [])

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

    let (>>%) (p: 'a Parser) (v: 'b) = p >>= (fun _ -> parseValue v)

    let (|>>) (p: 'a Parser) (f: 'a -> 'b) =
        p >>= (fun v ->
            toParser (fun i -> [(f v, i)])
        )

    let (.>>) (p1: 'a Parser) (p2: 'b Parser) =
        p1 >>= (fun res ->
            p2 >>= (fun _ -> parseValue res )
        )

    let (>>.) (p1: 'a Parser) (p2: 'b Parser) =
        p1 >>= (fun _ ->
            p2 >>= parseValue
        )


    (* Builder for computation expressions *)
    type ParseBuilder() =
        member _.Bind(p, f) = p >>= f
        member _.Delay(f: unit -> 'a Parser) = f ()

        member _.Return(v) = parseValue v
        member _.ReturnFrom(p: 'a Parser) = p
        member _.Zero() = parseNothing

    let parse = ParseBuilder()
