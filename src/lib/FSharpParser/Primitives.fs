namespace FSharpParser

module Primitives =
    open System
    open FSharpParser.Helper
    open FSharpParser.Parser

    (* Primitives *)
    let anyChar = toParser (fun input ->
        if input.Length > 0 then
            [(input.[0], input.Substring(1))]
        else
            []
    )

    let maybe (p: 'a Parser) = toParser (fun input ->
        match p input with
        | [] -> [(None, input)]
        | results -> results |> List.map (fun (v, out) -> (Some v, out))
    )

    let satisfy p = parse {
        let! c = anyChar
        if p c then return c
    }

    let rec satisfyMany p = parse {
        let! head = anyChar

        if p head then
            match! maybe (satisfyMany p) with
                        | None -> return string head
                        | Some rest -> return String.Concat(head, rest)
    }

    let satisfyManyOrNone p = 
        maybe (satisfyMany p) |>> function
        | None -> ""
        | Some str -> str

    let rec many p = parse {
        let! head = p

        match! maybe (many p) with
                | None -> return [head]
                | Some tail -> return head :: tail
    }

    let pChar c = satisfy (eq c)

    let pDigit = satisfy Char.IsDigit
    let pLetter = satisfy Char.IsLetter

    let pUpper = satisfy Char.IsUpper
    let pLower = satisfy Char.IsLower

    let pSpace = satisfyManyOrNone Char.IsWhiteSpace
    let pSpaced p = pSpace >>. p .>> pSpace

    let pInt = satisfyMany Char.IsDigit
    let pWord = satisfyMany (Char.IsWhiteSpace >> not)
