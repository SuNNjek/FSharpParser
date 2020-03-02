namespace FSharpParser

module Primitives =
    open System
    open FSharpParser.Parser

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

    let satisfyManyOrNone p = satisfyMany p <|> parseEmptyString

    let rec many p = parse {
        let! head = p
        let! tail = many p <|> parseEmpty

        return head :: tail
    }

    let pChar c = satisfy (fun v -> v.Equals(c))

    let pDigit = satisfy Char.IsDigit
    let pLetter = satisfy Char.IsLetter

    let pUpper = satisfy Char.IsUpper
    let pLower = satisfy Char.IsLower

    let pSpace = satisfyManyOrNone Char.IsWhiteSpace
    let pSpaced p = pSpace >>. p .>> pSpace

    let pInt = satisfyMany Char.IsDigit
    let pWord = satisfyMany (Char.IsWhiteSpace >> not)
