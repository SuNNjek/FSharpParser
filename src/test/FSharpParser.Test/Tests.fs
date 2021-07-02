namespace FSharpParser.Test

open System
open NUnit.Framework
open FsUnit
open FSharpParser.Parser
open FSharpParser.Primitives

[<TestFixture>]
type TestClass () =
    [<Test>]
    member _.TestParseInt () =
        let results = (pInt |>> Int32.Parse) "42"
        results |> should haveLength 1

        let result, rest = results.[0]
        result |> should equal 42
        rest |> should haveLength 0

    [<Test>]
    member _.TestParseWords () =
        let results = "Hello World!" |> many (pSpaced pWord)
        results |> should haveLength 1

        let result, rest = results.[0]
        result |> should haveLength 2
        rest |> should haveLength 0

        result.[0] |> should equal "Hello"
        result.[1] |> should equal "World!"

