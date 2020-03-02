namespace FSharpTest.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FSharpParser.Parser
open FSharpParser.Primitives

[<TestClass>]
type TestClass () =
    [<TestMethod>]
    member _.TestParseInt () =
        let results = (pInt |>> Int32.Parse) "42"
        Assert.AreEqual(1, results.Length)

        let result, rest = results.[0]
        Assert.AreEqual(42, result)
        Assert.AreEqual(0, rest.Length)

    [<TestMethod>]
    member _.TestParseWords () =
        let results = "Hello World!" |> many (pSpaced pWord)
        Assert.AreEqual(1, results.Length)

        let result, rest = results.[0]
        Assert.AreEqual(2, result.Length)
        Assert.AreEqual(0, rest.Length)

        Assert.AreEqual("Hello", result.[0])
        Assert.AreEqual("World!", result.[1])

