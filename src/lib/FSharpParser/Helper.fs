namespace FSharpParser

module Helper =
    let flatten<'a> = List.collect<'a list, 'a> id

    let eq a b = a.Equals(b)
