namespace FSharpParser

module Helper =
    let flatten<'a> = List.collect<'a list, 'a> id
