open FParsec
open Parser
open ContextBuilder
open Compiler

let success = function
| Success(s, _, _) -> s
| e ->
    printfn "%A" e
    System.Exception() |> raise
let program = 
    runParserOnFile pprogram () "test.pl" System.Text.Encoding.UTF8 
    |> success
let context = buildContext program |> Result.toOption |> Option.get
let result = 
    compile "output" context program 
    |> String.concat "\n"
printfn "%s" result


