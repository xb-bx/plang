module Compiler
open FSharpPlus
open ModuleReader
open Context
open Parser


let rec compileType (typ: Context.Type) = 
    match typ with
    | Builtin(b) -> b.ToString() |> String.toLower
    | Array(t) -> "$" + (compileType t)
    | Ref(t) -> "&" + (compileType t)
    | Boxed(t) -> "*" + (compileType t)
    | CustomType(custom) -> custom.name
let compileCustomType lines (typ: CustomType) = 
    let lines = lines @ [(sprintf "type %s" typ.name)]
    let fields = (typ.fields |> Map.toList |> List.map (fun (name,typ) -> (sprintf "%s %s" name (compileType typ))))
    let lines = lines @ fields @  ["end"]
    lines

    
    

    
    
    
let compile moduleName context program =
    printfn "%A" context
    let lines = [sprintf "module %s" moduleName]
    context.types 
        |> List.map asCustom
        |> List.fold compileCustomType lines


