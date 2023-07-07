module ContextBuilder
open Parser
open Context
open ModuleReader
open FSharpPlus

let asImport = function
| ImportDefinition(name) -> Some name
| _ -> None
let asStruct = function
| StructDefinition(s) -> Some s
| _ -> None
let asFunction = function
| FunctionDefinition(f) -> Some(f)
| _ -> None
let foldResult mapping state source = 
    let rec mapFoldResult' state source =
        match source with
        | [] -> Ok(state)
        | x :: rest -> 
            match mapping state x with
            | Ok(state) -> mapFoldResult' state rest
            | Error(e) -> Error(e)
    mapFoldResult' state source
let loadOrFindModuleIgnore context modl =
    monad {
        let! (_, context) = loadOrFindModule context modl
        return context
    }
let builtins = 
    System.Enum.GetValues<BuiltinType>()
    |> List.ofArray
    |> List.zip (System.Enum.GetNames<BuiltinType>() |> List.ofArray |> List.map String.toLower)
    |> Map.ofList
let tryGetBuiltin name = 
    builtins
        |> Map.tryFind name
        |> Option.bind (Builtin >> Some)

let rec getType context typeExpression: Result<Type, string> =
    match typeExpression with
    | SimpleType(None, typ) ->
        tryGetBuiltin typ <|> (context.types |> List.tryFind (fun x -> (x |> asCustom).name = typ))
        |> toResult (sprintf "Undefined type %s" typ)
    | SimpleType(Some(modname), typ) ->
        monad {
            let! modl = context.modules |> List.tryFind (fun x -> x.name = modname) |> toResult (sprintf "Unknown module %s" modname)
            let! typ = 
                modl.types 
                |> List.filter isCustom 
                |> List.tryFind (fun x -> (x |> asCustom).name = typ) 
                |> toResult (sprintf "Module %s does not contains type %s" modname typ)
            return typ
        }
    | ArrayType(typ) -> 
        monad {
            let! t = getType context typ
            return Array(t)
        }
    | BoxedType(typ) -> 
        monad {
            let! t = getType context (SimpleType(None, typ))
            return Boxed(t)
        }
    | ReferenceType(typ) -> 
        monad {
            let! t = getType context typ
            return Ref(t)
        }

let mapIterResult f (m: ('K * 'V) list) =
    let rec mapIterResult' (m: ('K * 'V) list) =
        match m with
        | [] -> Ok(())
        | x :: rest ->
            match f (fst x) (snd x) with
            | Ok(_) -> mapIterResult' rest
            | e -> e
    mapIterResult' m
let fillStruct context (str: Struct) = 
    monad {
        let! typ = context.types |> List.tryFind (fun x -> (x |> asCustom).name = str.name) |> toResult "Type not found" |> Result.bind (asCustom >> Ok)
        let! _ = 
            str.fields 
                |> List.map (fun x -> (x.name, x.typ))
                |> mapIterResult (
                    fun name ftype -> 
                        monad {
                            let! t = getType context ftype
                            typ.fields <- typ.fields |> Map.add name t
                            return ()
                        }
                )
        return ()
    }
let createFunction context (func: Parser.Function) = 
    monad {
        let args = 
            func.args
                |> List.map (fun x -> x.typ)
                |> List.map (getType context)
                |> sequence
        let! args = args
        let! ret = func.retType |> getType context

        return { name = func.name; args = args; retType = ret }
    }
        
let buildContext definitions =
    monad {
        let context = emptyContext
        let! context = 
            definitions 
            |> List.map asImport 
            |> List.filter Option.isSome 
            |> List.map Option.get
            |> foldResult loadOrFindModuleIgnore context
        let structs = 
            definitions
            |> List.map asStruct
            |> List.filter Option.isSome
            |> List.map Option.get
        let types = 
            structs
            |> List.map (fun x -> CustomType { name = x.name; fields = Map.empty; parent = None })
        let context = { context with types = types  }
        let r = 
            structs 
            |> List.map (fillStruct context)
            |> sequence
        let! _ = r 

        let functions =
            definitions
            |> List.map asFunction
            |> List.filter Option.isSome
            |> List.map Option.get
            |> List.map (createFunction context)
            |> sequence
        let! functions = functions
        return { context with functions = functions }
    }

