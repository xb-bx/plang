module ModuleReader
open FSharp.Core
open FSharpPlus
open System.IO
open Context

type Reader = { stream: Stream } with 
    member this.read() =
        match this.stream.ReadByte() with
        | -1 -> None
        |  b -> Some(byte b)
    static member fromStream stream = { stream = stream }

let (||>) f = Option.bind f
let (>|>) (r1: Result<'a, 'b>) (f: 'a -> Result<'c, 'b>) =
    match r1 with
    | Ok(v) -> f v
    | Error(err) -> Error(err)
    
        
let readString (reader: Reader) =
    let rec readString' (reader: Reader) str =
        match reader.read() with
        | Some(0uy) -> str |> List.toArray |> System.Text.Encoding.ASCII.GetString |> Ok
        | Some(c) -> readString' reader (str @ [byte c])
        | None -> Error "Unexpected EOF" 
    readString' reader []
let readU32 (reader: Reader) =
    let someUint32 (b: byte) = b |> uint32 |> Some
    
    monad {
        let! b1 = reader.read() |> Option.bind someUint32
        let! b2 = reader.read() |> Option.bind someUint32
        let! b3 = reader.read() |> Option.bind someUint32
        let! b4 = reader.read() |> Option.bind someUint32
        return b1 ||| (b2 <<< 8) ||| (b3 <<< 16) ||| (b4 <<< 24)
    } |> toResult "Unexpected EOF"
let openFile fileName = 
    if File.Exists fileName then 
        fileName |> File.OpenRead |> Reader.fromStream |> Ok
    else 
        fileName |> sprintf "Could not oppen file %s" |> Error
let range start endd = 
    let mutable i = 0
    seq {
        while i < endd do
            yield i
            i <- i + 1

    }
let setExt ext file = 
    match file with
    | f when f |> String.endsWith ext -> f
    | _ -> file + ext
let unfinished name = 
    { name = name; imports = []; typeImports = []; types = []; functions = []; functionImports = []}
let replace (name: string) (modl: Module) (modules: Module list) =
    let rec replace' name modl modules res = 
        match modules with
        | [] -> res
        | x :: rest when x.name = name -> replace' name modl rest (res @ [modl])
        | x :: rest -> replace' name modl rest (res @ [x])
    replace' name modl modules []
let foldResults (f: Context -> int -> Result<Module * Context, string>) (ctx: Context) (results: int list): Result<Module list * Context, string> =
    let mutable ctx = ctx
    let mutable output = []
    let mutable e = Error ""
    let mutable err = false
    for result in results do 
        if err then 
            ()
        else 
            match f ctx result with
            | Ok((res, newctx)) -> 
                output <- output @ [res]
                ctx <- newctx
            | Error(er) -> 
                e <- Error er
                err <- true
    if err then
        e
    else 
        Ok (output, ctx)
let asCustom (x: Type) = 
    match x with 
        | CustomType c  -> c 
        | _ -> System.Exception() |> raise
let custom typeName fields =
    { name = typeName; fields = fields; parent = None }
let isCustom = function
            | CustomType(_) -> true
            | _ -> false
let importType (reader: Reader) (imports: Module list) =
    monad {
        let! moduleN = readU32 reader |> Result.bind (int >> Ok)
        let! modl = imports |> List.tryItem moduleN |> toResult "Module not found"
        let! typeName = readString reader
        let! typ = modl.types |> List.filter isCustom |> List.tryFind(fun x -> (x |> asCustom).name = typeName) |> toResult (sprintf "Type %s not found" typeName) 
        return typ
    }
let defineType reader =
    monad {
        let! typeName = readString reader
        return CustomType(custom (typeName) Map.empty)
    }
let rec parseDescriptor (reader: Reader) (typeImports: Type list) (types: Type list): Result<Type, string> =
    monad {
        let! typeKind = reader.read() |> toResult "Unexpected EOF"
        let typeKind = int typeKind
        let res = 
            match typeKind with
            | 0 ->
                monad {
                    let! primitive = reader.read() |> toResult "Unexpected EOF"
                    let primitive = int primitive
                    let voidValue = LanguagePrimitives.EnumToValue BuiltinType.Void
                    let res = 
                        if primitive > voidValue then
                            Error("FUCK")
                        else 
                            let builtin: BuiltinType = (LanguagePrimitives.EnumOfValue primitive)
                            Builtin(builtin) |> Ok
                    let! res = res
                    return res
                }
            | 1 -> 
                monad {
                    let! under = parseDescriptor reader typeImports types
                    let! under = 
                        match under with
                        | CustomType(_) -> Boxed(under) |> Ok
                        | Builtin(b) -> Boxed(under) |> Ok
                        | _ -> "Error" |> Error
                    return under
                }
            | 2 -> 
                monad {
                    let! under = parseDescriptor reader typeImports types
                    return Array(under)
                }
            | 3 -> 
                monad {
                    let! under = parseDescriptor reader typeImports types
                    return Ref(under)
                }
            | 4 -> 
                monad {
                    let! index = readU32 reader |> Result.bind (int >> Ok)
                    let! typ = 
                        (typeImports |> List.tryItem index) 
                            <|> (types |> List.tryItem (index - (typeImports |> List.length))) 
                            |> toResult (sprintf "Unknown type %i in descriptor" index)
                    return typ
                }
            | _ -> 
                monad {
                    let! b = Error "Unknown kind of type descriptor"
                    return Builtin(BuiltinType.Void)
                }
        return! res
    }
let addFields reader types typeDescriptors = 
    monad {
        let! typeIndex = readU32 reader |> Result.bind (int >> Ok)
        let! typ = types |> List.tryItem typeIndex |> toResult "Unknown type"
        let! fieldName = readString reader
        let! typeDescriptor = readU32 reader |> Result.bind (int >> Ok)
        let! typeDescriptor = 
            typeDescriptors 
                |> List.tryItem typeDescriptor 
                |> toResult "Unknown type descriptor"
        let cust = asCustom typ
        cust.fields <- 
            cust.fields |> Map.add fieldName typeDescriptor
        return ()
        
    }
let parseFnImport reader (imports: Module list) =
    monad {
        let! modIndex = readU32 reader |> Result.bind (int >> Ok)
        let! modul = imports |> List.tryItem modIndex |> toResult "Unknown module"
        let! functionName = readString reader
        let! func = modul.functions |> List.tryFind (fun x -> x.name = functionName) |> toResult "Unknown function"
        return func
    }
let getDescriptor reader typeDescriptors: Result<Type, string> =
    monad {
        let index = readU32 reader |> Result.bind (int >> Ok)
        let! index = index
        let! typ = typeDescriptors |> List.tryItem index |> toResult "Unknown descriptor"
        return typ
    }
let parseFn reader typeDescriptors: Result<Function, string> = 
    monad {
        let! fnname = readString reader
        let! argc = readU32 reader |> Result.bind(int >> Ok)
        let args = 
            range 0 argc
                |> List.ofSeq
                |> List.map (fun _ -> getDescriptor reader typeDescriptors)
                |> sequence
        let! args = args
        let! localc = readU32 reader |> Result.bind(int >> Ok)
        for i in 0..(localc - 1) do
            let _ = readU32 reader
            ()
        let! retType = getDescriptor reader typeDescriptors
        let! b = readU32 reader
        let! b = readU32 reader 
        let f = { name = fnname; args = args; retType = retType}
        return f
    }
let rec readModule context reader =
    monad {
        let! name = readString reader
        let context = { context with modules = context.modules @ [ unfinished name ]}
        let! importCount =  readU32 reader |> Result.bind (int >> Ok)
        let! (imports, context) = 
            range 0 importCount 
            |> List.ofSeq
            |> foldResults (fun ctx _ -> readStringNameAndLoadModule ctx reader) context
        let! typeImportsCount = readU32 reader |> Result.bind (int >> Ok)
        let! typeImports = 
            range 0 typeImportsCount
            |> List.ofSeq
            |> List.map (fun _ -> importType reader imports)
            |> sequence
        let! typeDefCount = readU32 reader |> Result.bind (int >> Ok)
        let! types = 
            range 0 typeDefCount
            |> List.ofSeq
            |> List.map (fun _ -> defineType reader)
            |> sequence
        let! typeDescCount = readU32 reader |> Result.bind (int >> Ok)
        let! typeDescriptors = 
            range 0 typeDescCount
            |> List.ofSeq
            |> List.map (fun _ -> parseDescriptor reader typeImports types)
            |> sequence
        let descs = typeDescriptors
        descs |> List.iter (printfn "%A")
        let! fieldCount = readU32 reader |> Result.bind (int >> Ok) 

        let res = 
            range 0 fieldCount 
            |> List.ofSeq
            |> List.map (fun _ -> addFields reader types typeDescriptors)

        let r = res |> sequence
        let! r = r
        
        let! fnImportCount = readU32 reader |> Result.bind (int >> Ok)
        let! fnImports =
            range 0 fnImportCount 
            |> List.ofSeq
            |> List.map (fun _ -> parseFnImport reader imports)
            |> sequence
        let! fnDefCount = readU32 reader |> Result.bind (int >> Ok)
        let fnDefs =
            range 0 fnDefCount
            |> List.ofSeq
            |> List.map (fun _ -> parseFn reader typeDescriptors)
            |> sequence
        let! fnDefs = fnDefs

        
        let res = { name = name; imports = imports; typeImports = typeImports; types = types; functions = fnDefs; functionImports = fnImports }
        types 
            |> List.iter (fun x -> (asCustom x).parent <- Some res)
        return res, { context with modules =  replace name res context.modules } 
    }
and loadOrFindModule context modl =
    match  context.modules |> List.tryFind (fun x -> x.name = modl) with
    | Some(m) -> Ok(m, context)
    | _ -> readModuleFromFile context (Path.Combine(context.baseDirectory, (setExt ".mod" modl)))
and readStringNameAndLoadModule(context: Context) (reader:Reader) = 
    monad {
        let! modname = readString reader
        return! loadOrFindModule context modname 
    }
and readModuleFromFile context fileName: Result<Module * Context, string> =
    fileName |> openFile >|> readModule context


let readModuleFromStream context stream =
    stream |> Reader.fromStream |> readModule context
