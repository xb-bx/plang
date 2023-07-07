module Context

type BuiltinType = 
    | I64 = 0
    | U64 = 1
    | I32 = 2
    | U32 = 3
    | I16 = 4
    | U16 = 5
    | I8  = 6
    | U8  = 7
    | F64 = 8
    | F32 = 9
    | Char= 10
    | Any = 11
    | String  = 12
    | Boolean = 13
    | Void = 14

type CustomType = { name: string; mutable parent: Module option; mutable fields: Map<string, Type> }
and Function = { name: string; args: Type list; retType: Type }
and Module = { name: string; imports: Module list; typeImports: Type list;  types: Type list; functionImports: Function list; functions: Function list }
and Type = 
| Builtin of typ: BuiltinType
| Ref of underlaying: Type
| Boxed of underlaying: Type 
| Array of underlaying: Type
| CustomType of CustomType

type Context = { modules: Module list; types: Type list; functions: Function list; baseDirectory: string }
let emptyContext = { modules = []; types = []; functions = []; baseDirectory = "."}
let toResult error x =
    match x with
    | Some(v) -> Ok v
    | None -> Error error
