module Morphir.TypeScript.AST exposing
    ( TypeDef(..), TypeExp(..)
    , CompilationUnit, NamespacePath, ObjectExp, Privacy(..)
    )

{-| This module contains the TypeScript AST (Abstract Syntax Tree). The purpose of this AST is to make it easier to
generate valid TypeScript source code and to separate the language syntax from low-level formatting concerns. We use
this AST as the output of the TypeScript backend and also as the input of the pretty-printer that turns it into the
final text representation.

The AST is maintained manually and it does not have to cover the whole language. We focus on the parts of the language
that we use in the backend.

@docs TypeDef, TypeExp, FieldDef

-}

import Morphir.IR.FQName exposing (FQName)
import Morphir.IR.Name exposing (Name)
import Morphir.IR.Path exposing (Path)


{-| -}
type alias CompilationUnit =
    { dirPath : List String
    , fileName : String
    , imports : List NamespacePath
    , typeDefs : List TypeDef
    }


{-| Represents either a public or a private entity
-}
type Privacy
    = Public
    | Private


{-| (packagePath, modulePath).

Represents the path to a module. Used in various ways to produce either a path
to a module file, or a reference to that module's namespace. (eg in imports)

This has two components, the package path and the module path.
(Note: this is different from a Morphir Fully Qualified Name, which has three
components: package path, module path AND a local name).

-}
type alias NamespacePath =
    ( Path, Path )


{-| Represents a type definition.
-}
type TypeDef
    = Namespace
        { name : Name
        , privacy : Privacy
        , content : List TypeDef
        }
    | TypeAlias
        { name : Name
        , doc : String
        , privacy : Privacy
        , variables : List TypeExp
        , typeExpression : TypeExp
        }
    | Interface
        { name : Name
        , privacy : Privacy
        , variables : List TypeExp
        , fields : ObjectExp
        }
    | ImportAlias
        { name : Name
        , privacy : Privacy
        , namespacePath : NamespacePath
        }


{-| A type expression represents the right-hand side of a type annotation or a type alias.

The structure follows the documentation here:
<https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#the-primitives-string-number-and-boolean>

Only a small subset of the type-system is currently implemented.

-}
type TypeExp
    = Any
    | Boolean
    | List TypeExp {- Represents a Morphir 'List' type, as a Typescript 'Array' type -}
    | LiteralString String
    | Number
    | Object ObjectExp
    | String
    | Tuple (List TypeExp)
    | TypeRef FQName (List TypeExp)
    | Union (List TypeExp)
    | Variable String
    | UnhandledType String


{-| Represents an object expression (or interface definition) as a list of name-and-type pairs.
-}
type alias ObjectExp =
    List ( String, TypeExp )