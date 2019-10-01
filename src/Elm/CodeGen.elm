module Elm.CodeGen exposing
    ( file
    , normalModule, portModule
    , exposeAll, exposeExplicit
    , closedTypeExpose, funExpose, openTypeExpose, typeOrAliasExpose
    , importStmt
    , Linkage, addExposing, addImport, combineLinkage, emptyLinkage
    , aliasDecl, customTypeDecl, funDecl, patternDecl, portDecl
    , access, accessFun, apply, caseExpr, chain, char, float, fqFun, fqVal, fun, glsl, hex
    , ifExpr, int, lambda, letExpr, list, negate, op, opApply, parens, pipe, prefixOp, string
    , tuple, unit, update, val
    , allPattern, asPattern, charPattern, floatPattern, fqNamedPattern, hexPattern, intPattern
    , listPattern, namedPattern, parensPattern, recordPattern, stringPattern, tuplePattern, unConsPattern
    , unitPattern, varPattern
    , extRecordAnn, fqTyped, funAnn, recordAnn, tupleAnn, typeVar, typed, unitAnn
    , boolAnn, intAnn, floatAnn, stringAnn, charAnn
    , listAnn, setAnn, dictAnn, maybeAnn
    , signature
    , ModuleName, Module, File, Declaration, Import, TypeAnnotation
    , Exposing, TopLevelExpose, Expression, Pattern
    )

{-| Elm.CodeGen is a DSL designed to make it easier to write Elm code that generates Elm code.


# Build an Elm source file.

@docs file


# Build a module declaration.

`elm-syntax` also permits effects modules, but with kernel code restrictions you cannot use these so
they have been ommitted from the DSL.

@docs normalModule, portModule


# Build an 'exposing' statement.

@docs exposeAll, exposeExplicit
@docs closedTypeExpose, funExpose, openTypeExpose, typeOrAliasExpose


# Build an import statement.

@docs importStmt


# Incrementally build import and exposing statements.

This is useful during code generation where the exact imports and exports are not known in advance
but depend on what code is actually generated. Each section of code generation can declare the imports and
exposings that it needs and they can be combined and de-duplicated to produce a final list.

I have used the name `Linkage` to refer to the combination of a modules imports and exports. It describes
how a module is linked to other modules.

@docs Linkage, addExposing, addImport, combineLinkage, emptyLinkage


# Build top-level declarations.

@docs aliasDecl, customTypeDecl, funDecl, patternDecl, portDecl


# Build an expression.

@docs access, accessFun, apply, caseExpr, chain, char, float, fqFun, fqVal, fun, glsl, hex
@docs ifExpr, int, lambda, letExpr, list, negate, op, opApply, parens, pipe, prefixOp, string
@docs tuple, unit, update, val


# Build a pattern matching expression.

@docs allPattern, asPattern, charPattern, floatPattern, fqNamedPattern, hexPattern, intPattern
@docs listPattern, namedPattern, parensPattern, recordPattern, stringPattern, tuplePattern, unConsPattern
@docs unitPattern, varPattern


# Build a type annotation.

@docs extRecordAnn, fqTyped, funAnn, recordAnn, tupleAnn, typeVar, typed, unitAnn
@docs boolAnn, intAnn, floatAnn, stringAnn, charAnn
@docs listAnn, setAnn, dictAnn, maybeAnn


# Build a type signature.

@docs signature


# Types describing parts of the Elm AST.

These types are all declared in `elm-syntax` but are re-exported here for convenience.

@docs ModuleName, Module, File, Declaration, Import, TypeAnnotation
@docs Exposing, TopLevelExpose, Expression, Pattern

-}

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Case, CaseBlock, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))
import Elm.Syntax.Module exposing (DefaultModuleData, EffectModuleData, Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Elm.Syntax.Range exposing (Location, Range, emptyRange)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation(..))



--== Dynamic import and export lists.


{-| Linkage describes the lists of imports and exposings of a module and allows
these to be built up incrementally as code generation progresses.
-}
type alias Linkage =
    ( List Import, List TopLevelExpose )


{-| Simplifies the imports and exposings by removing any duplicates.
-}
combineLinkage : List Linkage -> Linkage
combineLinkage importsAndExposings =
    let
        ( imports, exposings ) =
            List.unzip importsAndExposings
    in
    ( List.concat imports
    , List.concat exposings
    )


{-| Creates empty imports and exposings lists.
-}
emptyLinkage : Linkage
emptyLinkage =
    ( [], [] )


{-| Adds an import to the list.
-}
addImport : Import -> Linkage -> Linkage
addImport imp iande =
    Tuple.mapFirst ((::) imp)
        iande


{-| Adds an exposing to the list.
-}
addExposing : TopLevelExpose -> Linkage -> Linkage
addExposing tlExpose iande =
    Tuple.mapSecond ((::) tlExpose)
        iande



--== Re-Export of Types


{-| A module name can consist of mulitple Stirngs separated with '.'.
-}
type alias ModuleName =
    List String


{-| The AST for an Elm module.
-}
type alias Module =
    Elm.Syntax.Module.Module


{-| The AST for an Elm file.
-}
type alias File =
    Elm.Syntax.File.File


{-| The AST for a top-level Elm declaration; a function, a value, a type or a
type alias.
-}
type alias Declaration =
    Elm.Syntax.Declaration.Declaration


{-| The AST for an Elm import statement.
-}
type alias Import =
    Elm.Syntax.Import.Import


{-| The AST for an Elm type annotation.
-}
type alias TypeAnnotation =
    Elm.Syntax.TypeAnnotation.TypeAnnotation


{-| The AST for an Elm exposing statement.
-}
type alias Exposing =
    Elm.Syntax.Exposing.Exposing


{-| The AST for a member of an Elm exposing statement.
-}
type alias TopLevelExpose =
    Elm.Syntax.Exposing.TopLevelExpose


{-| The AST for an Elm expression.
-}
type alias Expression =
    Elm.Syntax.Expression.Expression


{-| The AST for a de-structuring Elm pattern matching expression.
-}
type alias Pattern =
    Elm.Syntax.Pattern.Pattern



--== Elm.Syntax.Declaration


{-| FunctionDeclaration Function
-}
funDecl : Maybe String -> Maybe Signature -> String -> List Pattern -> Expression -> Declaration
funDecl docs sig name args expr =
    functionImplementation name args expr
        |> function docs sig
        |> FunctionDeclaration


{-| AliasDeclaration TypeAlias
-}
aliasDecl : Maybe String -> String -> List String -> TypeAnnotation -> Declaration
aliasDecl docs name args annotation =
    typeAlias docs name args annotation
        |> AliasDeclaration


{-| CustomTypeDeclaration Type
-}
customTypeDecl : Maybe String -> String -> List String -> List ( String, List TypeAnnotation ) -> Declaration
customTypeDecl docs name args constructors =
    customType docs name args constructors
        |> CustomTypeDeclaration


{-| PortDeclaration Signature
-}
portDecl : String -> TypeAnnotation -> Declaration
portDecl name annotation =
    signature name annotation
        |> PortDeclaration


{-| Destructuring (Node Pattern) (Node Expression)
-}
patternDecl : Pattern -> Expression -> Declaration
patternDecl pattern expr =
    Destructuring (nodify pattern) (nodify expr)


infixDecl : InfixDirection -> Int -> String -> String -> Declaration
infixDecl direction precedence symbol fn =
    infix_ direction precedence symbol fn
        |> InfixDeclaration



--== Elm.Syntax.Exposing


{-| All Range
-}
exposeAll : Exposing
exposeAll =
    All emptyRange


{-| Explicit (List (Node TopLevelExpose))
-}
exposeExplicit : List TopLevelExpose -> Exposing
exposeExplicit topLevelExposes =
    Explicit (nodifyAll topLevelExposes)


infixExpose : String -> TopLevelExpose
infixExpose sym =
    InfixExpose sym


{-| FunctionExpose String
-}
funExpose : String -> TopLevelExpose
funExpose fn =
    FunctionExpose fn


{-| TypeOrAliasExpose String
-}
typeOrAliasExpose : String -> TopLevelExpose
typeOrAliasExpose name =
    TypeOrAliasExpose name


{-| TypeExpose ExposedType
-}
openTypeExpose : String -> TopLevelExpose
openTypeExpose name =
    { name = name
    , open = Just emptyRange
    }
        |> TypeExpose


{-| TypeExpose ExposedType
-}
closedTypeExpose : String -> TopLevelExpose
closedTypeExpose name =
    { name = name
    , open = Nothing
    }
        |> TypeExpose



--== Elm.Syntax.Expression


{-| Joins multiple expressions together with the pipe operator `|>`. An
expression `a` combined with a list of expressions `[b, c, d]` results in:

    a |> b |> c |> d

-}
pipe : Expression -> List Expression -> Expression
pipe head expressions =
    case expressions of
        [] ->
            head

        [ expr ] ->
            opApply "|>" Left head expr

        expr :: exprs ->
            opApply "|>" Left head (pipe expr exprs)


{-| Joins multiple expressions together with the function chain operator `>>`. An
expression `a` combined with a list of expressions `[b, c, d]` results in:

    a >> b >> c >> d

-}
chain : Expression -> List Expression -> Expression
chain head expressions =
    case expressions of
        [] ->
            head

        [ expr ] ->
            opApply ">>" Left head expr

        expr :: exprs ->
            opApply ">>" Left head (pipe expr exprs)


{-| UnitExpr
-}
unit : Expression
unit =
    UnitExpr


{-| Application (List (Node Expression))
-}
apply : List Expression -> Expression
apply exprs =
    Application (nodifyAll exprs)


{-| OperatorApplication String InfixDirection (Node Expression) (Node Expression)
-}
opApply : String -> InfixDirection -> Expression -> Expression -> Expression
opApply symbol infixDir exprl exprr =
    OperatorApplication symbol infixDir (nodify exprl) (nodify exprr)


{-| FunctionOrValue ModuleName String
-}
fqFun : ModuleName -> String -> Expression
fqFun moduleName name =
    FunctionOrValue moduleName name


{-| FunctionOrValue ModuleName String

Note this is the same as `fqFun`

-}
fqVal : ModuleName -> String -> Expression
fqVal moduleName name =
    FunctionOrValue moduleName name


{-| Creates a FunctionOrValue with no qualifiying module.
-}
fun : String -> Expression
fun name =
    fqFun [] name


{-| Creates a FunctionOrValue with no qualifiying module.

Note this is the same as `fun`.

-}
val : String -> Expression
val name =
    fqVal [] name


{-| IfBlock (Node Expression) (Node Expression) (Node Expression)
-}
ifExpr : Expression -> Expression -> Expression -> Expression
ifExpr boolExpr trueExpr falseExpr =
    IfBlock (nodify boolExpr) (nodify trueExpr) (nodify falseExpr)


{-| PrefixOperator String
-}
prefixOp : String -> Expression
prefixOp symbol =
    PrefixOperator symbol


{-| Operator String
-}
op : String -> Expression
op symbol =
    Operator symbol


{-| Integer Int
-}
int : Int -> Expression
int intVal =
    Integer intVal


{-| Hex Int
-}
hex : Int -> Expression
hex hexVal =
    Hex hexVal


{-| Floatable Float
-}
float : Float -> Expression
float floatVal =
    Floatable floatVal


{-| Negation (Node Expression)
-}
negate : Expression -> Expression
negate expr =
    Negation (nodify expr)


{-| Literal String
-}
string : String -> Expression
string literal =
    Literal literal


{-| CharLiteral Char
-}
char : Char -> Expression
char charVal =
    CharLiteral charVal


{-| TupledExpression (List (Node Expression))
-}
tuple : List Expression -> Expression
tuple exprs =
    TupledExpression (nodifyAll exprs)


{-| ParenthesizedExpression (Node Expression)
-}
parens : Expression -> Expression
parens expr =
    ParenthesizedExpression (nodify expr)


{-| LetExpression LetBlock
-}
letExpr : List LetDeclaration -> Expression -> Expression
letExpr declarations expr =
    letBlock declarations expr
        |> LetExpression


{-| CaseExpression CaseBlock
-}
caseExpr : Expression -> List ( Pattern, Expression ) -> Expression
caseExpr expr cases =
    List.map (\( pat, body ) -> case_ pat body) cases
        |> caseBlock expr
        |> CaseExpression


{-| LambdaExpression Lambda
-}
lambda : List Pattern -> Expression -> Expression
lambda args expr =
    { args = nodifyAll args, expression = nodify expr }
        |> LambdaExpression


{-| RecordExpr (List (Node RecordSetter))
-}
record : List ( String, Expression ) -> Expression
record setters =
    List.map (\( fieldName, expr ) -> recordSetter fieldName expr) setters
        |> nodifyAll
        |> RecordExpr


{-| ListExpr (List (Node Expression))
-}
list : List Expression -> Expression
list exprs =
    ListExpr (nodifyAll exprs)


{-| RecordAccess (Node Expression) (Node String)
-}
access : Expression -> String -> Expression
access expr selector =
    RecordAccess (nodify expr) (nodify selector)


{-| RecordAccessFunction String
-}
accessFun : String -> Expression
accessFun selector =
    RecordAccessFunction selector


{-| RecordUpdateExpression (Node String) (List (Node RecordSetter))
-}
update : String -> List ( String, Expression ) -> Expression
update varName setters =
    List.map (\( fieldName, expr ) -> recordSetter fieldName expr) setters
        |> nodifyAll
        |> RecordUpdateExpression (nodify varName)


{-| GLSLExpression String
-}
glsl : String -> Expression
glsl expr =
    GLSLExpression expr


letBlock : List LetDeclaration -> Expression -> LetBlock
letBlock decls expr =
    { declarations = nodifyAll decls
    , expression = nodify expr
    }


letFunction : Function -> LetDeclaration
letFunction func =
    LetFunction func


letDestructuring : Pattern -> Expression -> LetDeclaration
letDestructuring pattern expr =
    LetDestructuring (nodify pattern) (nodify expr)


recordSetter : String -> Expression -> RecordSetter
recordSetter field expr =
    ( nodify field, nodify expr )


caseBlock : Expression -> List Case -> CaseBlock
caseBlock expr cases =
    { expression = nodify expr
    , cases = cases
    }


case_ : Pattern -> Expression -> Case
case_ pattern expr =
    ( nodify pattern, nodify expr )


function : Maybe String -> Maybe Signature -> FunctionImplementation -> Function
function docs sig decl =
    { documentation = nodifyMaybe docs
    , signature = nodifyMaybe sig
    , declaration = nodify decl
    }


functionImplementation : String -> List Pattern -> Expression -> FunctionImplementation
functionImplementation name args expr =
    { name = nodify name
    , arguments = nodifyAll args
    , expression = nodify expr
    }



--== Elm.Syntax.File


{-| Assembles all the components of an Elm file; the module declaration, the
comments, the imports and the top-level declarations.
-}
file : Module -> List Import -> List Declaration -> List String -> File
file mod imports declarations comments =
    { moduleDefinition = nodify mod
    , imports = nodifyAll imports
    , declarations = nodifyAll declarations
    , comments = nodifyAll comments
    }



--== Elm.Syntax.Import


{-| Creates an Elm import statement; the name of the module, an optional alias
name for the module, and an optional list of exposings from the module.
-}
importStmt : ModuleName -> Maybe ModuleName -> Maybe Exposing -> Import
importStmt modName aliasName exposes =
    { moduleName = nodify modName
    , moduleAlias = nodifyMaybe aliasName
    , exposingList = nodifyMaybe exposes
    }



--== Elm.Syntax.Infix


infix_ : InfixDirection -> Int -> String -> String -> Infix
infix_ direction precedence symbol fn =
    { direction = nodify direction
    , precedence = nodify precedence
    , operator = nodify symbol
    , function = nodify fn
    }


left : InfixDirection
left =
    Left


right : InfixDirection
right =
    Right


non : InfixDirection
non =
    Non



--== Elm.Syntax.Module


{-| NormalModule DefaultModuleData
-}
normalModule : ModuleName -> List TopLevelExpose -> Module
normalModule name exposes =
    NormalModule <| defaultModuleData name (exposing_ exposes)


{-| PortModule DefaultModuleData
-}
portModule : ModuleName -> List TopLevelExpose -> Module
portModule name exposes =
    PortModule <| defaultModuleData name (exposing_ exposes)


effectModule : ModuleName -> List TopLevelExpose -> Maybe String -> Maybe String -> Module
effectModule name exposes cmd sub =
    EffectModule <| effectModuleData name (exposing_ exposes) cmd sub


defaultModuleData : ModuleName -> Exposing -> DefaultModuleData
defaultModuleData name exposes =
    { moduleName = nodify name
    , exposingList = nodify exposes
    }


effectModuleData : ModuleName -> Exposing -> Maybe String -> Maybe String -> EffectModuleData
effectModuleData name exposes cmd sub =
    { moduleName = nodify name
    , exposingList = nodify exposes
    , command = nodifyMaybe cmd
    , subscription = nodifyMaybe sub
    }



--== Elm.Syntax.Pattern


{-| AllPattern
-}
allPattern : Pattern
allPattern =
    AllPattern


{-| UnitPattern
-}
unitPattern : Pattern
unitPattern =
    UnitPattern


{-| CharPattern Char
-}
charPattern : Char -> Pattern
charPattern charVal =
    CharPattern charVal


{-| StringPattern String
-}
stringPattern : String -> Pattern
stringPattern literal =
    StringPattern literal


{-| IntPattern Int
-}
intPattern : Int -> Pattern
intPattern intVal =
    IntPattern intVal


{-| HexPattern Int
-}
hexPattern : Int -> Pattern
hexPattern hexVal =
    HexPattern hexVal


{-| FloatPattern Float
-}
floatPattern : Float -> Pattern
floatPattern floatVal =
    FloatPattern floatVal


{-| TuplePattern (List (Node Pattern))
-}
tuplePattern : List Pattern -> Pattern
tuplePattern patterns =
    TuplePattern (nodifyAll patterns)


{-| RecordPattern (List (Node String))
-}
recordPattern : List String -> Pattern
recordPattern fields =
    RecordPattern (nodifyAll fields)


{-| UnConsPattern (Node Pattern) (Node Pattern)
-}
unConsPattern : Pattern -> Pattern -> Pattern
unConsPattern hd tl =
    UnConsPattern (nodify hd) (nodify tl)


{-| ListPattern (List (Node Pattern))
-}
listPattern : List Pattern -> Pattern
listPattern seq =
    ListPattern (nodifyAll seq)


{-| VarPattern String
-}
varPattern : String -> Pattern
varPattern name =
    VarPattern name


{-| NamedPattern QualifiedNameRef (List (Node Pattern))
-}
namedPattern : String -> List Pattern -> Pattern
namedPattern name patterns =
    NamedPattern { moduleName = [], name = name } (nodifyAll patterns)


{-| NamedPattern QualifiedNameRef (List (Node Pattern))
-}
fqNamedPattern : ModuleName -> String -> List Pattern -> Pattern
fqNamedPattern moduleName name patterns =
    NamedPattern { moduleName = moduleName, name = name } (nodifyAll patterns)


{-| AsPattern (Node Pattern) (Node String)
-}
asPattern : Pattern -> String -> Pattern
asPattern pattern name =
    AsPattern (nodify pattern) (nodify name)


{-| ParenthesizedPattern (Node Pattern)
-}
parensPattern : Pattern -> Pattern
parensPattern pattern =
    ParenthesizedPattern (nodify pattern)



--== Elm.Syntax.Signature


{-| Creates a type signature.
-}
signature : String -> TypeAnnotation -> Signature
signature name annotation =
    { name = nodify name
    , typeAnnotation = nodify annotation
    }



--== Elm.Syntax.Type


customType : Maybe String -> String -> List String -> List ( String, List TypeAnnotation ) -> Type
customType docs name args constructors =
    let
        vcons =
            List.map (\( consName, annotation ) -> valueConstructor consName annotation) constructors
    in
    { documentation = nodifyMaybe docs
    , name = nodify name
    , generics = nodifyAll args
    , constructors = nodifyAll vcons
    }


valueConstructor : String -> List TypeAnnotation -> ValueConstructor
valueConstructor name annotations =
    { name = nodify name
    , arguments = nodifyAll annotations
    }



--== Elm.Syntax.TypeAlias


typeAlias : Maybe String -> String -> List String -> TypeAnnotation -> TypeAlias
typeAlias docs name args annotation =
    { documentation = nodifyMaybe docs
    , name = nodify name
    , generics = nodifyAll args
    , typeAnnotation = nodify annotation
    }



--== Elm.Syntax.TypeAnnotation


{-| GenericType String
-}
typeVar : String -> TypeAnnotation
typeVar name =
    GenericType name


{-| Typed (Node ( ModuleName, String )) (List (Node TypeAnnotation))
-}
fqTyped : ModuleName -> String -> List TypeAnnotation -> TypeAnnotation
fqTyped moduleName name args =
    Typed (nodify ( moduleName, name )) (nodifyAll args)


{-| Typed (Node ( ModuleName, String )) (List (Node TypeAnnotation))
-}
typed : String -> List TypeAnnotation -> TypeAnnotation
typed name args =
    Typed (nodify ( [], name )) (nodifyAll args)


{-| Unit
-}
unitAnn : TypeAnnotation
unitAnn =
    Unit


{-| Tupled (List (Node TypeAnnotation))
-}
tupleAnn : List TypeAnnotation -> TypeAnnotation
tupleAnn args =
    Tupled (nodifyAll args)


{-| Record RecordDefinition
-}
recordAnn : List ( String, TypeAnnotation ) -> TypeAnnotation
recordAnn fields =
    List.map (uncurry recordField) fields
        |> recordDefinition
        |> Record


{-| GenericRecord (Node String) (Node RecordDefinition)
-}
extRecordAnn : String -> List ( String, TypeAnnotation ) -> TypeAnnotation
extRecordAnn argName fields =
    List.map (uncurry recordField) fields
        |> recordDefinition
        |> nodify
        |> GenericRecord (nodify argName)


{-| FunctionTypeAnnotation (Node TypeAnnotation) (Node TypeAnnotation)
-}
funAnn : TypeAnnotation -> TypeAnnotation -> TypeAnnotation
funAnn arg result =
    FunctionTypeAnnotation (nodify arg) (nodify result)


{-| A `Bool` type annotation.
-}
boolAnn : TypeAnnotation
boolAnn =
    typed "Bool" []


{-| An `Int` type annotation.
-}
intAnn : TypeAnnotation
intAnn =
    typed "Int" []


{-| A `Float` type annotation.
-}
floatAnn : TypeAnnotation
floatAnn =
    typed "Float" []


{-| A `String` type annotation.
-}
stringAnn : TypeAnnotation
stringAnn =
    typed "String" []


{-| A `Char` type annotation.
-}
charAnn : TypeAnnotation
charAnn =
    typed "Char" []


{-| Creates a `List` type annotation.
-}
listAnn : TypeAnnotation -> TypeAnnotation
listAnn listArg =
    typed "List" [ listArg ]


{-| Creates a `Set` type annotation.
-}
setAnn : TypeAnnotation -> TypeAnnotation
setAnn setArg =
    typed "Set" [ setArg ]


{-| Creates a `Dict` type annotation.
-}
dictAnn : TypeAnnotation -> TypeAnnotation -> TypeAnnotation
dictAnn keyArg valArg =
    typed "Dict" [ keyArg, valArg ]


{-| Creates a `Maybe` type annotation.
-}
maybeAnn : TypeAnnotation -> TypeAnnotation
maybeAnn maybeArg =
    typed "Maybe" [ maybeArg ]


recordDefinition : List RecordField -> RecordDefinition
recordDefinition fields =
    nodifyAll fields


recordField : String -> TypeAnnotation -> RecordField
recordField field typeAnnotation =
    ( nodify field, nodify typeAnnotation )



--== Helpers


nodify : a -> Node a
nodify exp =
    Node emptyRange exp


nodifyMaybe : Maybe a -> Maybe (Node a)
nodifyMaybe =
    Maybe.map nodify


nodifyAll : List a -> List (Node a)
nodifyAll =
    List.map nodify


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry fn ( a, b ) =
    fn a b


exposing_ : List TopLevelExpose -> Exposing
exposing_ exposes =
    case exposes of
        [] ->
            All emptyRange

        es ->
            Explicit <| nodifyAll exposes
