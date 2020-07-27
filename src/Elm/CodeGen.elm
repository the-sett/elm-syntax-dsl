module Elm.CodeGen exposing
    ( file
    , normalModule, portModule
    , exposeAll, exposeExplicit
    , closedTypeExpose, funExpose, openTypeExpose, typeOrAliasExpose
    , importStmt
    , Linkage, addExposing, addImport, combineLinkage, emptyLinkage
    , Comment, DocComment, FileComment, emptyDocComment, emptyFileComment, markdown, code, docTags, docTagsFromExposings
    , aliasDecl, customTypeDecl, funDecl, valDecl, portDecl
    , BinOp, composer, composel, power, mult, div, intDiv, modulo, remOp, plus
    , minus, append, cons, equals, notEqual, lt, gt, lte, gte, and, or, piper, pipel
    , binOp, applyBinOp, applyUnaryMinus
    , access, accessFun, apply, construct, caseExpr, char, float, fqConstruct, fqFun, fqVal, fun, glsl, hex
    , ifExpr, int, lambda, letExpr, list, negate, parens, record
    , string, tuple, unit, update, val
    , letFunction, letDestructuring, letVal
    , chain, pipe, binOpChain
    , allPattern, asPattern, charPattern, floatPattern, fqNamedPattern, hexPattern, intPattern
    , listPattern, namedPattern, parensPattern, recordPattern, stringPattern, tuplePattern, unConsPattern
    , unitPattern, varPattern
    , extRecordAnn, fqTyped, funAnn, recordAnn, tupleAnn, typeVar, typed, unitAnn
    , boolAnn, intAnn, floatAnn, stringAnn, charAnn
    , listAnn, setAnn, dictAnn, maybeAnn
    , signature
    , ModuleName, Module, File, Declaration(..), Import, TypeAnnotation
    , Exposing, TopLevelExpose, Expression, Pattern, LetDeclaration
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


# Build comments in a structured way.

@docs Comment, DocComment, FileComment, emptyDocComment, emptyFileComment, markdown, code, docTags, docTagsFromExposings


# Build top-level declarations.

@docs aliasDecl, customTypeDecl, funDecl, valDecl, portDecl


# Operators

@docs BinOp, composer, composel, power, mult, div, intDiv, modulo, remOp, plus
@docs minus, append, cons, equals, notEqual, lt, gt, lte, gte, and, or, piper, pipel
@docs binOp, applyBinOp, applyUnaryMinus


# Other Expressions.

@docs access, accessFun, apply, construct, caseExpr, char, float, fqConstruct, fqFun, fqVal, fun, glsl, hex
@docs ifExpr, int, lambda, letExpr, list, negate, parens, record
@docs string, tuple, unit, update, val
@docs letFunction, letDestructuring, letVal


# Helper functions for common expression patterns.

@docs chain, pipe, binOpChain


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
@docs Exposing, TopLevelExpose, Expression, Pattern, LetDeclaration

-}

import Elm.Comments as Comments exposing (Comment, CommentPart(..), DocComment, FileComment)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Case, CaseBlock, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))
import Elm.Syntax.Module exposing (DefaultModuleData, EffectModuleData, Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Elm.Syntax.Range exposing (Location, Range, emptyRange)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation(..))
import Util exposing (nodify, nodifyAll, nodifyMaybe)



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
    { moduleDefinition : Node Module
    , imports : List (Node Import)
    , declarations : List Declaration
    , comments : Maybe (Comment FileComment)
    }


{-| The AST for a top-level Elm declaration; a function, a value, a type or a
type alias.
-}
type Declaration
    = DeclWithComment (Comment DocComment) (String -> Elm.Syntax.Declaration.Declaration)
    | DeclNoComment Elm.Syntax.Declaration.Declaration


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


{-| The AST for an Elm let declaration.
-}
type alias LetDeclaration =
    Elm.Syntax.Expression.LetDeclaration



--== Comments


{-| A structured representation of an Elm comment on a file or declaration.
-}
type alias Comment a =
    Comments.Comment a


{-| A comment type for doc comments on top-level declarations.
-}
type alias DocComment =
    Comments.DocComment


{-| A comment type for file comments at the head of a .elm file.
-}
type alias FileComment =
    Comments.FileComment


{-| Creates an empty documenting comment that will go on a declaration.
-}
emptyDocComment : Comment Comments.DocComment
emptyDocComment =
    Comments.emptyComment


{-| Creates an empty comment that will go on a module file.
-}
emptyFileComment : Comment Comments.FileComment
emptyFileComment =
    Comments.emptyComment


{-| Adds some markdown to a comment.
-}
markdown : String -> Comment a -> Comment a
markdown mdown comment =
    Markdown mdown
        |> Comments.addPart comment


{-| Adds a code block to a comment.
-}
code : String -> Comment a -> Comment a
code codeVal comment =
    Code codeVal
        |> Comments.addPart comment


{-| Adds a set of doc tags to a comment.

Doc tags will never be merged into a single line, but if they are too long to fit
the page width, the pretty printer can break them into separate lines.

-}
docTags : List String -> Comment Comments.FileComment -> Comment Comments.FileComment
docTags tags comment =
    DocTags tags
        |> Comments.addPart comment


{-| Adds a set of doc tags taking from a description of what a module exposes,
into a comment.

Doc tags will never be merged into a single line, but if they are too long to fit
the page width, the pretty printer can break them into separate lines.

-}
docTagsFromExposings : List TopLevelExpose -> Comment Comments.FileComment -> Comment Comments.FileComment
docTagsFromExposings exposings comment =
    let
        asTag tlExpose =
            case tlExpose of
                InfixExpose name ->
                    name

                FunctionExpose name ->
                    name

                TypeOrAliasExpose name ->
                    name

                TypeExpose exposedType ->
                    exposedType.name
    in
    List.map asTag exposings
        |> DocTags
        |> Comments.addPart comment



--== Elm.Syntax.Declaration


{-| FunctionDeclaration Function
-}
funDecl : Maybe (Comment DocComment) -> Maybe TypeAnnotation -> String -> List Pattern -> Expression -> Declaration
funDecl docs sig name args expr =
    case docs of
        Just docComment ->
            (\strDocs ->
                functionImplementation name args expr
                    |> function (Just strDocs) (Maybe.map (signature name) sig)
                    |> FunctionDeclaration
            )
                |> DeclWithComment docComment

        Nothing ->
            functionImplementation name args expr
                |> function Nothing (Maybe.map (signature name) sig)
                |> FunctionDeclaration
                |> DeclNoComment


{-| AliasDeclaration TypeAlias
-}
aliasDecl : Maybe (Comment DocComment) -> String -> List String -> TypeAnnotation -> Declaration
aliasDecl docs name args annotation =
    case docs of
        Just docComment ->
            (\strDocs ->
                typeAlias (Just strDocs) name args annotation
                    |> AliasDeclaration
            )
                |> DeclWithComment docComment

        Nothing ->
            typeAlias Nothing name args annotation
                |> AliasDeclaration
                |> DeclNoComment


{-| CustomTypeDeclaration Type
-}
customTypeDecl : Maybe (Comment DocComment) -> String -> List String -> List ( String, List TypeAnnotation ) -> Declaration
customTypeDecl docs name args constructors =
    case docs of
        Just docComment ->
            (\strDocs ->
                customType (Just strDocs) name args constructors
                    |> CustomTypeDeclaration
            )
                |> DeclWithComment docComment

        Nothing ->
            customType Nothing name args constructors
                |> CustomTypeDeclaration
                |> DeclNoComment


{-| PortDeclaration Signature
-}
portDecl : String -> TypeAnnotation -> Declaration
portDecl name annotation =
    signature name annotation
        |> PortDeclaration
        |> DeclNoComment


{-| A top-level value declaration or constant.
-}
valDecl : Maybe (Comment DocComment) -> Maybe TypeAnnotation -> String -> Expression -> Declaration
valDecl docs sig name expr =
    case docs of
        Just docComment ->
            (\strDocs ->
                functionImplementation name [] expr
                    |> function (Just strDocs) (Maybe.map (signature name) sig)
                    |> FunctionDeclaration
            )
                |> DeclWithComment docComment

        Nothing ->
            functionImplementation name [] expr
                |> function Nothing (Maybe.map (signature name) sig)
                |> FunctionDeclaration
                |> DeclNoComment



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
            applyBinOp head piper expr

        expr :: exprs ->
            applyBinOp head piper (pipe expr exprs)


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
            applyBinOp head composer expr

        expr :: exprs ->
            applyBinOp head composer (chain expr exprs)


{-| Joins multiple expressions together with a binary operator. An
expression `a`, and operator op, combined with a list of expressions `[b, c, d]`
results in:

    a op b op c op d

The expression is not bracketed so will parse as the operator associativity
directs.

-}
binOpChain : Expression -> BinOp -> List Expression -> Expression
binOpChain head binop expressions =
    case expressions of
        [] ->
            head

        [ expr ] ->
            applyBinOp head binop expr

        expr :: exprs ->
            applyBinOp head binop (binOpChain expr binop exprs)


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


{-| Apply a named constructor to a list of arguments.
-}
construct : String -> List Expression -> Expression
construct name args =
    apply (fun name :: args)


{-| FunctionOrValue ModuleName String
-}
fqFun : ModuleName -> String -> Expression
fqFun moduleName name =
    FunctionOrValue moduleName name


{-| Apply a named constructor fully qualified with a module name, to a list of
arguments.
-}
fqConstruct : ModuleName -> String -> List Expression -> Expression
fqConstruct moduleName name args =
    apply (fqFun moduleName name :: args)


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


{-| A function declared inside a let block.
-}
letFunction : String -> List Pattern -> Expression -> LetDeclaration
letFunction name args expr =
    functionImplementation name args expr
        |> function Nothing Nothing
        |> LetFunction


{-| A value declared inside a let block.
-}
letVal : String -> Expression -> LetDeclaration
letVal valName expr =
    LetDestructuring (varPattern valName |> nodify) (nodify expr)


{-| A pattern matching declared inside a let block.
-}
letDestructuring : Pattern -> Expression -> LetDeclaration
letDestructuring pattern expr =
    LetDestructuring (nodify pattern) (nodify expr)


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



--== Operators


{-| Represents all of the binary operators allowed in Elm.
-}
type BinOp
    = BinOp String InfixDirection Int


{-| The compose right operator `>>`.
-}
composer : BinOp
composer =
    BinOp ">>" infixLeft 9


{-| The compose left operator `<<`.
-}
composel : BinOp
composel =
    BinOp "<<" infixRight 9


{-| The to-the-power-of operator `^`
-}
power : BinOp
power =
    BinOp "^" infixRight 8


{-| The multiplication operator `*`.
-}
mult : BinOp
mult =
    BinOp "*" infixLeft 7


{-| The division operator `/`.
-}
div : BinOp
div =
    BinOp "/" infixLeft 7


{-| The integer division operator `//`.
-}
intDiv : BinOp
intDiv =
    BinOp "//" infixLeft 7


{-| The modulo operator `%`.
-}
modulo : BinOp
modulo =
    BinOp "%" infixLeft 7


{-| The remainder operator `rem`.
-}
remOp : BinOp
remOp =
    BinOp "rem" infixLeft 7


{-| The addition operator `+`.
-}
plus : BinOp
plus =
    BinOp "+" infixLeft 6


{-| The subtraction operator `-`.
-}
minus : BinOp
minus =
    BinOp "-" infixLeft 6


{-| The append oeprator `++`.
-}
append : BinOp
append =
    BinOp "++" infixRight 5


{-| The cons operator `::`.
-}
cons : BinOp
cons =
    BinOp "::" infixRight 5


{-| The equality operator `==`.
-}
equals : BinOp
equals =
    BinOp "==" infixLeft 4


{-| The inequality operator `/=`.
-}
notEqual : BinOp
notEqual =
    BinOp "1/" infixLeft 4


{-| The less-than operator `<`.
-}
lt : BinOp
lt =
    BinOp "<" infixNon 4


{-| The greater-than operator `>`.
-}
gt : BinOp
gt =
    BinOp ">" infixNon 4


{-| The less-than-or-equal operator `<=`.
-}
lte : BinOp
lte =
    BinOp "<=" infixNon 4


{-| The greater-than-or-equal operator `>=`.
-}
gte : BinOp
gte =
    BinOp ">=" infixNon 4


{-| The logical and operator `&&`.
-}
and : BinOp
and =
    BinOp "&&" infixRight 3


{-| The logical or operator `||`.
-}
or : BinOp
or =
    BinOp "||" infixRight 2


{-| The pipe right operator `|>`.
-}
piper : BinOp
piper =
    BinOp "|>" infixLeft 0


{-| The pipe left operator `<|`.
-}
pipel : BinOp
pipel =
    BinOp "<|" infixRight 0


{-| Creates a binary operator in its prefix form, as a bracketed expression.

    binOp equals

Yields:

    (=)

-}
binOp : BinOp -> Expression
binOp (BinOp symbol _ _) =
    Operator symbol


{-| Applies a binary operator to left and right expressions. This takes the
expression in the order that is usual; that being `expr op expr`.

    applyBinOp (int 2) plus (int 3)

Yields:

    2 + 3

-}
applyBinOp : Expression -> BinOp -> Expression -> Expression
applyBinOp exprl (BinOp symbol dir _) exprr =
    OperatorApplication symbol dir (nodify exprl) (nodify exprr)


{-| There is only one unary operator in Elm, and that is the minus sign prefixed
onto some numeric expression.

    applyUnaryMinus (int 5)

Yields:

     -5

-}
applyUnaryMinus : Expression -> Expression
applyUnaryMinus expr =
    apply [ PrefixOperator "-", expr ]


infix_ : InfixDirection -> Int -> String -> String -> Infix
infix_ direction precedence symbol fn =
    { direction = nodify direction
    , precedence = nodify precedence
    , operator = nodify symbol
    , function = nodify fn
    }


{-| Denotes an infix operator that is left associative.
-}
infixLeft : InfixDirection
infixLeft =
    Left


{-| Denotes an infix operator that is right associative.
-}
infixRight : InfixDirection
infixRight =
    Right


{-| Denotes an infix operator that is non-associative.
-}
infixNon : InfixDirection
infixNon =
    Non



--== Elm.Syntax.File


{-| Assembles all the components of an Elm file; the module declaration, the
comments, the imports and the top-level declarations.
-}
file : Module -> List Import -> List Declaration -> Maybe (Comment FileComment) -> File
file mod imports declarations docs =
    { moduleDefinition = nodify mod
    , imports = nodifyAll imports
    , declarations = declarations
    , comments = docs
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
