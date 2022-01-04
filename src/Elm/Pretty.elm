module Elm.Pretty exposing
    ( prepareLayout, pretty
    , prettyModule
    , prettyImports, prettyExposing
    , prettyDeclaration, prettyFun, prettyTypeAlias, prettyCustomType, prettyPortDeclaration, prettyDestructuring
    , prettySignature, prettyPattern, prettyExpression, prettyTypeAnnotation
    )

{-| Elm.Pretty is a pretty printer for Elm syntax trees. It makes use of
`the-sett/elm-pretty-printer` to best fit the code to a given page width in
characters.

It aims to output code that is fully stable with respect to `elm-format` in the
sense that running `elm-format` on the output should have no effect at all. The
advantage of this is that if generated code moves to being edited by hand, there
will not be a large white-space only diff created when `elm-format` is applied.

To print the `Doc` created by the `pretty` functions, `the-sett/elm-pretty-printer`
is used:

    import Elm.Pretty
    import Pretty


    -- Fit to a page width of 120 characters
    elmAsString =
        Elm.Pretty.prepareLayout 120 someFile
            |> Pretty.pretty 120

There is also a helper `pretty` function in this module that can go straight to
a `String`, for convenience:

    -- Fit to a page width of 120 characters
    elmAsString =
        Elm.Pretty.pretty 120 someFile


# Pretty prints an entire Elm file.

@docs prepareLayout, pretty


# Pretty printing snippets of Elm.

@docs prettyModule
@docs prettyImports, prettyExposing
@docs prettyDeclaration, prettyFun, prettyTypeAlias, prettyCustomType, prettyPortDeclaration, prettyDestructuring
@docs prettySignature, prettyPattern, prettyExpression, prettyTypeAnnotation

-}

import Bool.Extra
import Elm.CodeGen exposing (Declaration(..), File)
import Elm.Comments
import Elm.Syntax.Declaration
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (CaseBlock, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.File
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))
import Elm.Syntax.Module exposing (DefaultModuleData, EffectModuleData, Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (RecordField, TypeAnnotation(..))
import Elm.Token as Token exposing (Token)
import Hex
import ImportsAndExposing
import Pretty exposing (Doc)
import Util exposing (denode, denodeAll, denodeMaybe, nodify, nodifyAll)


{-| Prepares a file of Elm code for layout by the pretty printer.

Note that the `Doc` type returned by this is a `Pretty.Doc`. This can be printed
to a string by the `the-sett/elm-pretty-printer` package.

These `Doc` based functions are exposed in case you want to pretty print some
Elm inside something else with the pretty printer. The `pretty` function can be
used to go directly from a `File` to a `String`, if that is more convenient.

-}
prepareLayout : Int -> File -> Doc Token
prepareLayout width file =
    let
        layoutDeclComments decls =
            List.map
                (prettyDocComment width)
                decls

        ( innerFile, tags ) =
            case file.comments of
                Just comment ->
                    let
                        ( fileCommentStr, innerTags ) =
                            Elm.Comments.prettyFileComment width comment
                    in
                    ( { moduleDefinition = file.moduleDefinition
                      , imports = file.imports
                      , declarations = layoutDeclComments file.declarations |> nodifyAll
                      , comments = nodifyAll [ fileCommentStr ]
                      }
                    , innerTags
                    )

                Nothing ->
                    ( { moduleDefinition = file.moduleDefinition
                      , imports = file.imports
                      , declarations = layoutDeclComments file.declarations |> nodifyAll
                      , comments = []
                      }
                    , []
                    )
    in
    prettyModule (denode innerFile.moduleDefinition)
        |> Pretty.a Pretty.line
        |> Pretty.a Pretty.line
        |> Pretty.a (prettyComments (denodeAll innerFile.comments))
        |> Pretty.a (importsPretty innerFile)
        |> Pretty.a (prettyDeclarations (denodeAll innerFile.declarations))


importsPretty : Elm.Syntax.File.File -> Doc Token
importsPretty file =
    case file.imports of
        [] ->
            Pretty.line

        _ ->
            prettyImports (denodeAll file.imports)
                |> Pretty.a Pretty.line
                |> Pretty.a Pretty.line
                |> Pretty.a Pretty.line


{-| Prints a file of Elm code to the given page width, making use of the pretty
printer.
-}
pretty : Int -> File -> String
pretty width file =
    prepareLayout width file
        |> Pretty.pretty width


{-| Pretty prints a module definition.
-}
prettyModule : Module -> Doc Token
prettyModule mod =
    case mod of
        NormalModule defaultModuleData ->
            prettyDefaultModuleData defaultModuleData

        PortModule defaultModuleData ->
            prettyPortModuleData defaultModuleData

        EffectModule effectModuleData ->
            prettyEffectModuleData effectModuleData


prettyModuleName : ModuleName -> Doc Token
prettyModuleName name =
    List.map (\n -> Token.plain n) name
        |> Pretty.join dot


prettyModuleNameDot : ModuleName -> Doc Token
prettyModuleNameDot name =
    case name of
        [] ->
            Pretty.empty

        _ ->
            List.map Token.type_ name
                |> Pretty.join dot
                |> Pretty.a dot


prettyModuleNameAlias : ModuleName -> Doc Token
prettyModuleNameAlias name =
    case name of
        [] ->
            Pretty.empty

        _ ->
            Token.keyword "as "
                |> Pretty.a (List.map Token.plain name |> Pretty.join dot)


prettyDefaultModuleData : DefaultModuleData -> Doc Token
prettyDefaultModuleData moduleData =
    Pretty.words
        [ Token.keyword "module"
        , prettyModuleName (denode moduleData.moduleName)
        , prettyExposing (denode moduleData.exposingList)
        ]


prettyPortModuleData : DefaultModuleData -> Doc Token
prettyPortModuleData moduleData =
    Pretty.words
        [ Token.keyword "port module"
        , prettyModuleName (denode moduleData.moduleName)
        , prettyExposing (denode moduleData.exposingList)
        ]


prettyEffectModuleData : EffectModuleData -> Doc Token
prettyEffectModuleData moduleData =
    let
        prettyCmdAndSub maybeCmd maybeSub =
            case ( maybeCmd, maybeSub ) of
                ( Nothing, Nothing ) ->
                    Nothing

                ( Just cmdName, Just subName ) ->
                    [ Pretty.string "where { command ="
                    , Pretty.string cmdName
                    , Pretty.string ","
                    , Pretty.string "subscription ="
                    , Pretty.string subName
                    , Pretty.string "}"
                    ]
                        |> Pretty.words
                        |> Just

                ( Just cmdName, Nothing ) ->
                    [ Pretty.string "where { command ="
                    , Pretty.string cmdName
                    , Pretty.string "}"
                    ]
                        |> Pretty.words
                        |> Just

                ( Nothing, Just subName ) ->
                    [ Pretty.string "where { subscription ="
                    , Pretty.string subName
                    , Pretty.string "}"
                    ]
                        |> Pretty.words
                        |> Just
    in
    Pretty.words
        [ Token.keyword "effect module"
        , prettyModuleName (denode moduleData.moduleName)
        , prettyCmdAndSub (denodeMaybe moduleData.command) (denodeMaybe moduleData.subscription)
            |> prettyMaybe identity
        , prettyExposing (denode moduleData.exposingList)
        ]


prettyComments : List String -> Doc Token
prettyComments comments =
    case comments of
        [] ->
            Pretty.empty

        _ ->
            List.map Token.comment comments
                |> Pretty.lines
                |> Pretty.a Pretty.line
                |> Pretty.a Pretty.line


{-| Pretty prints a list of import statements.

The list will be de-duplicated and sorted.

-}
prettyImports : List Import -> Doc Token
prettyImports imports =
    ImportsAndExposing.sortAndDedupImports imports
        |> List.map prettyImport
        |> Pretty.lines


prettyImport : Import -> Doc Token
prettyImport import_ =
    Pretty.join Pretty.space
        [ Token.keyword "import"
        , prettyModuleName (denode import_.moduleName)
        , prettyMaybe prettyModuleNameAlias (denodeMaybe import_.moduleAlias)
        , prettyMaybe prettyExposing (denodeMaybe import_.exposingList)
        ]


{-| Pretty prints the contents of an exposing statement, as found on a module or import
statement.

The exposed values will be de-duplicated and sorted.

-}
prettyExposing : Exposing -> Doc Token
prettyExposing exposing_ =
    let
        exposings =
            case exposing_ of
                All _ ->
                    Token.plain ".." |> Pretty.parens

                Explicit tll ->
                    ImportsAndExposing.sortAndDedupExposings (denodeAll tll)
                        |> prettyTopLevelExposes
                        |> Pretty.parens
    in
    Token.keyword "exposing"
        |> Pretty.a Pretty.space
        |> Pretty.a exposings


prettyTopLevelExposes : List TopLevelExpose -> Doc Token
prettyTopLevelExposes exposes =
    List.map prettyTopLevelExpose exposes
        |> Pretty.join (Pretty.string ", ")


prettyTopLevelExpose : TopLevelExpose -> Doc Token
prettyTopLevelExpose tlExpose =
    case tlExpose of
        InfixExpose val ->
            Pretty.string val
                |> Pretty.parens

        FunctionExpose val ->
            Token.signature val

        TypeOrAliasExpose val ->
            Token.type_ val

        TypeExpose exposedType ->
            case exposedType.open of
                Nothing ->
                    Token.type_ exposedType.name

                Just _ ->
                    Token.type_ exposedType.name
                        |> Pretty.a (Pretty.string "(..)")



--== Declarations


{-| Pretty prints a single top-level declaration.
-}
prettyDeclaration : Int -> Declaration -> Doc Token
prettyDeclaration width decl =
    let
        innerDecl =
            prettyDocComment width decl
    in
    prettyElmSyntaxDeclaration innerDecl


{-| Pretty prints an elm-syntax declaration.
-}
prettyElmSyntaxDeclaration : Elm.Syntax.Declaration.Declaration -> Doc Token
prettyElmSyntaxDeclaration decl =
    case decl of
        Elm.Syntax.Declaration.FunctionDeclaration fn ->
            prettyFun fn

        Elm.Syntax.Declaration.AliasDeclaration tAlias ->
            prettyTypeAlias tAlias

        Elm.Syntax.Declaration.CustomTypeDeclaration type_ ->
            prettyCustomType type_

        Elm.Syntax.Declaration.PortDeclaration sig ->
            prettyPortDeclaration sig

        Elm.Syntax.Declaration.InfixDeclaration infix_ ->
            prettyInfix infix_

        Elm.Syntax.Declaration.Destructuring pattern expr ->
            prettyDestructuring (denode pattern) (denode expr)


prettyDeclarations : List Elm.Syntax.Declaration.Declaration -> Doc Token
prettyDeclarations decls =
    List.map
        (\decl ->
            prettyElmSyntaxDeclaration decl
                |> Pretty.a Pretty.line
        )
        decls
        |> doubleLines


{-| Pretty prints any doc comments on a declaration to string format, and provides
the result as an elm-syntax declaration.
-}
prettyDocComment : Int -> Declaration -> Elm.Syntax.Declaration.Declaration
prettyDocComment width decl =
    case decl of
        DeclWithComment comment declFn ->
            declFn (Elm.Comments.prettyDocComment width comment)

        DeclNoComment declNoComment ->
            declNoComment


{-| Pretty prints an Elm function, which may include documentation and a signature too.
-}
prettyFun : Function -> Doc Token
prettyFun fn =
    [ prettyMaybe prettyDocumentation (denodeMaybe fn.documentation)
    , prettyMaybe prettySignature (denodeMaybe fn.signature)
    , prettyFunctionImplementation (denode fn.declaration)
    ]
        |> Pretty.lines


{-| Pretty prints a type alias definition, which may include documentation too.
-}
prettyTypeAlias : TypeAlias -> Doc Token
prettyTypeAlias tAlias =
    let
        typeAliasPretty =
            [ Token.keyword "type alias"
            , Token.type_ (denode tAlias.name)
            , List.map Token.plain (denodeAll tAlias.generics) |> Pretty.words
            , Pretty.string "="
            ]
                |> Pretty.words
                |> Pretty.a Pretty.line
                |> Pretty.a (prettyTypeAnnotation (denode tAlias.typeAnnotation))
                |> Pretty.nest 4
    in
    [ prettyMaybe prettyDocumentation (denodeMaybe tAlias.documentation)
    , typeAliasPretty
    ]
        |> Pretty.lines


{-| Pretty prints a custom type declaration, which may include documentation too.
-}
prettyCustomType : Type -> Doc Token
prettyCustomType type_ =
    let
        customTypePretty =
            [ Token.keyword "type"
            , Token.type_ (denode type_.name)
            , List.map Token.plain (denodeAll type_.generics) |> Pretty.words
            ]
                |> Pretty.words
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.string "= ")
                |> Pretty.a (prettyValueConstructors (denodeAll type_.constructors))
                |> Pretty.nest 4
    in
    [ prettyMaybe prettyDocumentation (denodeMaybe type_.documentation)
    , customTypePretty
    ]
        |> Pretty.lines


prettyValueConstructors : List ValueConstructor -> Doc Token
prettyValueConstructors constructors =
    List.map prettyValueConstructor constructors
        |> Pretty.join (Pretty.line |> Pretty.a (Pretty.string "| "))


prettyValueConstructor : ValueConstructor -> Doc Token
prettyValueConstructor cons =
    [ Token.plain (denode cons.name)
    , List.map prettyTypeAnnotationParens (denodeAll cons.arguments) |> Pretty.lines
    ]
        |> Pretty.lines
        |> Pretty.group
        |> Pretty.nest 4


{-| Pretty prints a port declaration.
-}
prettyPortDeclaration : Signature -> Doc Token
prettyPortDeclaration sig =
    [ Token.keyword "port"
    , prettySignature sig
    ]
        |> Pretty.words


prettyInfix : Infix -> Doc Token
prettyInfix infix_ =
    let
        dirToString direction =
            case direction of
                Left ->
                    "left"

                Right ->
                    "right"

                Non ->
                    "non"
    in
    [ Pretty.string "infix"
    , Pretty.string (dirToString (denode infix_.direction))
    , Pretty.string (String.fromInt (denode infix_.precedence))
    , Pretty.string (denode infix_.operator) |> Pretty.parens
    , Pretty.string "="
    , Pretty.string (denode infix_.function)
    ]
        |> Pretty.words


{-| Pretty prints a destructuring declaration.
-}
prettyDestructuring : Pattern -> Expression -> Doc Token
prettyDestructuring pattern expr =
    [ [ prettyPattern pattern
      , Pretty.string "="
      ]
        |> Pretty.words
    , prettyExpression expr
    ]
        |> Pretty.lines
        |> Pretty.nest 4


prettyDocumentation : Documentation -> Doc Token
prettyDocumentation docs =
    Pretty.string docs


{-| Pretty prints a type signature.
-}
prettySignature : Signature -> Doc Token
prettySignature sig =
    [ [ Token.signature (denode sig.name)
      , Pretty.string ":"
      ]
        |> Pretty.words
    , prettyTypeAnnotation (denode sig.typeAnnotation)
    ]
        |> Pretty.lines
        |> Pretty.nest 4
        |> Pretty.group


prettyFunctionImplementation : FunctionImplementation -> Doc Token
prettyFunctionImplementation impl =
    Pretty.words
        [ Token.signature (denode impl.name)
        , prettyArgs (denodeAll impl.arguments)
        , Pretty.string "="
        ]
        |> Pretty.a Pretty.line
        |> Pretty.a (prettyExpression (denode impl.expression))
        |> Pretty.nest 4


prettyArgs : List Pattern -> Doc Token
prettyArgs args =
    List.map (prettyPatternInner False) args
        |> Pretty.words



--== Patterns


{-| Pretty prints a pattern.
-}
prettyPattern : Pattern -> Doc Token
prettyPattern pattern =
    prettyPatternInner True pattern


adjustPatternParentheses : Bool -> Pattern -> Pattern
adjustPatternParentheses isTop pattern =
    let
        addParens pat =
            case ( isTop, pat ) of
                ( False, NamedPattern _ (_ :: _) ) ->
                    nodify pat |> ParenthesizedPattern

                ( False, AsPattern _ _ ) ->
                    nodify pat |> ParenthesizedPattern

                ( _, _ ) ->
                    pat

        removeParens pat =
            case pat of
                ParenthesizedPattern innerPat ->
                    if shouldRemove (denode innerPat) then
                        denode innerPat
                            |> removeParens

                    else
                        pat

                _ ->
                    pat

        shouldRemove pat =
            case ( isTop, pat ) of
                ( False, NamedPattern _ _ ) ->
                    False

                ( _, AsPattern _ _ ) ->
                    False

                ( _, _ ) ->
                    isTop
    in
    removeParens pattern
        |> addParens


prettyPatternInner : Bool -> Pattern -> Doc Token
prettyPatternInner isTop pattern =
    case adjustPatternParentheses isTop pattern of
        AllPattern ->
            Pretty.string "_"

        UnitPattern ->
            Pretty.string "()"

        CharPattern val ->
            Token.literal (escapeChar val)
                |> singleQuotes

        StringPattern val ->
            Token.literal val
                |> quotes

        IntPattern val ->
            Token.number (String.fromInt val)

        HexPattern val ->
            Token.number (toHexString val)

        FloatPattern val ->
            Token.number (String.fromFloat val)

        TuplePattern vals ->
            Pretty.space
                |> Pretty.a
                    (List.map (prettyPatternInner True) (denodeAll vals)
                        |> Pretty.join (Pretty.string ", ")
                    )
                |> Pretty.a Pretty.space
                |> Pretty.parens

        RecordPattern fields ->
            List.map Token.plain (denodeAll fields)
                |> Pretty.join (Pretty.string ", ")
                |> Pretty.surround Pretty.space Pretty.space
                |> Pretty.braces

        UnConsPattern hdPat tlPat ->
            [ prettyPatternInner False (denode hdPat)
            , Pretty.string "::"
            , prettyPatternInner False (denode tlPat)
            ]
                |> Pretty.words

        ListPattern listPats ->
            case listPats of
                [] ->
                    Pretty.string "[]"

                _ ->
                    let
                        open =
                            Pretty.a Pretty.space (Pretty.string "[")

                        close =
                            Pretty.a (Pretty.string "]") Pretty.space
                    in
                    List.map (prettyPatternInner False) (denodeAll listPats)
                        |> Pretty.join (Pretty.string ", ")
                        |> Pretty.surround open close

        VarPattern var ->
            Token.plain var

        NamedPattern qnRef listPats ->
            (prettyModuleNameDot qnRef.moduleName
                |> Pretty.a (Token.plain qnRef.name)
            )
                :: List.map (prettyPatternInner False) (denodeAll listPats)
                |> Pretty.words

        AsPattern pat name ->
            [ prettyPatternInner False (denode pat)
            , Token.keyword "as"
            , Token.plain (denode name)
            ]
                |> Pretty.words

        ParenthesizedPattern pat ->
            prettyPatternInner True (denode pat)
                |> Pretty.parens



--== Expressions


type alias Context =
    { precedence : Int
    , isTop : Bool
    , isLeftPipe : Bool
    }


topContext =
    { precedence = 11
    , isTop = True
    , isLeftPipe = False
    }


adjustExpressionParentheses : Context -> Expression -> Expression
adjustExpressionParentheses context expression =
    let
        addParens expr =
            case ( context.isTop, context.isLeftPipe, expr ) of
                ( False, False, LetExpression _ ) ->
                    nodify expr |> ParenthesizedExpression

                ( False, False, CaseExpression _ ) ->
                    nodify expr |> ParenthesizedExpression

                ( False, False, LambdaExpression _ ) ->
                    nodify expr |> ParenthesizedExpression

                ( False, False, IfBlock _ _ _ ) ->
                    nodify expr |> ParenthesizedExpression

                ( _, _, _ ) ->
                    expr

        removeParens expr =
            case expr of
                ParenthesizedExpression innerExpr ->
                    if shouldRemove (denode innerExpr) then
                        denode innerExpr
                            |> removeParens

                    else
                        expr

                _ ->
                    expr

        shouldRemove expr =
            case ( context.isTop, context.isLeftPipe, expr ) of
                ( True, _, _ ) ->
                    True

                ( _, True, _ ) ->
                    True

                ( False, _, Application _ ) ->
                    if context.precedence < 11 then
                        True

                    else
                        False

                ( False, _, FunctionOrValue _ _ ) ->
                    True

                ( False, _, Integer _ ) ->
                    True

                ( False, _, Hex _ ) ->
                    True

                ( False, _, Floatable _ ) ->
                    True

                ( False, _, Negation _ ) ->
                    True

                ( False, _, Literal _ ) ->
                    True

                ( False, _, CharLiteral _ ) ->
                    True

                ( False, _, TupledExpression _ ) ->
                    True

                ( False, _, RecordExpr _ ) ->
                    True

                ( False, _, ListExpr _ ) ->
                    True

                ( False, _, RecordAccess _ _ ) ->
                    True

                ( False, _, RecordAccessFunction _ ) ->
                    True

                ( False, _, RecordUpdateExpression _ _ ) ->
                    True

                ( _, _, _ ) ->
                    False
    in
    removeParens expression
        |> addParens


{-| Pretty prints an expression.
-}
prettyExpression : Expression -> Doc Token
prettyExpression expression =
    prettyExpressionInner topContext 4 expression
        |> Tuple.first


prettyExpressionInner : Context -> Int -> Expression -> ( Doc Token, Bool )
prettyExpressionInner context indent expression =
    case adjustExpressionParentheses context expression of
        UnitExpr ->
            ( Pretty.string "()"
            , False
            )

        Application exprs ->
            prettyApplication indent exprs

        OperatorApplication symbol dir exprl exprr ->
            prettyOperatorApplication indent symbol dir exprl exprr

        FunctionOrValue modl val ->
            ( prettyModuleNameDot modl
                |> Pretty.a (Token.plain val)
            , False
            )

        IfBlock exprBool exprTrue exprFalse ->
            prettyIfBlock indent exprBool exprTrue exprFalse

        PrefixOperator symbol ->
            ( Token.plain symbol |> Pretty.parens
            , False
            )

        Operator symbol ->
            ( Pretty.string symbol
            , False
            )

        Integer val ->
            ( Token.number (String.fromInt val)
            , False
            )

        Hex val ->
            ( Token.number (toHexString val)
            , False
            )

        Floatable val ->
            ( Token.number (String.fromFloat val)
            , False
            )

        Negation expr ->
            let
                ( prettyExpr, alwaysBreak ) =
                    prettyExpressionInner topContext 4 (denode expr)
            in
            ( Token.plain "-"
                |> Pretty.a prettyExpr
            , alwaysBreak
            )

        Literal val ->
            ( prettyLiteral val
            , False
            )

        CharLiteral val ->
            ( Token.literal (escapeChar val)
                |> singleQuotes
            , False
            )

        TupledExpression exprs ->
            prettyTupledExpression indent exprs

        ParenthesizedExpression expr ->
            prettyParenthesizedExpression indent expr

        LetExpression letBlock ->
            prettyLetBlock indent letBlock

        CaseExpression caseBlock ->
            prettyCaseBlock indent caseBlock

        LambdaExpression lambda ->
            prettyLambdaExpression indent lambda

        RecordExpr setters ->
            prettyRecordExpr setters

        ListExpr exprs ->
            prettyList indent exprs

        RecordAccess expr field ->
            prettyRecordAccess expr field

        RecordAccessFunction field ->
            ( Token.plain field
            , False
            )

        RecordUpdateExpression var setters ->
            prettyRecordUpdateExpression indent var setters

        GLSLExpression val ->
            ( Token.plain "glsl"
            , True
            )


prettyApplication : Int -> List (Node Expression) -> ( Doc Token, Bool )
prettyApplication indent exprs =
    let
        ( prettyExpressions, alwaysBreak ) =
            List.map (prettyExpressionInner { precedence = 11, isTop = False, isLeftPipe = False } 4) (denodeAll exprs)
                |> List.unzip
                |> Tuple.mapSecond Bool.Extra.any
    in
    ( prettyExpressions
        |> Pretty.lines
        |> Pretty.nest indent
        |> Pretty.align
        |> optionalGroup alwaysBreak
    , alwaysBreak
    )


isEndLineOperator : String -> Bool
isEndLineOperator op =
    case op of
        "<|" ->
            True

        _ ->
            False


prettyOperatorApplication : Int -> String -> InfixDirection -> Node Expression -> Node Expression -> ( Doc Token, Bool )
prettyOperatorApplication indent symbol dir exprl exprr =
    if symbol == "<|" then
        prettyOperatorApplicationLeft indent symbol dir exprl exprr

    else
        prettyOperatorApplicationRight indent symbol dir exprl exprr


prettyOperatorApplicationLeft : Int -> String -> InfixDirection -> Node Expression -> Node Expression -> ( Doc Token, Bool )
prettyOperatorApplicationLeft indent symbol _ exprl exprr =
    let
        context =
            { precedence = precedence symbol
            , isTop = False
            , isLeftPipe = True
            }

        ( prettyExpressionLeft, alwaysBreakLeft ) =
            prettyExpressionInner context 4 (denode exprl)

        ( prettyExpressionRight, alwaysBreakRight ) =
            prettyExpressionInner context 4 (denode exprr)

        alwaysBreak =
            alwaysBreakLeft || alwaysBreakRight
    in
    ( [ [ prettyExpressionLeft, Pretty.string symbol ] |> Pretty.words
      , prettyExpressionRight
      ]
        |> Pretty.lines
        |> optionalGroup alwaysBreak
        |> Pretty.nest 4
    , alwaysBreak
    )


prettyOperatorApplicationRight : Int -> String -> InfixDirection -> Node Expression -> Node Expression -> ( Doc Token, Bool )
prettyOperatorApplicationRight indent symbol _ exprl exprr =
    let
        expandExpr : Int -> Context -> Expression -> List ( Doc Token, Bool )
        expandExpr innerIndent context expr =
            case expr of
                OperatorApplication sym _ left right ->
                    innerOpApply False sym left right

                _ ->
                    [ prettyExpressionInner context innerIndent expr ]

        innerOpApply : Bool -> String -> Node Expression -> Node Expression -> List ( Doc Token, Bool )
        innerOpApply isTop sym left right =
            let
                context =
                    { precedence = precedence sym
                    , isTop = False
                    , isLeftPipe = "<|" == sym
                    }

                innerIndent =
                    decrementIndent 4 (String.length symbol + 1)

                leftIndent =
                    if isTop then
                        indent

                    else
                        innerIndent

                rightSide =
                    denode right |> expandExpr innerIndent context
            in
            case rightSide of
                ( hdExpr, hdBreak ) :: tl ->
                    List.append (denode left |> expandExpr leftIndent context)
                        (( Pretty.string sym |> Pretty.a Pretty.space |> Pretty.a hdExpr, hdBreak ) :: tl)

                [] ->
                    []

        ( prettyExpressions, alwaysBreak ) =
            innerOpApply True symbol exprl exprr
                |> List.unzip
                |> Tuple.mapSecond Bool.Extra.any
    in
    ( prettyExpressions
        |> Pretty.join (Pretty.nest indent Pretty.line)
        |> Pretty.align
        |> optionalGroup alwaysBreak
    , alwaysBreak
    )


prettyIfBlock : Int -> Node Expression -> Node Expression -> Node Expression -> ( Doc Token, Bool )
prettyIfBlock indent exprBool exprTrue exprFalse =
    let
        innerIfBlock : Node Expression -> Node Expression -> Node Expression -> List (Doc Token)
        innerIfBlock innerExprBool innerExprTrue innerExprFalse =
            let
                context =
                    topContext

                ifPart =
                    let
                        ( prettyBoolExpr, alwaysBreak ) =
                            prettyExpressionInner topContext 4 (denode innerExprBool)
                    in
                    [ [ Token.keyword "if"
                      , prettyExpressionInner topContext 4 (denode innerExprBool) |> Tuple.first
                      ]
                        |> Pretty.lines
                        |> optionalGroup alwaysBreak
                        |> Pretty.nest indent
                    , Token.keyword "then"
                    ]
                        |> Pretty.lines
                        |> optionalGroup alwaysBreak

                truePart =
                    prettyExpressionInner topContext 4 (denode innerExprTrue)
                        |> Tuple.first
                        |> Pretty.indent indent

                elsePart =
                    Pretty.line
                        |> Pretty.a (Token.keyword "else")

                falsePart =
                    case denode innerExprFalse of
                        IfBlock nestedExprBool nestedExprTrue nestedExprFalse ->
                            innerIfBlock nestedExprBool nestedExprTrue nestedExprFalse

                        _ ->
                            [ prettyExpressionInner topContext 4 (denode innerExprFalse)
                                |> Tuple.first
                                |> Pretty.indent indent
                            ]
            in
            case falsePart of
                [] ->
                    []

                [ falseExpr ] ->
                    [ ifPart
                    , truePart
                    , elsePart
                    , falseExpr
                    ]

                hd :: tl ->
                    List.append
                        [ ifPart
                        , truePart
                        , [ elsePart, hd ] |> Pretty.words
                        ]
                        tl

        prettyExpressions =
            innerIfBlock exprBool exprTrue exprFalse
    in
    ( prettyExpressions
        |> Pretty.lines
        |> Pretty.align
    , True
    )


prettyLiteral : String -> Doc Token
prettyLiteral val =
    Token.literal (escape val)
        |> quotes


prettyTupledExpression : Int -> List (Node Expression) -> ( Doc Token, Bool )
prettyTupledExpression indent exprs =
    let
        open =
            Pretty.a Pretty.space (Pretty.string "(")

        close =
            Pretty.a (Pretty.string ")") Pretty.line
    in
    case exprs of
        [] ->
            ( Pretty.string "()", False )

        _ ->
            let
                ( prettyExpressions, alwaysBreak ) =
                    List.map (prettyExpressionInner topContext (decrementIndent indent 2)) (denodeAll exprs)
                        |> List.unzip
                        |> Tuple.mapSecond Bool.Extra.any
            in
            ( prettyExpressions
                |> Pretty.separators ", "
                |> Pretty.surround open close
                |> Pretty.align
                |> optionalGroup alwaysBreak
            , alwaysBreak
            )


prettyParenthesizedExpression : Int -> Node Expression -> ( Doc Token, Bool )
prettyParenthesizedExpression indent expr =
    let
        open =
            Pretty.string "("

        close =
            Pretty.a (Pretty.string ")") Pretty.tightline

        ( prettyExpr, alwaysBreak ) =
            prettyExpressionInner topContext (decrementIndent indent 1) (denode expr)
    in
    ( prettyExpr
        |> Pretty.nest 1
        |> Pretty.surround open close
        |> Pretty.align
        |> optionalGroup alwaysBreak
    , alwaysBreak
    )


prettyLetBlock : Int -> LetBlock -> ( Doc Token, Bool )
prettyLetBlock indent letBlock =
    ( [ Token.keyword "let"
      , List.map (prettyLetDeclaration indent) (denodeAll letBlock.declarations)
            |> doubleLines
            |> Pretty.indent indent
      , Token.keyword "in"
      , prettyExpressionInner topContext 4 (denode letBlock.expression) |> Tuple.first
      ]
        |> Pretty.lines
        |> Pretty.align
    , True
    )


prettyLetDeclaration : Int -> LetDeclaration -> Doc Token
prettyLetDeclaration indent letDecl =
    case letDecl of
        LetFunction fn ->
            prettyFun fn

        LetDestructuring pattern expr ->
            [ prettyPatternInner False (denode pattern)
            , Pretty.string "="
            ]
                |> Pretty.words
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (prettyExpressionInner topContext 4 (denode expr)
                        |> Tuple.first
                        |> Pretty.indent indent
                    )


prettyCaseBlock : Int -> CaseBlock -> ( Doc Token, Bool )
prettyCaseBlock indent caseBlock =
    let
        casePart =
            let
                ( caseExpression, alwaysBreak ) =
                    prettyExpressionInner topContext 4 (denode caseBlock.expression)
            in
            [ [ Token.keyword "case"
              , caseExpression
              ]
                |> Pretty.lines
                |> optionalGroup alwaysBreak
                |> Pretty.nest indent
            , Token.keyword "of"
            ]
                |> Pretty.lines
                |> optionalGroup alwaysBreak

        prettyCase ( pattern, expr ) =
            prettyPattern (denode pattern)
                |> Pretty.a (Pretty.string " ->")
                |> Pretty.a Pretty.line
                |> Pretty.a (prettyExpressionInner topContext 4 (denode expr) |> Tuple.first |> Pretty.indent 4)
                |> Pretty.indent indent

        patternsPart =
            List.map prettyCase caseBlock.cases
                |> doubleLines
    in
    ( [ casePart, patternsPart ]
        |> Pretty.lines
        |> Pretty.align
    , True
    )


prettyLambdaExpression : Int -> Lambda -> ( Doc Token, Bool )
prettyLambdaExpression indent lambda =
    let
        ( prettyExpr, alwaysBreak ) =
            prettyExpressionInner topContext 4 (denode lambda.expression)
    in
    ( [ Pretty.string "\\"
            |> Pretty.a (List.map (prettyPatternInner False) (denodeAll lambda.args) |> Pretty.words)
            |> Pretty.a (Pretty.string " ->")
      , prettyExpr
      ]
        |> Pretty.lines
        |> Pretty.nest indent
        |> Pretty.align
        |> optionalGroup alwaysBreak
    , alwaysBreak
    )


prettyRecordExpr : List (Node RecordSetter) -> ( Doc Token, Bool )
prettyRecordExpr setters =
    let
        open =
            Pretty.a Pretty.space (Pretty.string "{")

        close =
            Pretty.a (Pretty.string "}")
                Pretty.line
    in
    case setters of
        [] ->
            ( Pretty.string "{}", False )

        _ ->
            let
                ( prettyExpressions, alwaysBreak ) =
                    List.map prettySetter (denodeAll setters)
                        |> List.unzip
                        |> Tuple.mapSecond Bool.Extra.any
            in
            ( prettyExpressions
                |> Pretty.separators ", "
                |> Pretty.surround open close
                |> Pretty.align
                |> optionalGroup alwaysBreak
            , alwaysBreak
            )


prettySetter : ( Node String, Node Expression ) -> ( Doc Token, Bool )
prettySetter ( fld, val ) =
    let
        ( prettyExpr, alwaysBreak ) =
            prettyExpressionInner topContext 4 (denode val)
    in
    ( [ [ Pretty.string (denode fld)
        , Pretty.string "="
        ]
            |> Pretty.words
      , prettyExpr
      ]
        |> Pretty.lines
        |> optionalGroup alwaysBreak
        |> Pretty.nest 4
    , alwaysBreak
    )


prettyList : Int -> List (Node Expression) -> ( Doc Token, Bool )
prettyList indent exprs =
    let
        open =
            Pretty.a Pretty.space (Pretty.string "[")

        close =
            Pretty.a (Pretty.string "]") Pretty.line
    in
    case exprs of
        [] ->
            ( Pretty.string "[]", False )

        _ ->
            let
                ( prettyExpressions, alwaysBreak ) =
                    List.map (prettyExpressionInner topContext (decrementIndent indent 2)) (denodeAll exprs)
                        |> List.unzip
                        |> Tuple.mapSecond Bool.Extra.any
            in
            ( prettyExpressions
                |> Pretty.separators ", "
                |> Pretty.surround open close
                |> Pretty.align
                |> optionalGroup alwaysBreak
            , alwaysBreak
            )


prettyRecordAccess : Node Expression -> Node String -> ( Doc Token, Bool )
prettyRecordAccess expr field =
    let
        ( prettyExpr, alwaysBreak ) =
            prettyExpressionInner topContext 4 (denode expr)
    in
    ( prettyExpr
        |> Pretty.a dot
        |> Pretty.a (Token.plain (denode field))
    , alwaysBreak
    )


prettyRecordUpdateExpression : Int -> Node String -> List (Node RecordSetter) -> ( Doc Token, Bool )
prettyRecordUpdateExpression indent var setters =
    let
        open =
            [ Pretty.string "{"
            , Token.plain (denode var)
            ]
                |> Pretty.words
                |> Pretty.a Pretty.line

        close =
            Pretty.a (Pretty.string "}")
                Pretty.line

        addBarToFirst exprs =
            case exprs of
                [] ->
                    []

                hd :: tl ->
                    Pretty.a hd (Pretty.string "| ") :: tl
    in
    case setters of
        [] ->
            ( Pretty.string "{}", False )

        _ ->
            let
                ( prettyExpressions, alwaysBreak ) =
                    List.map prettySetter (denodeAll setters)
                        |> List.unzip
                        |> Tuple.mapSecond Bool.Extra.any
            in
            ( open
                |> Pretty.a
                    (prettyExpressions
                        |> addBarToFirst
                        |> Pretty.separators ", "
                    )
                |> Pretty.nest indent
                |> Pretty.surround Pretty.empty close
                |> Pretty.align
                |> optionalGroup alwaysBreak
            , alwaysBreak
            )



--== Type Annotations


{-| Pretty prints a type annotation.
-}
prettyTypeAnnotation : TypeAnnotation -> Doc Token
prettyTypeAnnotation typeAnn =
    case typeAnn of
        GenericType val ->
            Token.plain val

        Typed fqName anns ->
            prettyTyped fqName anns

        Unit ->
            Pretty.string "()"

        Tupled anns ->
            prettyTupled anns

        Record recordDef ->
            prettyRecord (denodeAll recordDef)

        GenericRecord paramName recordDef ->
            prettyGenericRecord (denode paramName) (denodeAll (denode recordDef))

        FunctionTypeAnnotation fromAnn toAnn ->
            prettyFunctionTypeAnnotation fromAnn toAnn


prettyTyped : Node ( ModuleName, String ) -> List (Node TypeAnnotation) -> Doc Token
prettyTyped fqName anns =
    let
        ( moduleName, typeName ) =
            denode fqName

        typeDoc =
            prettyModuleNameDot moduleName
                |> Pretty.a (Token.type_ typeName)

        argsDoc =
            List.map prettyTypeAnnotationParens (denodeAll anns)
                |> Pretty.words
    in
    [ typeDoc
    , argsDoc
    ]
        |> Pretty.words


prettyTupled : List (Node TypeAnnotation) -> Doc Token
prettyTupled anns =
    Pretty.space
        |> Pretty.a
            (List.map prettyTypeAnnotation (denodeAll anns)
                |> Pretty.join (Pretty.string ", ")
            )
        |> Pretty.a Pretty.space
        |> Pretty.parens


prettyTypeAnnotationParens : TypeAnnotation -> Doc Token
prettyTypeAnnotationParens typeAnn =
    if isNakedCompound typeAnn then
        prettyTypeAnnotation typeAnn |> Pretty.parens

    else
        prettyTypeAnnotation typeAnn


prettyRecord : List RecordField -> Doc Token
prettyRecord fields =
    let
        open =
            Pretty.a Pretty.space (Pretty.string "{")

        close =
            Pretty.a (Pretty.string "}") Pretty.line
    in
    case fields of
        [] ->
            Pretty.string "{}"

        _ ->
            fields
                |> List.map (Tuple.mapBoth denode denode)
                |> List.map prettyFieldTypeAnn
                |> Pretty.separators ", "
                |> Pretty.surround open close
                |> Pretty.group


prettyGenericRecord : String -> List RecordField -> Doc Token
prettyGenericRecord paramName fields =
    let
        open =
            [ Pretty.string "{"
            , Token.plain paramName
            ]
                |> Pretty.words
                |> Pretty.a Pretty.line

        close =
            Pretty.a (Pretty.string "}")
                Pretty.line

        addBarToFirst exprs =
            case exprs of
                [] ->
                    []

                hd :: tl ->
                    Pretty.a hd (Pretty.string "| ") :: tl
    in
    case fields of
        [] ->
            Pretty.string "{}"

        _ ->
            open
                |> Pretty.a
                    (fields
                        |> List.map (Tuple.mapBoth denode denode)
                        |> List.map prettyFieldTypeAnn
                        |> addBarToFirst
                        |> Pretty.separators ", "
                    )
                |> Pretty.nest 4
                |> Pretty.surround Pretty.empty close
                |> Pretty.group


prettyFieldTypeAnn : ( String, TypeAnnotation ) -> Doc Token
prettyFieldTypeAnn ( name, ann ) =
    [ [ Token.plain name
      , Pretty.string ":"
      ]
        |> Pretty.words
    , prettyTypeAnnotation ann
    ]
        |> Pretty.lines
        |> Pretty.nest 4
        |> Pretty.group


prettyFunctionTypeAnnotation : Node TypeAnnotation -> Node TypeAnnotation -> Doc Token
prettyFunctionTypeAnnotation left right =
    let
        expandLeft : TypeAnnotation -> Doc Token
        expandLeft ann =
            case ann of
                FunctionTypeAnnotation _ _ ->
                    prettyTypeAnnotationParens ann

                _ ->
                    prettyTypeAnnotation ann

        expandRight : TypeAnnotation -> List (Doc Token)
        expandRight ann =
            case ann of
                FunctionTypeAnnotation innerLeft innerRight ->
                    innerFnTypeAnn innerLeft innerRight

                _ ->
                    [ prettyTypeAnnotation ann ]

        innerFnTypeAnn : Node TypeAnnotation -> Node TypeAnnotation -> List (Doc Token)
        innerFnTypeAnn innerLeft innerRight =
            let
                rightSide =
                    denode innerRight |> expandRight
            in
            case rightSide of
                hd :: tl ->
                    (denode innerLeft |> expandLeft)
                        :: ([ Pretty.string "->", hd ] |> Pretty.words)
                        :: tl

                [] ->
                    []
    in
    innerFnTypeAnn left right
        |> Pretty.lines
        |> Pretty.group


{-| A type annotation is a naked compound if it is made up of multiple parts that
are not enclosed in brackets or braces. This means either a type or type alias with
arguments or a function type; records and tuples are compound but enclosed in brackets
or braces.

Naked type annotations need to be bracketed in situations type argument bindings are
ambiguous otherwise.

-}
isNakedCompound : TypeAnnotation -> Bool
isNakedCompound typeAnn =
    case typeAnn of
        Typed _ [] ->
            False

        Typed _ args ->
            True

        FunctionTypeAnnotation _ _ ->
            True

        _ ->
            False



--== Helpers


prettyMaybe : (a -> Doc Token) -> Maybe a -> Doc Token
prettyMaybe prettyFn maybeVal =
    Maybe.map prettyFn maybeVal
        |> Maybe.withDefault Pretty.empty


decrementIndent : Int -> Int -> Int
decrementIndent currentIndent spaces =
    let
        modded =
            modBy 4 (currentIndent - spaces)
    in
    if modded == 0 then
        4

    else
        modded


dot : Doc Token
dot =
    Pretty.string "."


quotes : Doc Token -> Doc Token
quotes doc =
    Pretty.surround (Pretty.char '"') (Pretty.char '"') doc


tripleQuotes : Doc Token -> Doc Token
tripleQuotes doc =
    Pretty.surround (Pretty.string "\"\"\"") (Pretty.string "\"\"\"") doc


singleQuotes : Doc Token -> Doc Token
singleQuotes doc =
    Pretty.surround (Pretty.char '\'') (Pretty.char '\'') doc


sqParens : Doc Token -> Doc Token
sqParens doc =
    Pretty.surround (Pretty.string "[") (Pretty.string "]") doc


doubleLines : List (Doc Token) -> Doc Token
doubleLines =
    Pretty.join (Pretty.a Pretty.line Pretty.line)


escape : String -> String
escape val =
    val
        |> String.replace "\\" "\\\\"
        |> String.replace "\"" "\\\""
        |> String.replace "\n" "\\n"
        |> String.replace "\t" "\\t"


escapeChar : Char -> String
escapeChar val =
    case val of
        '\\' ->
            "\\\\"

        '\'' ->
            "\\'"

        '\t' ->
            "\\t"

        '\n' ->
            "\\n"

        c ->
            String.fromChar c


optionalGroup : Bool -> Doc Token -> Doc Token
optionalGroup flag doc =
    if flag then
        doc

    else
        Pretty.group doc


optionalParens : Bool -> Doc Token -> Doc Token
optionalParens flag doc =
    if flag then
        Pretty.parens doc

    else
        doc


toHexString : Int -> String
toHexString val =
    let
        padWithZeros str =
            let
                length =
                    String.length str
            in
            if length < 2 then
                String.padLeft 2 '0' str

            else if length > 2 && length < 4 then
                String.padLeft 4 '0' str

            else if length > 4 && length < 8 then
                String.padLeft 8 '0' str

            else
                str
    in
    "0x" ++ (Hex.toString val |> String.toUpper |> padWithZeros)


{-| Calculate a precedence for any operator to be able to know when
parenthesis are needed or not.

When a lower precedence expression appears beneath a higher one, its needs
parenthesis.

When a higher precedence expression appears beneath a lower one, if should
not have parenthesis.

-}
precedence : String -> Int
precedence symbol =
    case symbol of
        ">>" ->
            9

        "<<" ->
            9

        "^" ->
            8

        "*" ->
            7

        "/" ->
            7

        "//" ->
            7

        "%" ->
            7

        "rem" ->
            7

        "+" ->
            6

        "-" ->
            6

        "++" ->
            5

        "::" ->
            5

        "==" ->
            4

        "/=" ->
            4

        "<" ->
            4

        ">" ->
            4

        "<=" ->
            4

        ">=" ->
            4

        "&&" ->
            3

        "||" ->
            2

        "|>" ->
            0

        "<|" ->
            0

        _ ->
            0
