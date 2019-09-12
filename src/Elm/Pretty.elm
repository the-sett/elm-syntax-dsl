module Elm.Pretty exposing (pretty)

{-| Elm.Pretty is a pretty printer for Elm syntax trees. It makes use of
`the-sett/elm-pretty-printer` to best fit the code to a given character width.

It aims to output code that is fully stable with respect to `elm-format` in the
sense that running `elm-format` on the output should have no effect at all. The
advantage of this is that if generated code moves to being edited by hand, there
will not be a large white-space only diff created when `elm-format` is applied.

@docs pretty

-}

import Bool.Extra
import Elm.Syntax.Comments exposing (Comment)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Documentation exposing (Documentation)
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
import Hex
import ImportsAndExposing
import Maybe.Extra
import Pretty exposing (Doc)



--== File Headers


{-| Pretty prints a file of Elm code.
-}
pretty : File -> Doc
pretty file =
    let
        importsPretty =
            case file.imports of
                [] ->
                    Pretty.line

                _ ->
                    prettyImports (denodeAll file.imports)
                        |> Pretty.a Pretty.line
                        |> Pretty.a Pretty.line
                        |> Pretty.a Pretty.line
    in
    prettyModule (denode file.moduleDefinition)
        |> Pretty.a Pretty.line
        |> Pretty.a Pretty.line
        |> Pretty.a (prettyComments (denodeAll file.comments))
        |> Pretty.a importsPretty
        |> Pretty.a (prettyDeclarations (denodeAll file.declarations))


prettyModule : Module -> Doc
prettyModule mod =
    case mod of
        NormalModule defaultModuleData ->
            prettyDefaultModuleData defaultModuleData

        PortModule defaultModuleData ->
            prettyPortModuleData defaultModuleData

        EffectModule effectModuleData ->
            prettyEffectModuleData effectModuleData


prettyModuleName : ModuleName -> Doc
prettyModuleName name =
    List.map Pretty.string name
        |> Pretty.join dot


prettyModuleNameDot : ModuleName -> Doc
prettyModuleNameDot name =
    case name of
        [] ->
            Pretty.empty

        _ ->
            List.map Pretty.string name
                |> Pretty.join dot
                |> Pretty.a dot


prettyModuleNameAlias : ModuleName -> Doc
prettyModuleNameAlias name =
    case name of
        [] ->
            Pretty.empty

        _ ->
            Pretty.string "as "
                |> Pretty.a (List.map Pretty.string name |> Pretty.join dot)


prettyDefaultModuleData : DefaultModuleData -> Doc
prettyDefaultModuleData moduleData =
    Pretty.words
        [ Pretty.string "module"
        , prettyModuleName (denode moduleData.moduleName)
        , prettyExposing (denode moduleData.exposingList)
        ]


prettyPortModuleData : DefaultModuleData -> Doc
prettyPortModuleData moduleData =
    Pretty.words
        [ Pretty.string "port module"
        , prettyModuleName (denode moduleData.moduleName)
        , prettyExposing (denode moduleData.exposingList)
        ]


prettyEffectModuleData : EffectModuleData -> Doc
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
        [ Pretty.string "effect module"
        , prettyModuleName (denode moduleData.moduleName)
        , prettyCmdAndSub (denodeMaybe moduleData.command) (denodeMaybe moduleData.subscription)
            |> prettyMaybe identity
        , prettyExposing (denode moduleData.exposingList)
        ]


prettyComments : List Comment -> Doc
prettyComments comments =
    -- List.map Pretty.string comments
    --     |> Pretty.lines
    Pretty.empty


prettyImports : List Import -> Doc
prettyImports imports =
    ImportsAndExposing.sortAndDedupImports imports
        |> List.map prettyImport
        |> Pretty.lines


prettyImport : Import -> Doc
prettyImport import_ =
    Pretty.join Pretty.space
        [ Pretty.string "import"
        , prettyModuleName (denode import_.moduleName)
        , prettyMaybe prettyModuleNameAlias (denodeMaybe import_.moduleAlias)
        , prettyMaybe prettyExposing (denodeMaybe import_.exposingList)
        ]


prettyExposing : Exposing -> Doc
prettyExposing exposing_ =
    let
        exposings =
            case exposing_ of
                All _ ->
                    Pretty.string ".." |> Pretty.parens

                Explicit tll ->
                    prettyTopLevelExposes (denodeAll tll)
                        |> Pretty.parens
    in
    Pretty.string "exposing"
        |> Pretty.a Pretty.space
        |> Pretty.a exposings


prettyTopLevelExposes : List TopLevelExpose -> Doc
prettyTopLevelExposes exposes =
    let
        tleName tle =
            case tle of
                InfixExpose val ->
                    val

                FunctionExpose val ->
                    val

                TypeOrAliasExpose val ->
                    val

                TypeExpose exposedType ->
                    exposedType.name
    in
    List.sortBy tleName exposes
        |> List.map prettyTopLevelExpose
        |> Pretty.join (Pretty.string ", ")


prettyTopLevelExpose : TopLevelExpose -> Doc
prettyTopLevelExpose tlExpose =
    case tlExpose of
        InfixExpose val ->
            Pretty.string val
                |> Pretty.parens

        FunctionExpose val ->
            Pretty.string val

        TypeOrAliasExpose val ->
            Pretty.string val

        TypeExpose exposedType ->
            case exposedType.open of
                Nothing ->
                    Pretty.string exposedType.name

                Just _ ->
                    Pretty.string exposedType.name
                        |> Pretty.a (Pretty.string "(..)")



--== Declarations


prettyDeclarations : List Declaration -> Doc
prettyDeclarations decls =
    List.map
        (\decl ->
            prettyDeclaration decl
                |> Pretty.a Pretty.line
        )
        decls
        |> doubleLines


prettyDeclaration : Declaration -> Doc
prettyDeclaration decl =
    case decl of
        FunctionDeclaration fn ->
            prettyFun fn

        AliasDeclaration tAlias ->
            prettyTypeAlias tAlias

        CustomTypeDeclaration type_ ->
            prettyCustomType type_

        PortDeclaration sig ->
            [ Pretty.string "port"
            , prettySignature sig
            ]
                |> Pretty.words

        InfixDeclaration infix_ ->
            prettyInfix infix_

        Destructuring pattern expr ->
            [ [ prettyPattern (denode pattern)
              , Pretty.string "="
              ]
                |> Pretty.words
            , prettyExpression (denode expr)
            ]
                |> Pretty.lines
                |> Pretty.nest 4


prettyFun : Function -> Doc
prettyFun fn =
    Pretty.lines
        [ prettyMaybe prettyDocumentation (denodeMaybe fn.documentation)
        , prettyMaybe prettySignature (denodeMaybe fn.signature)
        , prettyFunctionImplementation (denode fn.declaration)
        ]


prettyTypeAlias : TypeAlias -> Doc
prettyTypeAlias tAlias =
    [ prettyMaybe prettyDocumentation (denodeMaybe tAlias.documentation)
    , Pretty.string "type alias"
    , Pretty.string (denode tAlias.name)
    , List.map Pretty.string (denodeAll tAlias.generics) |> Pretty.words
    , Pretty.string "="
    ]
        |> Pretty.words
        |> Pretty.a Pretty.line
        |> Pretty.a (prettyTypeAnnotation (denode tAlias.typeAnnotation))
        |> Pretty.nest 4


prettyCustomType : Type -> Doc
prettyCustomType type_ =
    [ prettyMaybe prettyDocumentation (denodeMaybe type_.documentation)
    , Pretty.string "type"
    , Pretty.string (denode type_.name)
    , List.map Pretty.string (denodeAll type_.generics) |> Pretty.words
    ]
        |> Pretty.words
        |> Pretty.a Pretty.line
        |> Pretty.a (Pretty.string "= ")
        |> Pretty.a (prettyValueConstructors (denodeAll type_.constructors))
        |> Pretty.nest 4


prettyValueConstructors : List ValueConstructor -> Doc
prettyValueConstructors constructors =
    List.map prettyValueConstructor constructors
        |> Pretty.join (Pretty.line |> Pretty.a (Pretty.string "| "))


prettyValueConstructor : ValueConstructor -> Doc
prettyValueConstructor cons =
    [ Pretty.string (denode cons.name)
    , List.map prettyTypeAnnotationParens (denodeAll cons.arguments) |> Pretty.words
    ]
        |> Pretty.words


prettyInfix : Infix -> Doc
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


prettyDocumentation : Documentation -> Doc
prettyDocumentation docs =
    --Pretty.string docs
    Pretty.empty


prettySignature : Signature -> Doc
prettySignature sig =
    [ [ Pretty.string (denode sig.name)
      , Pretty.string ":"
      ]
        |> Pretty.words
    , prettyTypeAnnotation (denode sig.typeAnnotation)
    ]
        |> Pretty.lines
        |> Pretty.nest 4
        |> Pretty.group


prettyFunctionImplementation : FunctionImplementation -> Doc
prettyFunctionImplementation impl =
    Pretty.words
        [ Pretty.string (denode impl.name)
        , prettyArgs (denodeAll impl.arguments)
        , Pretty.string "="
        ]
        |> Pretty.a Pretty.line
        |> Pretty.a (prettyExpression (denode impl.expression))
        |> Pretty.nest 4


prettyArgs : List Pattern -> Doc
prettyArgs args =
    List.map prettyPattern args
        |> Pretty.words



--== Patterns


prettyPattern : Pattern -> Doc
prettyPattern pattern =
    prettyPatternInner True pattern


adjustPatternParentheses : Bool -> Pattern -> Pattern
adjustPatternParentheses isTop pattern =
    let
        addParens pat =
            case ( isTop, pat ) of
                ( False, NamedPattern qnRef (_ :: _) ) ->
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
                ( _, NamedPattern _ _ ) ->
                    False

                ( _, _ ) ->
                    isTop
    in
    removeParens pattern
        |> addParens


prettyPatternInner : Bool -> Pattern -> Doc
prettyPatternInner isTop pattern =
    case adjustPatternParentheses isTop pattern of
        AllPattern ->
            Pretty.string "_"

        UnitPattern ->
            Pretty.string "()"

        CharPattern val ->
            Pretty.string (escapeChar val)
                |> singleQuotes

        StringPattern val ->
            Pretty.string val
                |> quotes

        IntPattern val ->
            Pretty.string (String.fromInt val)

        HexPattern val ->
            Pretty.string (Hex.toString val)

        FloatPattern val ->
            Pretty.string (String.fromFloat val)

        TuplePattern vals ->
            Pretty.space
                |> Pretty.a
                    (List.map (prettyPatternInner False) (denodeAll vals)
                        |> Pretty.join (Pretty.string ", ")
                    )
                |> Pretty.a Pretty.space
                |> Pretty.parens

        RecordPattern fields ->
            List.map Pretty.string (denodeAll fields)
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
                    List.map (prettyPatternInner False) (denodeAll listPats)
                        |> Pretty.join (Pretty.string ", ")
                        |> sqParens

        VarPattern var ->
            Pretty.string var

        NamedPattern qnRef listPats ->
            (prettyModuleNameDot qnRef.moduleName
                |> Pretty.a (Pretty.string qnRef.name)
            )
                :: List.map (prettyPatternInner False) (denodeAll listPats)
                |> Pretty.words

        AsPattern pat name ->
            [ prettyPatternInner False (denode pat)
            , Pretty.string "as"
            , Pretty.string (denode name)
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


prettyExpression : Expression -> Doc
prettyExpression expression =
    prettyExpressionInner topContext 4 expression
        |> Tuple.first


prettyExpressionInner : Context -> Int -> Expression -> ( Doc, Bool )
prettyExpressionInner context indent expression =
    case adjustExpressionParentheses context expression of
        UnitExpr ->
            ( Pretty.string "()"
            , False
            )

        Application exprs ->
            prettyApplication exprs

        OperatorApplication symbol dir exprl exprr ->
            prettyOperatorApplication symbol dir exprl exprr

        FunctionOrValue modl val ->
            ( prettyModuleNameDot modl
                |> Pretty.a (Pretty.string val)
            , False
            )

        IfBlock exprBool exprTrue exprFalse ->
            ( prettyIfBlock indent exprBool exprTrue exprFalse
            , True
            )

        PrefixOperator symbol ->
            ( Pretty.string symbol |> Pretty.parens
            , False
            )

        Operator symbol ->
            ( Pretty.string symbol
            , False
            )

        Integer val ->
            ( Pretty.string (String.fromInt val)
            , False
            )

        Hex val ->
            ( Pretty.string (Hex.toString val)
            , False
            )

        Floatable val ->
            ( Pretty.string (String.fromFloat val)
            , False
            )

        Negation expr ->
            let
                ( prettyExpr, alwaysBreak ) =
                    prettyExpressionInner topContext 4 (denode expr)
            in
            ( Pretty.string "-"
                |> Pretty.a prettyExpr
            , alwaysBreak
            )

        Literal val ->
            ( prettyLiteral val
            , False
            )

        CharLiteral val ->
            ( Pretty.string (escapeChar val)
                |> singleQuotes
            , False
            )

        TupledExpression exprs ->
            prettyTupledExpression exprs

        ParenthesizedExpression expr ->
            prettyParenthesizedExpression indent expr

        LetExpression letBlock ->
            prettyLetBlock indent letBlock

        CaseExpression caseBlock ->
            prettyCaseBlock caseBlock

        LambdaExpression lambda ->
            prettyLambdaExpression lambda

        RecordExpr setters ->
            prettyRecordExpr setters

        ListExpr exprs ->
            prettyList exprs

        RecordAccess expr field ->
            prettyRecordAccess expr field

        RecordAccessFunction field ->
            ( Pretty.string field
            , False
            )

        RecordUpdateExpression var setters ->
            prettyRecordUpdateExpression var setters

        GLSLExpression val ->
            ( Debug.todo "glsl"
            , True
            )


prettyApplication : List (Node Expression) -> ( Doc, Bool )
prettyApplication exprs =
    let
        ( prettyExpressions, alwaysBreak ) =
            List.map (prettyExpressionInner { precedence = 11, isTop = False, isLeftPipe = False } 4) (denodeAll exprs)
                |> List.unzip
                |> Tuple.mapSecond Bool.Extra.any
    in
    ( prettyExpressions
        |> Pretty.lines
        |> optionalGroup alwaysBreak
        |> Pretty.nest 4
    , alwaysBreak
    )


isEndLineOperator : String -> Bool
isEndLineOperator op =
    case op of
        "<|" ->
            True

        _ ->
            False


prettyOperatorApplication : String -> InfixDirection -> Node Expression -> Node Expression -> ( Doc, Bool )
prettyOperatorApplication symbol _ exprl exprr =
    let
        expandExpr : Int -> Context -> Expression -> List ( Doc, Bool )
        expandExpr indent context expr =
            case expr of
                OperatorApplication sym _ left right ->
                    innerOpApply sym left right

                _ ->
                    [ prettyExpressionInner context indent expr ]

        innerOpApply : String -> Node Expression -> Node Expression -> List ( Doc, Bool )
        innerOpApply sym left right =
            let
                context =
                    { precedence = precedence sym
                    , isTop = False
                    , isLeftPipe = "<|" == sym
                    }

                indent =
                    4 - (String.length symbol + 1)

                rightSide =
                    denode right |> expandExpr indent context
            in
            case rightSide of
                ( hdExpr, hdBreak ) :: tl ->
                    List.append (denode left |> expandExpr 4 context)
                        (( Pretty.string sym |> Pretty.a Pretty.space |> Pretty.a hdExpr, hdBreak ) :: tl)

                [] ->
                    []

        ( prettyExpressions, alwaysBreak ) =
            innerOpApply symbol exprl exprr
                |> List.unzip
                |> Tuple.mapSecond Bool.Extra.any
    in
    ( prettyExpressions
        |> Pretty.join (Pretty.nest 4 Pretty.line)
        |> optionalGroup alwaysBreak
    , alwaysBreak
    )


prettyIfBlock : Int -> Node Expression -> Node Expression -> Node Expression -> Doc
prettyIfBlock indent exprBool exprTrue exprFalse =
    [ [ Pretty.string "if"
      , prettyExpressionInner topContext 4 (denode exprBool) |> Tuple.first
      , Pretty.string "then"
      ]
        |> Pretty.words
    , prettyExpressionInner topContext 4 (denode exprTrue) |> Tuple.first
    ]
        |> Pretty.lines
        |> Pretty.nest indent
        |> Pretty.a Pretty.line
        |> Pretty.a Pretty.line
        |> Pretty.a
            ([ Pretty.string "else"
             , prettyExpressionInner topContext 4 (denode exprFalse) |> Tuple.first
             ]
                |> Pretty.lines
                |> Pretty.nest indent
            )


prettyLiteral : String -> Doc
prettyLiteral val =
    Pretty.string (escape val)
        |> quotes


prettyTupledExpression : List (Node Expression) -> ( Doc, Bool )
prettyTupledExpression exprs =
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
                    List.map (prettyExpressionInner topContext 4) (denodeAll exprs)
                        |> List.unzip
                        |> Tuple.mapSecond Bool.Extra.any
            in
            ( prettyExpressions
                |> Pretty.separators ", "
                |> Pretty.surround open close
                |> optionalGroup alwaysBreak
            , alwaysBreak
            )


prettyParenthesizedExpression : Int -> Node Expression -> ( Doc, Bool )
prettyParenthesizedExpression indent expr =
    let
        open =
            Pretty.string "("

        close =
            Pretty.a (Pretty.string ")") Pretty.tightline

        ( prettyExpr, alwaysBreak ) =
            prettyExpressionInner topContext (indent - 1) (denode expr)
    in
    ( prettyExpr
        |> Pretty.nest 1
        |> Pretty.surround open close
        |> Pretty.align
        |> optionalGroup alwaysBreak
    , alwaysBreak
    )


prettyLetBlock : Int -> LetBlock -> ( Doc, Bool )
prettyLetBlock indent letBlock =
    ( [ Pretty.string "let"
      , List.map (prettyLetDeclaration indent) (denodeAll letBlock.declarations)
            |> doubleLines
            |> Pretty.indent indent
      , Pretty.string "in"
      , prettyExpressionInner topContext 4 (denode letBlock.expression) |> Tuple.first
      ]
        |> Pretty.lines
      --|> Pretty.align
    , True
    )


prettyLetDeclaration : Int -> LetDeclaration -> Doc
prettyLetDeclaration indent letDecl =
    case letDecl of
        LetFunction fn ->
            prettyFun fn

        LetDestructuring pattern expr ->
            [ prettyPattern (denode pattern)
            , Pretty.string "="
            ]
                |> Pretty.words
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (prettyExpressionInner topContext 4 (denode expr)
                        |> Tuple.first
                        |> Pretty.indent indent
                    )


prettyCaseBlock : CaseBlock -> ( Doc, Bool )
prettyCaseBlock caseBlock =
    ( ([ Pretty.string "case"
       , prettyExpressionInner topContext 4 (denode caseBlock.expression) |> Tuple.first
       , Pretty.string "of"
       ]
        |> Pretty.words
      )
        |> Pretty.a Pretty.line
        |> Pretty.a
            (List.map
                (\( pattern, expr ) ->
                    prettyPattern (denode pattern)
                        |> Pretty.a (Pretty.string " ->")
                        |> Pretty.a Pretty.line
                        |> Pretty.a (prettyExpressionInner topContext 4 (denode expr) |> Tuple.first |> Pretty.indent 4)
                        |> Pretty.indent 4
                )
                caseBlock.cases
                |> doubleLines
            )
    , True
    )


prettyLambdaExpression : Lambda -> ( Doc, Bool )
prettyLambdaExpression lambda =
    let
        ( prettyExpr, alwaysBreak ) =
            prettyExpressionInner topContext 4 (denode lambda.expression)
    in
    ( [ Pretty.string "\\"
            |> Pretty.a (List.map prettyPattern (denodeAll lambda.args) |> Pretty.words)
            |> Pretty.a (Pretty.string " ->")
      , prettyExpr
      ]
        |> Pretty.lines
        |> optionalGroup alwaysBreak
        |> Pretty.nest 4
    , alwaysBreak
    )


prettyRecordExpr : List (Node RecordSetter) -> ( Doc, Bool )
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
                |> optionalGroup alwaysBreak
            , alwaysBreak
            )


prettySetter : ( Node String, Node Expression ) -> ( Doc, Bool )
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


prettyList : List (Node Expression) -> ( Doc, Bool )
prettyList exprs =
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
                    List.map (prettyExpressionInner topContext 4) (denodeAll exprs)
                        |> List.unzip
                        |> Tuple.mapSecond Bool.Extra.any
            in
            ( prettyExpressions
                |> Pretty.separators ", "
                |> Pretty.surround open close
                |> optionalGroup alwaysBreak
            , alwaysBreak
            )


prettyRecordAccess : Node Expression -> Node String -> ( Doc, Bool )
prettyRecordAccess expr field =
    let
        ( prettyExpr, alwaysBreak ) =
            prettyExpressionInner topContext 4 (denode expr)
    in
    ( prettyExpr
        |> Pretty.a dot
        |> Pretty.a (Pretty.string (denode field))
    , alwaysBreak
    )


prettyRecordUpdateExpression : Node String -> List (Node RecordSetter) -> ( Doc, Bool )
prettyRecordUpdateExpression var setters =
    let
        open =
            [ Pretty.string "{"
            , Pretty.string (denode var)
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
                |> Pretty.nest 4
                |> Pretty.surround Pretty.empty close
                |> optionalGroup alwaysBreak
            , alwaysBreak
            )



--== Type Annotations


prettyTypeAnnotation : TypeAnnotation -> Doc
prettyTypeAnnotation typeAnn =
    case typeAnn of
        GenericType val ->
            Pretty.string val

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


prettyTyped : Node ( ModuleName, String ) -> List (Node TypeAnnotation) -> Doc
prettyTyped fqName anns =
    let
        ( moduleName, typeName ) =
            denode fqName

        typeDoc =
            prettyModuleNameDot moduleName
                |> Pretty.a (Pretty.string typeName)

        argsDoc =
            List.map prettyTypeAnnotationParens (denodeAll anns)
                |> Pretty.words
    in
    [ typeDoc
    , argsDoc
    ]
        |> Pretty.words


prettyTupled : List (Node TypeAnnotation) -> Doc
prettyTupled anns =
    Pretty.space
        |> Pretty.a
            (List.map prettyTypeAnnotation (denodeAll anns)
                |> Pretty.join (Pretty.string ", ")
            )
        |> Pretty.a Pretty.space
        |> Pretty.parens


prettyTypeAnnotationParens : TypeAnnotation -> Doc
prettyTypeAnnotationParens typeAnn =
    if isNakedCompound typeAnn then
        prettyTypeAnnotation typeAnn |> Pretty.parens

    else
        prettyTypeAnnotation typeAnn


prettyRecord : List RecordField -> Doc
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


prettyGenericRecord : String -> List RecordField -> Doc
prettyGenericRecord paramName fields =
    let
        open =
            [ Pretty.string "{"
            , Pretty.string paramName
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


prettyFieldTypeAnn : ( String, TypeAnnotation ) -> Doc
prettyFieldTypeAnn ( name, ann ) =
    [ [ Pretty.string name
      , Pretty.string ":"
      ]
        |> Pretty.words
    , prettyTypeAnnotation ann
    ]
        |> Pretty.lines
        |> Pretty.nest 4
        |> Pretty.group


prettyFunctionTypeAnnotation : Node TypeAnnotation -> Node TypeAnnotation -> Doc
prettyFunctionTypeAnnotation left right =
    let
        expandLeft : TypeAnnotation -> Doc
        expandLeft ann =
            case ann of
                FunctionTypeAnnotation _ _ ->
                    prettyTypeAnnotationParens ann

                _ ->
                    prettyTypeAnnotation ann

        expandRight : TypeAnnotation -> List Doc
        expandRight ann =
            case ann of
                FunctionTypeAnnotation innerLeft innerRight ->
                    innerFnTypeAnn innerLeft innerRight

                _ ->
                    [ prettyTypeAnnotation ann ]

        innerFnTypeAnn : Node TypeAnnotation -> Node TypeAnnotation -> List Doc
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


denode : Node a -> a
denode =
    Node.value


denodeAll : List (Node a) -> List a
denodeAll =
    List.map denode


denodeMaybe : Maybe (Node a) -> Maybe a
denodeMaybe =
    Maybe.map denode


nodify : a -> Node a
nodify exp =
    Node emptyRange exp


prettyMaybe : (a -> Doc) -> Maybe a -> Doc
prettyMaybe prettyFn maybeVal =
    Maybe.map prettyFn maybeVal
        |> Maybe.withDefault Pretty.empty


dot : Doc
dot =
    Pretty.string "."


quotes : Doc -> Doc
quotes doc =
    Pretty.surround (Pretty.char '"') (Pretty.char '"') doc


tripleQuotes : Doc -> Doc
tripleQuotes doc =
    Pretty.surround (Pretty.string "\"\"\"") (Pretty.string "\"\"\"") doc


singleQuotes : Doc -> Doc
singleQuotes doc =
    Pretty.surround (Pretty.char '\'') (Pretty.char '\'') doc


sqParens : Doc -> Doc
sqParens doc =
    Pretty.surround (Pretty.string "[") (Pretty.string "]") doc


doubleLines : List Doc -> Doc
doubleLines =
    Pretty.join (Pretty.a Pretty.line Pretty.line)


escape : String -> String
escape val =
    val
        |> String.replace "\\" "\\\\"
        |> String.replace "\"" "\\\""
        |> String.replace "\n" "\\n"


escapeChar : Char -> String
escapeChar val =
    case val of
        '\'' ->
            "\\'"

        c ->
            String.fromChar c


optionalGroup : Bool -> Doc -> Doc
optionalGroup flag doc =
    if flag then
        doc

    else
        Pretty.group doc


optionalParens : Bool -> Doc -> Doc
optionalParens flag doc =
    if flag then
        Pretty.parens doc

    else
        doc


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
