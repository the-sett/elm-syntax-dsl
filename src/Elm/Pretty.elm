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
import Pretty exposing (Doc)


{-| Pretty prints a file of Elm code.
-}
pretty : File -> Doc
pretty file =
    prettyModule (denode file.moduleDefinition)
        |> Pretty.a Pretty.line
        |> Pretty.a Pretty.line
        |> Pretty.a (prettyComments (denodeAll file.comments))
        |> Pretty.a (prettyImports (denodeAll file.imports))
        |> Pretty.a Pretty.line
        |> Pretty.a Pretty.line
        |> Pretty.a Pretty.line
        |> Pretty.a (prettyDeclarations (denodeAll file.declarations))


prettyModule : Module -> Doc
prettyModule mod =
    case mod of
        NormalModule defaultModuleData ->
            prettyDefaultModuleData defaultModuleData

        PortModule defaultModuleData ->
            prettyDefaultModuleData defaultModuleData

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


prettyEffectModuleData : EffectModuleData -> Doc
prettyEffectModuleData moduleData =
    Pretty.words
        [ Pretty.string "module"
        , prettyModuleName (denode moduleData.moduleName)
        , prettyExposing (denode moduleData.exposingList)
        , prettyMaybe Pretty.string (denodeMaybe moduleData.command)
        , prettyMaybe Pretty.string (denodeMaybe moduleData.subscription)
        ]


prettyComments : List Comment -> Doc
prettyComments comments =
    -- List.map Pretty.string comments
    --     |> Pretty.lines
    Pretty.empty


prettyImports : List Import -> Doc
prettyImports imports =
    List.map prettyImport imports
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
            Pretty.string "sig"

        InfixDeclaration infix_ ->
            prettyInfix infix_

        Destructuring pattern expr ->
            [ prettyPattern (denode pattern)
            , Pretty.string "="
            , prettyExpression (denode expr)
            ]
                |> Pretty.words


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
    , List.map prettyTypeAnnotation (denodeAll cons.arguments) |> Pretty.words
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
    [ Pretty.string (denode sig.name)
    , Pretty.string ":"
    , prettyTypeAnnotation (denode sig.typeAnnotation)
    ]
        |> Pretty.words


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


prettyPattern : Pattern -> Doc
prettyPattern pattern =
    case pattern of
        AllPattern ->
            Pretty.string "_"

        UnitPattern ->
            Pretty.string "()"

        CharPattern val ->
            Pretty.string (escapeChar val)
                |> singleQuotes

        StringPattern val ->
            Pretty.string val

        IntPattern val ->
            Pretty.string (String.fromInt val)

        HexPattern val ->
            Pretty.string "hexPattern"

        FloatPattern val ->
            Pretty.string (String.fromFloat val)

        TuplePattern vals ->
            Pretty.space
                |> Pretty.a
                    (List.map prettyPattern (denodeAll vals)
                        |> Pretty.join (Pretty.string ", ")
                    )
                |> Pretty.a Pretty.space
                |> Pretty.parens

        RecordPattern fields ->
            List.map Pretty.string (denodeAll fields)
                |> Pretty.join (Pretty.string ", ")
                |> Pretty.braces

        UnConsPattern hdPat tlPat ->
            [ prettyPattern (denode hdPat)
            , Pretty.string "::"
            , prettyPattern (denode tlPat)
            ]
                |> Pretty.words

        ListPattern listPats ->
            case listPats of
                [] ->
                    Pretty.string "[]"

                _ ->
                    List.map prettyPattern (denodeAll listPats)
                        |> Pretty.join (Pretty.string ", ")
                        |> sqParens

        VarPattern var ->
            Pretty.string var

        NamedPattern qnRef listPats ->
            (prettyModuleNameDot qnRef.moduleName
                |> Pretty.a (Pretty.string qnRef.name)
            )
                :: List.map prettyPattern (denodeAll listPats)
                |> Pretty.words

        AsPattern pat name ->
            [ prettyPattern (denode pat)
            , Pretty.string "as"
            , Pretty.string (denode name)
            ]
                |> Pretty.words

        ParenthesizedPattern pat ->
            prettyPattern (denode pat)
                |> Pretty.parens


prettyExpression : Expression -> Doc
prettyExpression expression =
    prettyExpressionInner expression
        |> Tuple.first


prettyExpressionInner : Expression -> ( Doc, Bool )
prettyExpressionInner expression =
    case expression of
        UnitExpr ->
            ( Pretty.string "()"
            , False
            )

        Application exprs ->
            prettyApplication exprs

        OperatorApplication symbol _ exprl exprr ->
            prettyOperatorApplication symbol exprl exprr

        FunctionOrValue modl val ->
            ( prettyModuleNameDot modl
                |> Pretty.a (Pretty.string val)
            , False
            )

        IfBlock exprBool exprTrue exprFalse ->
            ( prettyIfBlock exprBool exprTrue exprFalse
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
            ( Pretty.string "hex"
            , False
            )

        Floatable val ->
            ( Pretty.string (String.fromFloat val)
            , False
            )

        Negation expr ->
            let
                ( prettyExpr, alwaysBreak ) =
                    prettyExpressionInner (denode expr)
            in
            ( Pretty.string "-"
                |> Pretty.a prettyExpr
            , alwaysBreak
            )

        Literal val ->
            ( Pretty.string (escape val)
                |> quotes
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
            prettyParenthesizedExpression expr

        LetExpression letBlock ->
            prettyLetBlock letBlock

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
            ( Pretty.string "glsl"
            , True
            )


prettyApplication : List (Node Expression) -> ( Doc, Bool )
prettyApplication exprs =
    let
        ( prettyExpressions, alwaysBreak ) =
            List.map prettyExpressionInner (denodeAll exprs)
                |> List.unzip
                |> Tuple.mapSecond Bool.Extra.any
    in
    ( prettyExpressions
        |> Pretty.lines
        |> optionalGroup alwaysBreak
        |> Pretty.nest 4
    , alwaysBreak
    )


prettyOperatorApplication : String -> Node Expression -> Node Expression -> ( Doc, Bool )
prettyOperatorApplication symbol exprl exprr =
    let
        expandExpr : Expression -> List ( Doc, Bool )
        expandExpr expr =
            case expr of
                OperatorApplication sym _ left right ->
                    innerOpApply sym left right

                _ ->
                    [ prettyExpressionInner expr ]

        innerOpApply : String -> Node Expression -> Node Expression -> List ( Doc, Bool )
        innerOpApply sym left right =
            let
                rightSide =
                    denode right |> expandExpr
            in
            case rightSide of
                ( hdExpr, hdBreak ) :: tl ->
                    List.append (denode left |> expandExpr)
                        (( Pretty.string symbol |> Pretty.a Pretty.space |> Pretty.a hdExpr, hdBreak ) :: tl)

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


prettyIfBlock : Node Expression -> Node Expression -> Node Expression -> Doc
prettyIfBlock exprBool exprTrue exprFalse =
    [ [ Pretty.string "if"
      , prettyExpressionInner (denode exprBool) |> Tuple.first
      , Pretty.string "then"
      ]
        |> Pretty.words
    , prettyExpressionInner (denode exprTrue) |> Tuple.first
    ]
        |> Pretty.lines
        |> Pretty.nest 4
        |> Pretty.a Pretty.line
        |> Pretty.a Pretty.line
        |> Pretty.a
            ([ Pretty.string "else"
             , prettyExpressionInner (denode exprFalse) |> Tuple.first
             ]
                |> Pretty.lines
                |> Pretty.nest 4
            )


prettyTupledExpression : List (Node Expression) -> ( Doc, Bool )
prettyTupledExpression exprs =
    let
        ( prettyExpressions, alwaysBreak ) =
            List.map prettyExpressionInner (denodeAll exprs)
                |> List.unzip
                |> Tuple.mapSecond Bool.Extra.any
    in
    ( Pretty.space
        |> Pretty.a
            (prettyExpressions
                |> Pretty.join (Pretty.string ", ")
            )
        |> Pretty.a Pretty.space
        |> Pretty.parens
    , alwaysBreak
    )


prettyParenthesizedExpression : Node Expression -> ( Doc, Bool )
prettyParenthesizedExpression expr =
    let
        open =
            Pretty.string "("

        close =
            Pretty.a (Pretty.string ")") Pretty.tightline

        ( prettyExpr, alwaysBreak ) =
            prettyExpressionInner (denode expr)
    in
    ( prettyExpr
        |> Pretty.surround open close
        |> optionalGroup alwaysBreak
    , alwaysBreak
    )


prettyLetBlock : LetBlock -> ( Doc, Bool )
prettyLetBlock letBlock =
    ( [ Pretty.string "let"
      , List.map prettyLetDeclaration (denodeAll letBlock.declarations)
            |> doubleLines
            |> Pretty.indent 4
      , Pretty.string "in"
      , prettyExpressionInner (denode letBlock.expression) |> Tuple.first
      ]
        |> Pretty.lines
    , True
    )


prettyLetDeclaration : LetDeclaration -> Doc
prettyLetDeclaration letDecl =
    case letDecl of
        LetFunction fn ->
            prettyFun fn

        LetDestructuring pattern expr ->
            [ prettyPattern (denode pattern)
            , Pretty.string "="
            ]
                |> Pretty.words
                |> Pretty.a Pretty.line
                |> Pretty.a (prettyExpressionInner (denode expr) |> Tuple.first |> Pretty.indent 4)


prettyCaseBlock : CaseBlock -> ( Doc, Bool )
prettyCaseBlock caseBlock =
    ( ([ Pretty.string "case"
       , prettyExpressionInner (denode caseBlock.expression) |> Tuple.first
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
                        |> Pretty.a (prettyExpressionInner (denode expr) |> Tuple.first |> Pretty.indent 4)
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
            prettyExpressionInner (denode lambda.expression)
    in
    ( [ [ Pretty.string "\\"
            |> Pretty.a (Pretty.string "args")
        , Pretty.string "->"
        ]
            |> Pretty.words
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

        prettySetter : ( Node String, Node Expression ) -> ( Doc, Bool )
        prettySetter ( fld, val ) =
            let
                ( prettyExpr, alwaysBreak ) =
                    prettyExpressionInner (denode val)
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
                    List.map prettyExpressionInner (denodeAll exprs)
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
            prettyExpressionInner (denode expr)
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
            Pretty.string "{"
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string (denode var))
                |> Pretty.a (Pretty.string "|")
                |> Pretty.a Pretty.space

        close =
            Pretty.a (Pretty.string "}")
                Pretty.line

        prettySetter : ( Node String, Node Expression ) -> ( Doc, Bool )
        prettySetter ( fld, val ) =
            let
                ( prettyExpr, alwaysBreak ) =
                    prettyExpressionInner (denode val)
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


prettyTypeAnnotation : TypeAnnotation -> Doc
prettyTypeAnnotation typeAnn =
    case typeAnn of
        GenericType val ->
            Pretty.string val

        Typed fqName anns ->
            let
                ( moduleName, typeName ) =
                    denode fqName

                typeDoc =
                    prettyModuleNameDot moduleName
                        |> Pretty.a (Pretty.string typeName)

                argsDoc =
                    List.map prettyTypeAnnotation (denodeAll anns)
                        |> Pretty.words
            in
            [ typeDoc
            , argsDoc
            ]
                |> Pretty.words

        Unit ->
            Pretty.string "()"

        Tupled anns ->
            Pretty.space
                |> Pretty.a
                    (List.map prettyTypeAnnotation (denodeAll anns)
                        |> Pretty.join (Pretty.string ", ")
                    )
                |> Pretty.a Pretty.space
                |> Pretty.parens

        Record recordDef ->
            Pretty.string "record"

        GenericRecord paramName recordDef ->
            Pretty.string "genrec"

        FunctionTypeAnnotation fromAnn toAnn ->
            [ prettyTypeAnnotation (denode fromAnn)
            , prettyTypeAnnotation (denode toAnn)
            ]
                |> Pretty.join (Pretty.string " -> ")



--== Helpers


denode =
    Node.value


denodeAll =
    List.map denode


denodeMaybe =
    Maybe.map denode


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
    String.replace "\\" "\\\\" val


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
