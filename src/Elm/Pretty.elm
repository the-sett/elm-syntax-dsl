module Elm.Pretty exposing (pretty)

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


pretty : File -> Doc
pretty file =
    Pretty.lines
        [ prettyModule (denode file.moduleDefinition)
        , Pretty.string ""
        , prettyComments (denodeAll file.comments)
        , Pretty.line
        , prettyImports (denodeAll file.imports)
        , Pretty.line
        , prettyDeclarations (denodeAll file.declarations)
        ]


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


prettyDefaultModuleData : DefaultModuleData -> Doc
prettyDefaultModuleData moduleData =
    Pretty.join Pretty.space
        [ Pretty.string "module"
        , prettyModuleName (denode moduleData.moduleName)
        , Pretty.string "exposing"
        , prettyExposing (denode moduleData.exposingList)
        ]


prettyEffectModuleData : EffectModuleData -> Doc
prettyEffectModuleData moduleData =
    Pretty.join Pretty.space
        [ Pretty.string "module"
        , prettyModuleName (denode moduleData.moduleName)
        , prettyExposing (denode moduleData.exposingList)
        , prettyMaybe Pretty.string (denodeMaybe moduleData.command)
        , prettyMaybe Pretty.string (denodeMaybe moduleData.subscription)
        ]


prettyComments : List Comment -> Doc
prettyComments comments =
    List.map Pretty.string comments
        |> Pretty.lines


prettyImports : List Import -> Doc
prettyImports imports =
    List.map prettyImport imports
        |> Pretty.lines


prettyImport : Import -> Doc
prettyImport import_ =
    Pretty.join Pretty.space
        [ Pretty.string "import"
        , prettyModuleName (denode import_.moduleName)
        , prettyMaybe prettyModuleName (denodeMaybe import_.moduleAlias)
        , prettyMaybe prettyExposing (denodeMaybe import_.exposingList)
        ]


prettyExposing : Exposing -> Doc
prettyExposing exposing_ =
    case exposing_ of
        All _ ->
            Pretty.string ".."
                |> Pretty.parens

        Explicit tll ->
            prettyTopLevelExposes (denodeAll tll)
                |> Pretty.parens


prettyTopLevelExposes : List TopLevelExpose -> Doc
prettyTopLevelExposes exposes =
    List.map prettyTopLevelExpose exposes
        |> Pretty.join (Pretty.string ",")


prettyTopLevelExpose : TopLevelExpose -> Doc
prettyTopLevelExpose tlExpose =
    case tlExpose of
        InfixExpose val ->
            Pretty.string val

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
                |> Pretty.a Pretty.line
        )
        decls
        |> Pretty.lines


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
            Pretty.string "infix"

        Destructuring pattern expr ->
            [ prettyPattern (denode pattern)
            , Pretty.string "="
            , prettyExpression (denode expr)
            ]
                |> Pretty.lines
                |> Pretty.group


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
        |> Pretty.hang 4


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
        |> Pretty.hang 4


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


prettyDocumentation : Documentation -> Doc
prettyDocumentation docs =
    Pretty.string docs


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
        |> Pretty.hang 4


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
            Pretty.string (String.fromChar val)

        StringPattern val ->
            Pretty.string val

        IntPattern val ->
            Pretty.string (String.fromInt val)

        HexPattern val ->
            Pretty.string "hexPattern"

        FloatPattern val ->
            Pretty.string (String.fromFloat val)

        TuplePattern vals ->
            List.map prettyPattern (denodeAll vals)
                |> Pretty.join (Pretty.string ", ")
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
            List.map prettyPattern (denodeAll listPats)
                |> Pretty.join (Pretty.string ", ")
                |> sqParens

        VarPattern var ->
            Pretty.string var

        NamedPattern qnRef listPats ->
            prettyModuleNameDot qnRef.moduleName
                |> Pretty.a (Pretty.string qnRef.name)
                |> Pretty.a Pretty.space
                |> Pretty.a
                    (List.map prettyPattern (denodeAll listPats)
                        |> Pretty.words
                    )

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
    case expression of
        UnitExpr ->
            Pretty.string "()"

        Application exprs ->
            List.map prettyExpression (denodeAll exprs)
                |> Pretty.lines
                |> Pretty.group
                |> Pretty.nest 4

        OperatorApplication symbol direction exprl exprr ->
            [ prettyExpression (denode exprl)
            , Pretty.string symbol
                |> Pretty.a Pretty.space
                |> Pretty.a (prettyExpression (denode exprr))
            ]
                |> Pretty.lines
                |> Pretty.group
                |> Pretty.hang 4

        FunctionOrValue modl val ->
            prettyModuleNameDot modl
                |> Pretty.a (Pretty.string val)

        IfBlock exprBool exprTrue exprFalse ->
            [ [ Pretty.string "if"
              , prettyExpression (denode exprBool)
              , Pretty.string "then"
              ]
                |> Pretty.words
            , prettyExpression (denode exprTrue) |> Pretty.indent 4
            , Pretty.string "else"
            , prettyExpression (denode exprFalse) |> Pretty.indent 4
            ]
                |> Pretty.lines

        PrefixOperator symbol ->
            Pretty.string symbol

        Operator symbol ->
            Pretty.string symbol

        Integer val ->
            Pretty.string (String.fromInt val)

        Hex val ->
            Pretty.string "hex"

        Floatable val ->
            Pretty.string (String.fromFloat val)

        Negation expr ->
            Pretty.string "-"
                |> Pretty.a (prettyExpression (denode expr))

        Literal val ->
            Pretty.string val
                |> quotes

        CharLiteral val ->
            Pretty.string (String.fromChar val)
                |> singleQuotes

        TupledExpression exprs ->
            List.map prettyExpression (denodeAll exprs)
                |> Pretty.join (Pretty.string ", ")
                |> Pretty.group
                |> Pretty.parens

        ParenthesizedExpression expr ->
            prettyExpression (denode expr)
                |> Pretty.parens

        LetExpression letBlock ->
            prettyLetBlock letBlock

        CaseExpression caseBlock ->
            prettyCaseBlock caseBlock

        LambdaExpression lambda ->
            [ Pretty.string "\\"
            , Pretty.string "args"
            , Pretty.string "->"
            , prettyExpression (denode lambda.expression)
            ]
                |> Pretty.lines
                |> Pretty.group

        RecordExpr setters ->
            [ Pretty.string "{"
            , List.map
                (\( fld, val ) ->
                    [ Pretty.string (denode fld)
                    , Pretty.string "="
                    , prettyExpression (denode val)
                    ]
                        |> Pretty.join (Pretty.string ", ")
                )
                (denodeAll setters)
                |> Pretty.lines
                |> Pretty.group
            , Pretty.string "}"
            ]
                |> Pretty.lines
                |> Pretty.group

        ListExpr exprs ->
            List.map prettyExpression (denodeAll exprs)
                |> Pretty.lines
                |> Pretty.group
                |> sqParens

        RecordAccess expr field ->
            prettyExpression (denode expr)
                |> Pretty.a dot
                |> Pretty.a (Pretty.string (denode field))

        RecordAccessFunction field ->
            Pretty.a (Pretty.string field) dot

        RecordUpdateExpression var setters ->
            [ Pretty.string "["
            , Pretty.string (denode var)
            , Pretty.string "|"
            , List.map
                (\( fld, val ) ->
                    [ Pretty.string (denode fld)
                    , Pretty.string "="
                    , prettyExpression (denode val)
                    ]
                        |> Pretty.join (Pretty.string ", ")
                )
                (denodeAll setters)
                |> Pretty.lines
                |> Pretty.group
            , Pretty.string "]"
            ]
                |> Pretty.lines
                |> Pretty.group

        GLSLExpression val ->
            Pretty.string "glsl"


prettyLetBlock : LetBlock -> Doc
prettyLetBlock letBlock =
    [ Pretty.string "let"
    , List.map prettyLetDeclaration (denodeAll letBlock.declarations)
        |> Pretty.lines
        |> Pretty.indent 4
    , Pretty.string "in"
    , prettyExpression (denode letBlock.expression)
    ]
        |> Pretty.lines


prettyLetDeclaration : LetDeclaration -> Doc
prettyLetDeclaration letDecl =
    case letDecl of
        LetFunction fn ->
            prettyFun fn

        _ ->
            Pretty.string "letDestructuring"


prettyCaseBlock : CaseBlock -> Doc
prettyCaseBlock caseBlock =
    ([ Pretty.string "case"
     , prettyExpression (denode caseBlock.expression)
     , Pretty.string "of"
     ]
        |> Pretty.words
    )
        :: List.map
            (\( pattern, expr ) ->
                [ prettyPattern (denode pattern)
                , Pretty.string "->"
                , Pretty.line
                , prettyExpression (denode expr) |> Pretty.indent 4
                , Pretty.line
                ]
                    |> Pretty.words
                    |> Pretty.indent 4
            )
            caseBlock.cases
        |> Pretty.lines


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
            List.map prettyTypeAnnotation (denodeAll anns)
                |> Pretty.join (Pretty.string ", ")
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
    Pretty.surround (Pretty.char '[') (Pretty.char ']') doc
