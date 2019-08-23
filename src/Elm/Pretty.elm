module Elm.Pretty exposing (pretty)

import Elm.Syntax.Comments exposing (Comment)
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
import Pretty exposing (Doc)


denode =
    Node.value


denodeAll =
    List.map denode


denodeMaybe =
    Maybe.map denode


pretty : File -> Doc
pretty file =
    Pretty.lines
        [ prettyModule (denode file.moduleDefinition)
        , prettyComments (denodeAll file.comments)
        , prettyImports (denodeAll file.imports)
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
        |> Pretty.join (Pretty.string ".")


prettyDefaultModuleData : DefaultModuleData -> Doc
prettyDefaultModuleData moduleData =
    Pretty.lines
        [ prettyModuleName (denode moduleData.moduleName)
        , prettyExposing (denode moduleData.exposingList)
        ]


prettyEffectModuleData : EffectModuleData -> Doc
prettyEffectModuleData moduleData =
    Pretty.lines
        [ prettyModuleName (denode moduleData.moduleName)
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
    Pretty.join (Pretty.string " ")
        [ prettyModuleName (denode import_.moduleName)
        , prettyMaybe prettyModuleName (denodeMaybe import_.moduleAlias)
        , prettyMaybe prettyExposing (denodeMaybe import_.exposingList)
        ]


prettyExposing : Exposing -> Doc
prettyExposing exposing_ =
    Pretty.string "exp"


prettyDeclarations : List Declaration -> Doc
prettyDeclarations _ =
    Pretty.string "declarations"


prettyMaybe : (a -> Doc) -> Maybe a -> Doc
prettyMaybe prettyFn maybeVal =
    Maybe.map prettyFn maybeVal
        |> Maybe.withDefault (Pretty.string "")
