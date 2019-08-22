module Elm.Pretty exposing (pretty)

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


deNode =
    Node.value


deNodeAll =
    List.map deNode


pretty : File -> Doc
pretty file =
    Pretty.lines
        [ prettyModule (deNode file.moduleDefinition)
        , prettyComments (deNodeAll file.comments)
        , prettyImports (deNodeAll file.imports)
        , prettyDeclarations (deNodeAll file.declarations)
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
