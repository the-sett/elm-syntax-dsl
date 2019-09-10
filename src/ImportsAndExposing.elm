module ImportsAndExposing exposing (sortAndDedupImports)

import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (emptyRange)
import Maybe.Extra


sortAndDedupImports : List Import -> List Import
sortAndDedupImports imports =
    let
        impName imp =
            denode imp.moduleName
    in
    List.sortBy impName imports
        |> groupByModuleName
        |> List.map combineDups


groupByModuleName innerImports =
    let
        ( _, hdGroup, remGroups ) =
            case innerImports of
                [] ->
                    ( [], [], [ [] ] )

                hd :: _ ->
                    List.foldl
                        (\imp ( currName, currAccum, accum ) ->
                            let
                                nextName =
                                    denode imp.moduleName
                            in
                            if nextName == currName then
                                ( currName, imp :: currAccum, accum )

                            else
                                ( nextName, [ imp ], currAccum :: accum )
                        )
                        ( denode hd.moduleName, [], [] )
                        innerImports
    in
    hdGroup :: remGroups


combineExposings maybeLeft maybeRight =
    case ( maybeLeft, maybeRight ) of
        ( Nothing, Nothing ) ->
            Nothing

        ( Just left, Nothing ) ->
            Just left

        ( Nothing, Just right ) ->
            Just right

        ( Just left, Just right ) ->
            Just left


combineDups : List Import -> Import
combineDups innerImports =
    case innerImports of
        [] ->
            { moduleName = nodify []
            , moduleAlias = Nothing
            , exposingList = Nothing
            }

        hd :: tl ->
            List.foldl
                (\imp result ->
                    { moduleName = imp.moduleName
                    , moduleAlias = Maybe.Extra.or imp.moduleAlias result.moduleAlias
                    , exposingList = combineExposings imp.exposingList result.exposingList
                    }
                )
                hd
                tl


denode =
    Node.value


nodify : a -> Node a
nodify exp =
    Node emptyRange exp
