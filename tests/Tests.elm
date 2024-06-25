module Tests exposing (tests)

import Elm.CodeGen
import Elm.Pretty
import Expect
import Pretty
import Test exposing (Test)


tests : Test
tests =
    Test.describe "escape sequence in `Elm.CodeGen.string` sometimes isn't escaped in pretty printed expression"
        [ Test.test "string with escaped carriage return is pretty printed correctly"
            (\() ->
                Elm.CodeGen.string "\u{000D}"
                    |> Elm.Pretty.prettyExpression
                    |> Pretty.pretty 100
                    |> Expect.equal "\"\\u{000D}\""
            )
        , Test.test "string with escaped combo emoji is pretty printed correctly"
            (\() ->
                Elm.CodeGen.string "👩\u{200D}🚒"
                    |> Elm.Pretty.prettyExpression
                    |> Pretty.pretty 100
                    |> String.toList
                    |> Expect.equalLists ("\"👩\\u{200D}🚒\"" |> String.toList)
            )
        ]
