module Tests exposing (tests)

import Elm.CodeGen
import Elm.Pretty
import Expect
import Pretty
import Test exposing (Test)


tests : Test
tests =
    Test.describe "escape sequence in literal characters sometimes isn't escaped in pretty printed expression"
        [ Test.test "string with escaped line break is pretty printed correctly"
            (\() ->
                Elm.CodeGen.string "\n"
                    |> Elm.Pretty.prettyExpression
                    |> Pretty.pretty 100
                    |> Expect.equal "\"\\n\""
            )
        , Test.test "string with escaped tab break is pretty printed correctly"
            (\() ->
                Elm.CodeGen.string "\t"
                    |> Elm.Pretty.prettyExpression
                    |> Pretty.pretty 100
                    |> Expect.equal "\"\\t\""
            )
        , Test.test "string with escaped backslash return is pretty printed correctly"
            (\() ->
                Elm.CodeGen.string "\\"
                    |> Elm.Pretty.prettyExpression
                    |> Pretty.pretty 100
                    |> Expect.equal "\"\\\\\""
            )
        , Test.test "string with escaped double quote is pretty printed correctly"
            (\() ->
                Elm.CodeGen.string "\""
                    |> Elm.Pretty.prettyExpression
                    |> Pretty.pretty 100
                    |> Expect.equal "\"\\\"\""
            )
        , Test.test "string with escaped carriage return is pretty printed correctly"
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
        , Test.test "char with escaped line break is pretty printed correctly"
            (\() ->
                Elm.CodeGen.char '\n'
                    |> Elm.Pretty.prettyExpression
                    |> Pretty.pretty 100
                    |> Expect.equal "'\\n'"
            )
        , Test.test "char with escaped tab is pretty printed correctly"
            (\() ->
                Elm.CodeGen.char '\t'
                    |> Elm.Pretty.prettyExpression
                    |> Pretty.pretty 100
                    |> Expect.equal "'\\t'"
            )
        , Test.test "char with escaped backslash is pretty printed correctly"
            (\() ->
                Elm.CodeGen.char '\\'
                    |> Elm.Pretty.prettyExpression
                    |> Pretty.pretty 100
                    |> Expect.equal "'\\\\'"
            )
        , Test.test "char with escaped single quote is pretty printed correctly"
            (\() ->
                Elm.CodeGen.char '\''
                    |> Elm.Pretty.prettyExpression
                    |> Pretty.pretty 100
                    |> Expect.equal "'\\''"
            )
        , Test.test "char with escaped double quote is pretty printed correctly"
            (\() ->
                Elm.CodeGen.char '"'
                    |> Elm.Pretty.prettyExpression
                    |> Pretty.pretty 100
                    |> Expect.equal "'\"'"
            )
        , Test.test "char with escaped code 200D is pretty printed correctly"
            (\() ->
                Elm.CodeGen.char '\u{200D}'
                    |> Elm.Pretty.prettyExpression
                    |> Pretty.pretty 100
                    |> Expect.equal "'\\u{200D}'"
            )
        , Test.test "char with escaped carriage return is pretty printed correctly"
            (\() ->
                Elm.CodeGen.char '\u{000D}'
                    |> Elm.Pretty.prettyExpression
                    |> Pretty.pretty 100
                    |> Expect.equal "'\\u{000D}'"
            )
        ]
