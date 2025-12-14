module SourceRoundTripTests exposing (suite)

{-| Tests for round-tripping Elm source code through:

1.  Start with Elm source code as a String
2.  Parse the source code using Elm.DSLParser (String -> AST)
3.  Pretty print the AST using Elm.Pretty (AST -> String)
4.  Compare the output with the original source

These tests demonstrate that triple-quoted multiline strings (""") do NOT
round-trip correctly - the pretty printer converts them to escaped regular strings.

-}

import Elm.DSLParser
import Elm.Pretty
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Source Round-trip tests"
        [ tripleQuotedStringTests
        ]


{-| Test that source code round-trips exactly.
The source must be formatted exactly as the pretty printer would format it.
-}
roundTripSource : String -> String -> Expect.Expectation
roundTripSource label source =
    case Elm.DSLParser.parse source of
        Err deadEnds ->
            Expect.fail
                ("Failed to parse source:\n"
                    ++ source
                    ++ "\n\nErrors: "
                    ++ Debug.toString deadEnds
                )

        Ok file ->
            let
                printed =
                    Elm.Pretty.pretty 120 file
            in
            if printed == source then
                Expect.pass

            else
                Expect.fail
                    ("Source did not round-trip exactly.\n\n"
                        ++ "=== Original ===\n"
                        ++ source
                        ++ "\n\n=== Pretty printed ===\n"
                        ++ printed
                        ++ "\n\n=== Diff ===\n"
                        ++ showDiff source printed
                    )


{-| Show a simple diff between two strings.
-}
showDiff : String -> String -> String
showDiff original printed =
    let
        origLines =
            String.lines original

        printedLines =
            String.lines printed

        maxLines =
            max (List.length origLines) (List.length printedLines)

        diffLine i =
            let
                origLine =
                    List.head (List.drop i origLines) |> Maybe.withDefault ""

                printedLine =
                    List.head (List.drop i printedLines) |> Maybe.withDefault ""
            in
            if origLine == printedLine then
                "  " ++ origLine

            else
                "- " ++ origLine ++ "\n+ " ++ printedLine
    in
    List.range 0 (maxLines - 1)
        |> List.map diffLine
        |> String.join "\n"



-- ============================================================================
-- Triple-Quoted String Tests (""")
-- ============================================================================
-- These tests verify that triple-quoted strings do NOT round-trip correctly.
-- The pretty printer converts """ strings to escaped regular strings.
-- These tests are expected to FAIL until the pretty printer is fixed.


tripleQuotedStringTests : Test
tripleQuotedStringTests =
    describe "Triple-quoted strings (\"\"\")"
        [ test "Simple triple-quoted string" <|
            \_ ->
                roundTripSource "triple simple"
                    ("module Test exposing (val)\n\n\nval =\n    \"\"\"hello\"\"\"\n")
        , test "Triple-quoted multiline string" <|
            \_ ->
                roundTripSource "triple multiline"
                    ("module Test exposing (val)\n\n\nval =\n    \"\"\"\nline1\nline2\n\"\"\"\n")
        , test "Triple-quoted SQL query" <|
            \_ ->
                roundTripSource "triple sql"
                    ("module Test exposing (query)\n\n\nquery =\n    \"\"\"\nSELECT *\nFROM users\nWHERE active = true\n\"\"\"\n")
        , test "Triple-quoted HTML template" <|
            \_ ->
                roundTripSource "triple html"
                    ("module Test exposing (html)\n\n\nhtml =\n    \"\"\"\n<html>\n<body>\n</body>\n</html>\n\"\"\"\n")
        , test "Triple-quoted with leading content" <|
            \_ ->
                roundTripSource "triple leading"
                    ("module Test exposing (val)\n\n\nval =\n    \"\"\"hello\nworld\"\"\"\n")
        , test "Triple-quoted empty string" <|
            \_ ->
                roundTripSource "triple empty"
                    ("module Test exposing (val)\n\n\nval =\n    \"\"\"\"\"\"\n")
        , test "Triple-quoted with blank lines" <|
            \_ ->
                roundTripSource "triple blanks"
                    ("module Test exposing (val)\n\n\nval =\n    \"\"\"\nfirst\n\nsecond\n\"\"\"\n")
        , test "Triple-quoted in let binding" <|
            \_ ->
                roundTripSource "triple let"
                    ("module Test exposing (example)\n\n\nexample =\n    let\n        sql =\n            \"\"\"\nSELECT 1\n\"\"\"\n    in\n    sql\n")
        , test "Triple-quoted JSON" <|
            \_ ->
                roundTripSource "triple json"
                    ("module Test exposing (json)\n\n\njson =\n    \"\"\"\n{\\\"name\\\": \\\"test\\\"}\n\"\"\"\n")
        ]
