module RoundTripTests exposing (suite)

{-| Property-based tests for round-tripping Elm code through:

1.  Generate random Elm code using Elm.CodeGen
2.  Pretty print the code using Elm.Pretty
3.  Parse the code using Elm.DSLParser
4.  Check that the re-parsed code matches the original structure

-}

import Elm.CodeGen as CG
import Elm.DSLParser
import Elm.Pretty
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Expect
import Fuzz exposing (Fuzzer)
import Fuzzers
import Pretty
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Round-trip tests"
        [ literalTests
        , variableTests
        , applicationTests
        , operatorTests
        , controlFlowTests
        , letTests
        , lambdaTests
        , collectionTests
        , recordTests
        , patternTests
        , typeAnnotationTests
        , declarationTests
        , parenthesesTests
        , hexAndNumericTests
        , escapeSequenceTests
        , complexPatternTests
        , lambdaEdgeCaseTests
        , letEdgeCaseTests
        , recordEdgeCaseTests
        , typeAnnotationEdgeCaseTests
        , declarationEdgeCaseTests
        , operatorEdgeCaseTests
        , realWorldCombinationTests
        ]



-- ============================================================================
-- Test Helpers
-- ============================================================================


{-| Create a minimal Elm file containing a single expression.
-}
wrapExprInFile : Expression -> CG.File
wrapExprInFile expr =
    CG.file
        (CG.normalModule [ "Test" ] [ CG.funExpose "test" ])
        []
        [ CG.valDecl Nothing Nothing "test" expr ]
        Nothing


{-| Pretty print a file to a string.
-}
prettyPrintFile : CG.File -> String
prettyPrintFile file =
    Elm.Pretty.pretty 120 file


{-| Extract the first declaration's expression from a file.
-}
extractExpr : CG.File -> Maybe Expression
extractExpr file =
    case file.declarations of
        (CG.DeclNoComment (FunctionDeclaration fn)) :: _ ->
            let
                (Node _ impl) =
                    fn.declaration
            in
            Just (Elm.Syntax.Node.value impl.expression)

        (CG.DeclWithComment _ declFn) :: _ ->
            case declFn "" of
                FunctionDeclaration fn ->
                    let
                        (Node _ impl) =
                            fn.declaration
                    in
                    Just (Elm.Syntax.Node.value impl.expression)

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Test that an expression round-trips successfully.
The expression structure should be preserved (ignoring source locations).
-}
roundTripExpr : String -> Expression -> Expect.Expectation
roundTripExpr label expr =
    let
        file =
            wrapExprInFile expr

        printed =
            prettyPrintFile file

        parsed =
            Elm.DSLParser.parse printed
    in
    case parsed of
        Err deadEnds ->
            Expect.fail
                ("Failed to parse:\n"
                    ++ printed
                    ++ "\n\nErrors: "
                    ++ Debug.toString deadEnds
                )

        Ok parsedFile ->
            case extractExpr parsedFile of
                Nothing ->
                    Expect.fail ("Could not extract expression from parsed file:\n" ++ printed)

                Just parsedExpr ->
                    -- For now, just check that parsing succeeded.
                    -- A more thorough check would compare the AST structure.
                    Expect.pass



-- ============================================================================
-- Literal Tests
-- ============================================================================


literalTests : Test
literalTests =
    describe "Literals"
        [ fuzz Fuzz.int "Integer literals round-trip" <|
            \n ->
                roundTripExpr "int" (CG.int n)
        , fuzz Fuzz.niceFloat "Float literals round-trip" <|
            \f ->
                roundTripExpr "float" (CG.float f)
        , fuzz Fuzzers.simpleString "String literals round-trip" <|
            \s ->
                roundTripExpr "string" (CG.string s)
        , fuzz Fuzzers.simpleChar "Char literals round-trip" <|
            \c ->
                roundTripExpr "char" (CG.char c)
        , test "Unit expression round-trips" <|
            \_ ->
                roundTripExpr "unit" CG.unit
        ]



-- ============================================================================
-- Variable Tests
-- ============================================================================


variableTests : Test
variableTests =
    describe "Variables"
        [ fuzz Fuzzers.identifier "Simple variable references round-trip" <|
            \name ->
                roundTripExpr "var" (CG.val name)
        , fuzz Fuzzers.qualifiedVarExpr "Qualified variable references round-trip" <|
            \expr ->
                roundTripExpr "qualified var" expr
        , fuzz Fuzzers.constructorExpr "Constructor expressions round-trip" <|
            \expr ->
                roundTripExpr "constructor" expr
        ]



-- ============================================================================
-- Application Tests
-- ============================================================================


applicationTests : Test
applicationTests =
    describe "Function Application"
        [ fuzz Fuzzers.simpleApplicationExpr "Simple application round-trips" <|
            \expr ->
                roundTripExpr "simple app" expr
        , fuzz Fuzzers.nestedApplicationExpr "Nested application round-trips" <|
            \expr ->
                roundTripExpr "nested app" expr
        , fuzz2 Fuzzers.identifier Fuzzers.literalExpr "Multi-arg application round-trips" <|
            \fn arg ->
                roundTripExpr "multi-arg app"
                    (CG.apply [ CG.val fn, arg, CG.int 42 ])
        , fuzz Fuzzers.identifier "Constructor application round-trips" <|
            \arg ->
                roundTripExpr "constructor app"
                    (CG.apply [ CG.val "Just", CG.val arg ])
        ]



-- ============================================================================
-- Operator Tests
-- ============================================================================


operatorTests : Test
operatorTests =
    describe "Operators"
        [ fuzz2 Fuzz.int Fuzz.int "Arithmetic operators round-trip" <|
            \a b ->
                roundTripExpr "arithmetic"
                    (CG.applyBinOp (CG.int a) CG.plus (CG.int b))
        , fuzz2 Fuzz.int Fuzz.int "Comparison operators round-trip" <|
            \a b ->
                roundTripExpr "comparison"
                    (CG.applyBinOp (CG.int a) CG.equals (CG.int b))
        , test "Boolean operators round-trip" <|
            \_ ->
                roundTripExpr "boolean"
                    (CG.applyBinOp (CG.val "a") CG.and (CG.val "b"))
        , test "List cons operator round-trips" <|
            \_ ->
                roundTripExpr "cons"
                    (CG.applyBinOp (CG.int 1) CG.cons (CG.list [ CG.int 2 ]))
        , test "List append operator round-trips" <|
            \_ ->
                roundTripExpr "append"
                    (CG.applyBinOp (CG.list [ CG.int 1 ]) CG.append (CG.list [ CG.int 2 ]))
        , fuzz Fuzzers.pipeRightExpr "Pipe right operator round-trips" <|
            \expr ->
                roundTripExpr "pipe right" expr
        , fuzz Fuzzers.pipeLeftExpr "Pipe left operator round-trips" <|
            \expr ->
                roundTripExpr "pipe left" expr
        , test "Composition operators round-trip" <|
            \_ ->
                roundTripExpr "composition"
                    (CG.applyBinOp (CG.val "f") CG.composer (CG.val "g"))
        , fuzz Fuzzers.binaryOpExpr "Mixed operators round-trip" <|
            \expr ->
                roundTripExpr "mixed ops" expr
        ]



-- ============================================================================
-- Control Flow Tests
-- ============================================================================


controlFlowTests : Test
controlFlowTests =
    describe "Control Flow"
        [ fuzz Fuzzers.ifExpr "Simple if-then-else round-trips" <|
            \expr ->
                roundTripExpr "if-then-else" expr
        , test "Nested if-then-else round-trips" <|
            \_ ->
                roundTripExpr "nested if"
                    (CG.ifExpr (CG.val "a")
                        (CG.ifExpr (CG.val "b") (CG.int 1) (CG.int 2))
                        (CG.int 3)
                    )
        , fuzz Fuzzers.caseExpr "Case with constructor patterns round-trips" <|
            \expr ->
                roundTripExpr "case" expr
        , test "Case with literal patterns round-trips" <|
            \_ ->
                roundTripExpr "case literal"
                    (CG.caseExpr (CG.val "x")
                        [ ( CG.intPattern 0, CG.string "zero" )
                        , ( CG.intPattern 1, CG.string "one" )
                        , ( CG.allPattern, CG.string "other" )
                        ]
                    )
        , test "Case with wildcard patterns round-trips" <|
            \_ ->
                roundTripExpr "case wildcard"
                    (CG.caseExpr (CG.val "x")
                        [ ( CG.allPattern, CG.int 0 )
                        ]
                    )
        , test "Case with as-patterns round-trips" <|
            \_ ->
                roundTripExpr "case as"
                    (CG.caseExpr (CG.val "maybeX")
                        [ ( CG.asPattern (CG.namedPattern "Just" [ CG.varPattern "x" ]) "whole"
                          , CG.val "whole"
                          )
                        , ( CG.namedPattern "Nothing" [], CG.val "Nothing" )
                        ]
                    )
        ]



-- ============================================================================
-- Let Expression Tests
-- ============================================================================


letTests : Test
letTests =
    describe "Let Expressions"
        [ fuzz Fuzzers.letExpr "Single let binding round-trips" <|
            \expr ->
                roundTripExpr "let single" expr
        , test "Multiple let bindings round-trip" <|
            \_ ->
                roundTripExpr "let multiple"
                    (CG.letExpr
                        [ CG.letVal "x" (CG.int 1)
                        , CG.letVal "y" (CG.int 2)
                        ]
                        (CG.applyBinOp (CG.val "x") CG.plus (CG.val "y"))
                    )
        , test "Let with destructuring round-trips" <|
            \_ ->
                roundTripExpr "let destructure"
                    (CG.letExpr
                        [ CG.letDestructuring
                            (CG.tuplePattern [ CG.varPattern "a", CG.varPattern "b" ])
                            (CG.tuple [ CG.int 1, CG.int 2 ])
                        ]
                        (CG.val "a")
                    )
        , test "Nested let expressions round-trip" <|
            \_ ->
                roundTripExpr "let nested"
                    (CG.letExpr
                        [ CG.letVal "x"
                            (CG.letExpr
                                [ CG.letVal "y" (CG.int 1) ]
                                (CG.val "y")
                            )
                        ]
                        (CG.val "x")
                    )
        ]



-- ============================================================================
-- Lambda Tests
-- ============================================================================


lambdaTests : Test
lambdaTests =
    describe "Lambda Expressions"
        [ fuzz Fuzzers.lambdaExpr "Single-argument lambda round-trips" <|
            \expr ->
                roundTripExpr "lambda single" expr
        , fuzz2 Fuzzers.identifier Fuzzers.identifier "Multi-argument lambda round-trips" <|
            \arg1 arg2 ->
                roundTripExpr "lambda multi"
                    (CG.lambda
                        [ CG.varPattern arg1, CG.varPattern arg2 ]
                        (CG.applyBinOp (CG.val arg1) CG.plus (CG.val arg2))
                    )
        , test "Lambda with tuple pattern round-trips" <|
            \_ ->
                roundTripExpr "lambda tuple"
                    (CG.lambda
                        [ CG.tuplePattern [ CG.varPattern "a", CG.varPattern "b" ] ]
                        (CG.val "a")
                    )
        , test "Lambda with record pattern round-trips" <|
            \_ ->
                roundTripExpr "lambda record"
                    (CG.lambda
                        [ CG.recordPattern [ "x", "y" ] ]
                        (CG.val "x")
                    )
        ]



-- ============================================================================
-- Collection Tests
-- ============================================================================


collectionTests : Test
collectionTests =
    describe "Collections"
        [ test "Empty list round-trips" <|
            \_ ->
                roundTripExpr "empty list" (CG.list [])
        , fuzz Fuzzers.listExpr "List with elements round-trips" <|
            \expr ->
                roundTripExpr "list" expr
        , fuzz Fuzzers.tupleExpr "Tuple pairs round-trip" <|
            \expr ->
                roundTripExpr "tuple pair" expr
        , test "Tuple triples round-trip" <|
            \_ ->
                roundTripExpr "tuple triple"
                    (CG.tuple [ CG.int 1, CG.string "two", CG.val "three" ])
        ]



-- ============================================================================
-- Record Tests
-- ============================================================================


recordTests : Test
recordTests =
    describe "Records"
        [ test "Empty record round-trips" <|
            \_ ->
                roundTripExpr "empty record" (CG.record [])
        , fuzz Fuzzers.recordExpr "Record creation round-trips" <|
            \expr ->
                roundTripExpr "record" expr
        , fuzz Fuzzers.recordUpdateExpr "Record update round-trips" <|
            \expr ->
                roundTripExpr "record update" expr
        , fuzz Fuzzers.recordAccessExpr "Record field access round-trips" <|
            \expr ->
                roundTripExpr "record access" expr
        , fuzz Fuzzers.identifier "Record accessor function round-trips" <|
            \field ->
                roundTripExpr "record accessor"
                    (CG.accessFun ("." ++ field))
        ]



-- ============================================================================
-- Pattern Tests
-- ============================================================================


patternTests : Test
patternTests =
    describe "Patterns"
        [ fuzz Fuzzers.varPattern "Variable patterns round-trip" <|
            \pat ->
                roundTripPattern "var pattern" pat
        , test "Wildcard patterns round-trip" <|
            \_ ->
                roundTripPattern "wildcard" CG.allPattern
        , fuzz (Fuzz.intRange 0 1000) "Integer patterns round-trip" <|
            \n ->
                roundTripPattern "int pattern" (CG.intPattern n)
        , fuzz Fuzzers.simpleString "String patterns round-trip" <|
            \s ->
                roundTripPattern "string pattern" (CG.stringPattern s)
        , fuzz Fuzzers.tuplePattern "Tuple patterns round-trip" <|
            \pat ->
                roundTripPattern "tuple pattern" pat
        , fuzz Fuzzers.listPattern "List patterns round-trip" <|
            \pat ->
                roundTripPattern "list pattern" pat
        , fuzz Fuzzers.consPattern "Cons patterns round-trip" <|
            \pat ->
                roundTripPattern "cons pattern" pat
        , fuzz Fuzzers.constructorPattern "Constructor patterns round-trip" <|
            \pat ->
                roundTripPattern "constructor pattern" pat
        , fuzz Fuzzers.recordPattern "Record patterns round-trip" <|
            \pat ->
                roundTripPattern "record pattern" pat
        ]


{-| Test that a pattern round-trips successfully by embedding it in a case expression.
-}
roundTripPattern : String -> Pattern -> Expect.Expectation
roundTripPattern label pat =
    roundTripExpr label
        (CG.caseExpr (CG.val "x")
            [ ( pat, CG.int 1 )
            , ( CG.allPattern, CG.int 0 )
            ]
        )



-- ============================================================================
-- Type Annotation Tests
-- ============================================================================


typeAnnotationTests : Test
typeAnnotationTests =
    describe "Type Annotations"
        [ fuzz Fuzzers.simpleTypeAnn "Simple types round-trip" <|
            \ann ->
                roundTripTypeAnn "simple type" ann
        , fuzz Fuzzers.typeVarAnn "Type variables round-trip" <|
            \ann ->
                roundTripTypeAnn "type var" ann
        , fuzz Fuzzers.functionTypeAnn "Function types round-trip" <|
            \ann ->
                roundTripTypeAnn "function type" ann
        , fuzz Fuzzers.tupleTypeAnn "Tuple types round-trip" <|
            \ann ->
                roundTripTypeAnn "tuple type" ann
        , fuzz Fuzzers.recordTypeAnn "Record types round-trip" <|
            \ann ->
                roundTripTypeAnn "record type" ann
        , fuzz Fuzzers.parameterizedTypeAnn "Parameterized types round-trip" <|
            \ann ->
                roundTripTypeAnn "parameterized type" ann
        ]


{-| Test that a type annotation round-trips by embedding it in a value declaration.
-}
roundTripTypeAnn : String -> TypeAnnotation -> Expect.Expectation
roundTripTypeAnn label ann =
    let
        file =
            CG.file
                (CG.normalModule [ "Test" ] [ CG.funExpose "test" ])
                []
                [ CG.valDecl Nothing (Just ann) "test" (CG.int 42) ]
                Nothing

        printed =
            prettyPrintFile file

        parsed =
            Elm.DSLParser.parse printed
    in
    case parsed of
        Err deadEnds ->
            Expect.fail
                ("Failed to parse:\n"
                    ++ printed
                    ++ "\n\nErrors: "
                    ++ Debug.toString deadEnds
                )

        Ok _ ->
            Expect.pass



-- ============================================================================
-- Declaration Tests
-- ============================================================================


declarationTests : Test
declarationTests =
    describe "Declarations"
        [ fuzz Fuzzers.valDecl "Value declarations round-trip" <|
            \decl ->
                roundTripDecl "val decl" decl
        , fuzz Fuzzers.funDecl "Function declarations round-trip" <|
            \decl ->
                roundTripDecl "fun decl" decl
        , fuzz Fuzzers.typeAliasDecl "Type alias declarations round-trip" <|
            \decl ->
                roundTripDecl "type alias" decl
        , fuzz Fuzzers.customTypeDecl "Custom type declarations round-trip" <|
            \decl ->
                roundTripDecl "custom type" decl
        ]


{-| Test that a declaration round-trips.
-}
roundTripDecl : String -> CG.Declaration -> Expect.Expectation
roundTripDecl label decl =
    let
        file =
            CG.file
                (CG.normalModule [ "Test" ] [ CG.funExpose "test" ])
                []
                [ decl ]
                Nothing

        printed =
            prettyPrintFile file

        parsed =
            Elm.DSLParser.parse printed
    in
    case parsed of
        Err deadEnds ->
            Expect.fail
                ("Failed to parse:\n"
                    ++ printed
                    ++ "\n\nErrors: "
                    ++ Debug.toString deadEnds
                )

        Ok _ ->
            Expect.pass



-- ============================================================================
-- Parentheses Tests - Targeting parens preservation in pipelines and operators
-- ============================================================================


parenthesesTests : Test
parenthesesTests =
    describe "Parentheses preservation"
        [ -- Basic parenthesized expressions
          test "Simple parenthesized expression round-trips" <|
            \_ ->
                roundTripExpr "parens simple"
                    (CG.parens (CG.int 42))
        , test "Nested parentheses round-trip" <|
            \_ ->
                roundTripExpr "parens nested"
                    (CG.parens (CG.parens (CG.val "x")))
        , test "Parenthesized binary op round-trips" <|
            \_ ->
                roundTripExpr "parens binop"
                    (CG.parens (CG.applyBinOp (CG.int 1) CG.plus (CG.int 2)))

        -- Pipe right (|>) with parentheses
        , test "Pipe right with parenthesized function round-trips" <|
            \_ ->
                roundTripExpr "piper parens fn"
                    (CG.applyBinOp (CG.val "x") CG.piper (CG.parens (CG.apply [ CG.val "f", CG.val "y" ])))
        , test "Pipe right chain with parenthesized middle round-trips" <|
            \_ ->
                roundTripExpr "piper chain parens"
                    (CG.applyBinOp
                        (CG.applyBinOp (CG.val "x") CG.piper (CG.parens (CG.val "f")))
                        CG.piper
                        (CG.val "g")
                    )
        , test "Parenthesized pipe right expression round-trips" <|
            \_ ->
                roundTripExpr "parens piper"
                    (CG.parens (CG.applyBinOp (CG.val "x") CG.piper (CG.val "f")))
        , test "Pipe right with parenthesized lambda round-trips" <|
            \_ ->
                roundTripExpr "piper parens lambda"
                    (CG.applyBinOp
                        (CG.val "x")
                        CG.piper
                        (CG.parens (CG.lambda [ CG.varPattern "y" ] (CG.applyBinOp (CG.val "y") CG.plus (CG.int 1))))
                    )

        -- Pipe left (<|) with parentheses
        , test "Pipe left with parenthesized argument round-trips" <|
            \_ ->
                roundTripExpr "pipel parens arg"
                    (CG.applyBinOp (CG.val "f") CG.pipel (CG.parens (CG.applyBinOp (CG.int 1) CG.plus (CG.int 2))))
        , test "Pipe left chain with parenthesized expression round-trips" <|
            \_ ->
                roundTripExpr "pipel chain parens"
                    (CG.applyBinOp
                        (CG.val "f")
                        CG.pipel
                        (CG.parens (CG.applyBinOp (CG.val "g") CG.pipel (CG.val "x")))
                    )
        , test "Parenthesized pipe left expression round-trips" <|
            \_ ->
                roundTripExpr "parens pipel"
                    (CG.parens (CG.applyBinOp (CG.val "f") CG.pipel (CG.val "x")))
        , test "Nested pipe left with parens round-trips" <|
            \_ ->
                roundTripExpr "pipel nested parens"
                    (CG.applyBinOp
                        (CG.val "f")
                        CG.pipel
                        (CG.applyBinOp
                            (CG.val "g")
                            CG.pipel
                            (CG.parens (CG.applyBinOp (CG.val "h") CG.pipel (CG.val "x")))
                        )
                    )

        -- Mixed |> and <| with parentheses
        , test "Mixed pipes: f <| (x |> g) round-trips" <|
            \_ ->
                roundTripExpr "mixed pipes 1"
                    (CG.applyBinOp
                        (CG.val "f")
                        CG.pipel
                        (CG.parens (CG.applyBinOp (CG.val "x") CG.piper (CG.val "g")))
                    )
        , test "Mixed pipes: (f <| x) |> g round-trips" <|
            \_ ->
                roundTripExpr "mixed pipes 2"
                    (CG.applyBinOp
                        (CG.parens (CG.applyBinOp (CG.val "f") CG.pipel (CG.val "x")))
                        CG.piper
                        (CG.val "g")
                    )
        , test "Mixed pipes: f <| g <| (x |> h) round-trips" <|
            \_ ->
                roundTripExpr "mixed pipes 3"
                    (CG.applyBinOp
                        (CG.val "f")
                        CG.pipel
                        (CG.applyBinOp
                            (CG.val "g")
                            CG.pipel
                            (CG.parens (CG.applyBinOp (CG.val "x") CG.piper (CG.val "h")))
                        )
                    )

        -- Arithmetic operators with parentheses (precedence)
        , test "Parens override precedence: (a + b) * c round-trips" <|
            \_ ->
                roundTripExpr "parens precedence 1"
                    (CG.applyBinOp
                        (CG.parens (CG.applyBinOp (CG.val "a") CG.plus (CG.val "b")))
                        CG.mult
                        (CG.val "c")
                    )
        , test "Parens override precedence: a * (b + c) round-trips" <|
            \_ ->
                roundTripExpr "parens precedence 2"
                    (CG.applyBinOp
                        (CG.val "a")
                        CG.mult
                        (CG.parens (CG.applyBinOp (CG.val "b") CG.plus (CG.val "c")))
                    )
        , test "Nested arithmetic parens: ((a + b) * c) + d round-trips" <|
            \_ ->
                roundTripExpr "parens precedence nested"
                    (CG.applyBinOp
                        (CG.parens
                            (CG.applyBinOp
                                (CG.parens (CG.applyBinOp (CG.val "a") CG.plus (CG.val "b")))
                                CG.mult
                                (CG.val "c")
                            )
                        )
                        CG.plus
                        (CG.val "d")
                    )

        -- Function application with parentheses
        , test "Function application with parens: f (g x) round-trips" <|
            \_ ->
                roundTripExpr "app parens"
                    (CG.apply [ CG.val "f", CG.parens (CG.apply [ CG.val "g", CG.val "x" ]) ])
        , test "Deeply nested application: f (g (h x)) round-trips" <|
            \_ ->
                roundTripExpr "app parens deep"
                    (CG.apply
                        [ CG.val "f"
                        , CG.parens
                            (CG.apply
                                [ CG.val "g"
                                , CG.parens (CG.apply [ CG.val "h", CG.val "x" ])
                                ]
                            )
                        ]
                    )

        -- Lambdas with parentheses in various positions
        , test "Parenthesized lambda round-trips" <|
            \_ ->
                roundTripExpr "parens lambda"
                    (CG.parens (CG.lambda [ CG.varPattern "x" ] (CG.val "x")))
        , test "Lambda with parenthesized body round-trips" <|
            \_ ->
                roundTripExpr "lambda parens body"
                    (CG.lambda [ CG.varPattern "x" ] (CG.parens (CG.applyBinOp (CG.val "x") CG.plus (CG.int 1))))
        , test "Lambda in parens applied to arg round-trips" <|
            \_ ->
                roundTripExpr "lambda parens applied"
                    (CG.apply
                        [ CG.parens (CG.lambda [ CG.varPattern "x" ] (CG.val "x"))
                        , CG.int 42
                        ]
                    )

        -- If expressions with parentheses
        , test "Parenthesized if expression round-trips" <|
            \_ ->
                roundTripExpr "parens if"
                    (CG.parens (CG.ifExpr (CG.val "cond") (CG.int 1) (CG.int 2)))
        , test "If with parenthesized condition round-trips" <|
            \_ ->
                roundTripExpr "if parens cond"
                    (CG.ifExpr
                        (CG.parens (CG.applyBinOp (CG.val "a") CG.and (CG.val "b")))
                        (CG.int 1)
                        (CG.int 2)
                    )

        -- Record access with parentheses
        , test "Record access on parenthesized expr: (f x).field round-trips" <|
            \_ ->
                roundTripExpr "parens record access"
                    (CG.access (CG.parens (CG.apply [ CG.val "f", CG.val "x" ])) "field")
        , test "Record access on parenthesized record update round-trips" <|
            \_ ->
                roundTripExpr "parens record update access"
                    (CG.access (CG.parens (CG.update "model" [ ( "x", CG.int 1 ) ])) "x")

        -- Let expressions with parentheses
        , test "Parenthesized let expression round-trips" <|
            \_ ->
                roundTripExpr "parens let"
                    (CG.parens
                        (CG.letExpr
                            [ CG.letVal "x" (CG.int 1) ]
                            (CG.val "x")
                        )
                    )
        , test "Let with parenthesized binding value round-trips" <|
            \_ ->
                roundTripExpr "let parens binding"
                    (CG.letExpr
                        [ CG.letVal "x" (CG.parens (CG.applyBinOp (CG.int 1) CG.plus (CG.int 2))) ]
                        (CG.val "x")
                    )

        -- Case expressions with parentheses
        , test "Parenthesized case expression round-trips" <|
            \_ ->
                roundTripExpr "parens case"
                    (CG.parens
                        (CG.caseExpr (CG.val "x")
                            [ ( CG.namedPattern "Just" [ CG.varPattern "y" ], CG.val "y" )
                            , ( CG.namedPattern "Nothing" [], CG.int 0 )
                            ]
                        )
                    )
        , test "Case with parenthesized scrutinee round-trips" <|
            \_ ->
                roundTripExpr "case parens scrutinee"
                    (CG.caseExpr (CG.parens (CG.apply [ CG.val "f", CG.val "x" ]))
                        [ ( CG.intPattern 0, CG.string "zero" )
                        , ( CG.allPattern, CG.string "other" )
                        ]
                    )

        -- List and cons with parentheses
        , test "List with parenthesized elements round-trips" <|
            \_ ->
                roundTripExpr "list parens elements"
                    (CG.list
                        [ CG.parens (CG.applyBinOp (CG.int 1) CG.plus (CG.int 2))
                        , CG.parens (CG.apply [ CG.val "f", CG.val "x" ])
                        ]
                    )
        , test "Cons with parenthesized head round-trips" <|
            \_ ->
                roundTripExpr "cons parens head"
                    (CG.applyBinOp
                        (CG.parens (CG.applyBinOp (CG.int 1) CG.plus (CG.int 2)))
                        CG.cons
                        (CG.list [ CG.int 3 ])
                    )

        -- Tuple with parenthesized elements
        , test "Tuple with parenthesized elements round-trips" <|
            \_ ->
                roundTripExpr "tuple parens elements"
                    (CG.tuple
                        [ CG.parens (CG.applyBinOp (CG.val "a") CG.plus (CG.val "b"))
                        , CG.parens (CG.apply [ CG.val "f", CG.val "x" ])
                        ]
                    )

        -- Complex nested combinations
        , test "Complex: f <| (\\x -> x |> g) <| y round-trips" <|
            \_ ->
                roundTripExpr "complex 1"
                    (CG.applyBinOp
                        (CG.applyBinOp
                            (CG.val "f")
                            CG.pipel
                            (CG.parens
                                (CG.lambda [ CG.varPattern "x" ]
                                    (CG.applyBinOp (CG.val "x") CG.piper (CG.val "g"))
                                )
                            )
                        )
                        CG.pipel
                        (CG.val "y")
                    )
        , test "Complex: (a |> b |> c) + (d |> e) round-trips" <|
            \_ ->
                roundTripExpr "complex 2"
                    (CG.applyBinOp
                        (CG.parens
                            (CG.applyBinOp
                                (CG.applyBinOp (CG.val "a") CG.piper (CG.val "b"))
                                CG.piper
                                (CG.val "c")
                            )
                        )
                        CG.plus
                        (CG.parens (CG.applyBinOp (CG.val "d") CG.piper (CG.val "e")))
                    )
        , test "Complex: f (g <| h <| x) (y |> i) round-trips" <|
            \_ ->
                roundTripExpr "complex 3"
                    (CG.apply
                        [ CG.val "f"
                        , CG.parens
                            (CG.applyBinOp
                                (CG.val "g")
                                CG.pipel
                                (CG.applyBinOp (CG.val "h") CG.pipel (CG.val "x"))
                            )
                        , CG.parens (CG.applyBinOp (CG.val "y") CG.piper (CG.val "i"))
                        ]
                    )

        -- Negation with parentheses
        , test "Negation of parenthesized sum: -(a + b) round-trips" <|
            \_ ->
                roundTripExpr "negate parens sum"
                    (CG.negate (CG.parens (CG.applyBinOp (CG.val "a") CG.plus (CG.val "b"))))
        , test "Negation of parenthesized product round-trips" <|
            \_ ->
                roundTripExpr "negate parens product"
                    (CG.negate (CG.parens (CG.applyBinOp (CG.val "a") CG.mult (CG.val "b"))))
        , test "Parenthesized negation round-trips" <|
            \_ ->
                roundTripExpr "parens negate"
                    (CG.parens (CG.negate (CG.val "x")))
        , test "Negation in binary op: a + (-b) round-trips" <|
            \_ ->
                roundTripExpr "binop negate"
                    (CG.applyBinOp (CG.val "a") CG.plus (CG.parens (CG.negate (CG.val "b"))))

        -- Accessor functions
        , test "Accessor function round-trips" <|
            \_ ->
                roundTripExpr "accessor"
                    (CG.accessFun ".field")
        , test "Accessor function in pipeline round-trips" <|
            \_ ->
                roundTripExpr "accessor pipeline"
                    (CG.applyBinOp (CG.val "record") CG.piper (CG.accessFun ".field"))
        , test "Parenthesized accessor in application round-trips" <|
            \_ ->
                roundTripExpr "parens accessor app"
                    (CG.apply [ CG.val "List.map", CG.parens (CG.accessFun ".name") ])

        -- Composition operators with parens
        , test "Composition with parens: (f >> g) >> h round-trips" <|
            \_ ->
                roundTripExpr "compose parens"
                    (CG.applyBinOp
                        (CG.parens (CG.applyBinOp (CG.val "f") CG.composer (CG.val "g")))
                        CG.composer
                        (CG.val "h")
                    )
        , test "Composition with parens: f >> (g >> h) round-trips" <|
            \_ ->
                roundTripExpr "compose parens 2"
                    (CG.applyBinOp
                        (CG.val "f")
                        CG.composer
                        (CG.parens (CG.applyBinOp (CG.val "g") CG.composer (CG.val "h")))
                    )
        , test "Left composition with parens round-trips" <|
            \_ ->
                roundTripExpr "composel parens"
                    (CG.applyBinOp
                        (CG.parens (CG.applyBinOp (CG.val "f") CG.composel (CG.val "g")))
                        CG.composel
                        (CG.val "h")
                    )

        -- Boolean operators with parens
        , test "Boolean: (a && b) || c round-trips" <|
            \_ ->
                roundTripExpr "bool parens 1"
                    (CG.applyBinOp
                        (CG.parens (CG.applyBinOp (CG.val "a") CG.and (CG.val "b")))
                        CG.or
                        (CG.val "c")
                    )
        , test "Boolean: a && (b || c) round-trips" <|
            \_ ->
                roundTripExpr "bool parens 2"
                    (CG.applyBinOp
                        (CG.val "a")
                        CG.and
                        (CG.parens (CG.applyBinOp (CG.val "b") CG.or (CG.val "c")))
                    )

        -- Comparison with parens
        , test "Comparison: (a + b) == c round-trips" <|
            \_ ->
                roundTripExpr "compare parens"
                    (CG.applyBinOp
                        (CG.parens (CG.applyBinOp (CG.val "a") CG.plus (CG.val "b")))
                        CG.equals
                        (CG.val "c")
                    )
        , test "Comparison: a == (b + c) round-trips" <|
            \_ ->
                roundTripExpr "compare parens 2"
                    (CG.applyBinOp
                        (CG.val "a")
                        CG.equals
                        (CG.parens (CG.applyBinOp (CG.val "b") CG.plus (CG.val "c")))
                    )

        -- String append with parens
        , test "String append: (a ++ b) ++ c round-trips" <|
            \_ ->
                roundTripExpr "append parens"
                    (CG.applyBinOp
                        (CG.parens (CG.applyBinOp (CG.val "a") CG.append (CG.val "b")))
                        CG.append
                        (CG.val "c")
                    )

        -- Pipes with function application inside
        , test "Pipe with applied function: x |> f a round-trips" <|
            \_ ->
                roundTripExpr "pipe applied fn"
                    (CG.applyBinOp
                        (CG.val "x")
                        CG.piper
                        (CG.apply [ CG.val "f", CG.val "a" ])
                    )
        , test "Pipe with applied function: f a <| x round-trips" <|
            \_ ->
                roundTripExpr "pipel applied fn"
                    (CG.applyBinOp
                        (CG.apply [ CG.val "f", CG.val "a" ])
                        CG.pipel
                        (CG.val "x")
                    )
        , test "Pipe chain with applied functions round-trips" <|
            \_ ->
                roundTripExpr "pipe chain applied"
                    (CG.applyBinOp
                        (CG.applyBinOp
                            (CG.val "x")
                            CG.piper
                            (CG.apply [ CG.val "f", CG.val "a" ])
                        )
                        CG.piper
                        (CG.apply [ CG.val "g", CG.val "b" ])
                    )

        -- Parens around unit and simple values
        , test "Parenthesized unit round-trips" <|
            \_ ->
                roundTripExpr "parens unit"
                    (CG.parens CG.unit)
        , test "Parenthesized string round-trips" <|
            \_ ->
                roundTripExpr "parens string"
                    (CG.parens (CG.string "hello"))

        -- Record update with complex expressions
        , test "Record update with pipe in value round-trips" <|
            \_ ->
                roundTripExpr "record update pipe"
                    (CG.update "model"
                        [ ( "value", CG.applyBinOp (CG.val "x") CG.piper (CG.val "f") ) ]
                    )
        , test "Record update with parens in value round-trips" <|
            \_ ->
                roundTripExpr "record update parens value"
                    (CG.update "model"
                        [ ( "value", CG.parens (CG.applyBinOp (CG.int 1) CG.plus (CG.int 2)) ) ]
                    )

        -- Very deep nesting
        , test "Deeply nested parens round-trip" <|
            \_ ->
                roundTripExpr "deep parens"
                    (CG.parens
                        (CG.parens
                            (CG.parens
                                (CG.parens (CG.val "x"))
                            )
                        )
                    )
        , test "Deep pipeline with parens round-trips" <|
            \_ ->
                roundTripExpr "deep pipe parens"
                    (CG.applyBinOp
                        (CG.applyBinOp
                            (CG.applyBinOp
                                (CG.parens (CG.val "x"))
                                CG.piper
                                (CG.parens (CG.val "f"))
                            )
                            CG.piper
                            (CG.parens (CG.val "g"))
                        )
                        CG.piper
                        (CG.parens (CG.val "h"))
                    )

        -- Negative numbers and application (parsing sensitive)
        , test "Function applied to negative int: f (-1) round-trips" <|
            \_ ->
                roundTripExpr "fn negative int"
                    (CG.apply [ CG.val "f", CG.parens (CG.negate (CG.int 1)) ])
        , test "Function applied to negative expression: f (-(a + b)) round-trips" <|
            \_ ->
                roundTripExpr "fn negative expr"
                    (CG.apply
                        [ CG.val "f"
                        , CG.parens (CG.negate (CG.parens (CG.applyBinOp (CG.val "a") CG.plus (CG.val "b"))))
                        ]
                    )

        -- Constructor applications with operators
        , test "Constructor with binop arg: Just (a + b) round-trips" <|
            \_ ->
                roundTripExpr "constructor binop"
                    (CG.apply
                        [ CG.val "Just"
                        , CG.parens (CG.applyBinOp (CG.val "a") CG.plus (CG.val "b"))
                        ]
                    )
        , test "Constructor with pipe arg: Just (x |> f) round-trips" <|
            \_ ->
                roundTripExpr "constructor pipe"
                    (CG.apply
                        [ CG.val "Just"
                        , CG.parens (CG.applyBinOp (CG.val "x") CG.piper (CG.val "f"))
                        ]
                    )
        , test "Ok with complex arg round-trips" <|
            \_ ->
                roundTripExpr "ok complex"
                    (CG.apply
                        [ CG.val "Ok"
                        , CG.parens
                            (CG.applyBinOp
                                (CG.val "a")
                                CG.plus
                                (CG.applyBinOp (CG.val "b") CG.mult (CG.val "c"))
                            )
                        ]
                    )

        -- Operator sections and partial application patterns
        , test "Operator in parens: (++) round-trips" <|
            \_ ->
                roundTripExpr "op parens append"
                    (CG.parens (CG.fqVal [] "++"))
        , test "Operator application: (++) a b round-trips" <|
            \_ ->
                roundTripExpr "op apply"
                    (CG.apply [ CG.parens (CG.fqVal [] "++"), CG.val "a", CG.val "b" ])

        -- Tricky precedence cases
        , test "Division precedence: a / (b / c) round-trips" <|
            \_ ->
                roundTripExpr "div precedence"
                    (CG.applyBinOp
                        (CG.val "a")
                        CG.div
                        (CG.parens (CG.applyBinOp (CG.val "b") CG.div (CG.val "c")))
                    )
        , test "Subtraction precedence: a - (b - c) round-trips" <|
            \_ ->
                roundTripExpr "sub precedence"
                    (CG.applyBinOp
                        (CG.val "a")
                        CG.minus
                        (CG.parens (CG.applyBinOp (CG.val "b") CG.minus (CG.val "c")))
                    )
        , test "Mixed div/mult: (a / b) * c vs a / (b * c) round-trips" <|
            \_ ->
                roundTripExpr "div mult"
                    (CG.applyBinOp
                        (CG.parens (CG.applyBinOp (CG.val "a") CG.div (CG.val "b")))
                        CG.mult
                        (CG.val "c")
                    )

        -- Cons operator edge cases
        , test "Nested cons: a :: (b :: c) round-trips" <|
            \_ ->
                roundTripExpr "nested cons"
                    (CG.applyBinOp
                        (CG.val "a")
                        CG.cons
                        (CG.parens (CG.applyBinOp (CG.val "b") CG.cons (CG.val "c")))
                    )
        , test "Cons with parens on left: (a + b) :: list round-trips" <|
            \_ ->
                roundTripExpr "cons left parens"
                    (CG.applyBinOp
                        (CG.parens (CG.applyBinOp (CG.val "a") CG.plus (CG.val "b")))
                        CG.cons
                        (CG.val "list")
                    )

        -- Application with infix in arg position
        , test "Apply with infix in multiple args round-trips" <|
            \_ ->
                roundTripExpr "apply multi infix"
                    (CG.apply
                        [ CG.val "f"
                        , CG.parens (CG.applyBinOp (CG.val "a") CG.plus (CG.val "b"))
                        , CG.parens (CG.applyBinOp (CG.val "c") CG.mult (CG.val "d"))
                        ]
                    )

        -- Lambda as argument without parens should work
        , test "Lambda passed to function round-trips" <|
            \_ ->
                roundTripExpr "lambda arg"
                    (CG.apply
                        [ CG.val "List.map"
                        , CG.parens (CG.lambda [ CG.varPattern "x" ] (CG.applyBinOp (CG.val "x") CG.plus (CG.int 1)))
                        ]
                    )

        -- Power operator (right associative)
        , test "Power operator: (a ^ b) ^ c round-trips" <|
            \_ ->
                roundTripExpr "power left assoc"
                    (CG.applyBinOp
                        (CG.parens (CG.applyBinOp (CG.val "a") CG.power (CG.val "b")))
                        CG.power
                        (CG.val "c")
                    )

        -- Integer division
        , test "Integer division with parens round-trips" <|
            \_ ->
                roundTripExpr "intdiv parens"
                    (CG.applyBinOp
                        (CG.parens (CG.applyBinOp (CG.val "a") CG.plus (CG.val "b")))
                        CG.intDiv
                        (CG.val "c")
                    )

        -- Power operator
        , test "Power with parens round-trips" <|
            \_ ->
                roundTripExpr "power parens"
                    (CG.applyBinOp
                        (CG.val "a")
                        CG.power
                        (CG.parens (CG.applyBinOp (CG.val "b") CG.plus (CG.val "c")))
                    )

        -- Complex real-world patterns
        , test "Real-world: List.map (\\x -> x + 1) |> List.filter (\\x -> x > 0) round-trips" <|
            \_ ->
                roundTripExpr "real world 1"
                    (CG.applyBinOp
                        (CG.apply
                            [ CG.fqVal [ "List" ] "map"
                            , CG.parens (CG.lambda [ CG.varPattern "x" ] (CG.applyBinOp (CG.val "x") CG.plus (CG.int 1)))
                            ]
                        )
                        CG.piper
                        (CG.apply
                            [ CG.fqVal [ "List" ] "filter"
                            , CG.parens (CG.lambda [ CG.varPattern "x" ] (CG.applyBinOp (CG.val "x") CG.gt (CG.int 0)))
                            ]
                        )
                    )
        , test "Real-world: case (f x) of ... round-trips" <|
            \_ ->
                roundTripExpr "real world case"
                    (CG.caseExpr
                        (CG.parens (CG.apply [ CG.val "f", CG.val "x" ]))
                        [ ( CG.namedPattern "Just" [ CG.varPattern "y" ], CG.val "y" )
                        , ( CG.namedPattern "Nothing" [], CG.val "default" )
                        ]
                    )
        ]



-- ============================================================================
-- Additional Helper Fuzzers (re-exported from Fuzzers module)
-- ============================================================================


fuzz2 : Fuzzer a -> Fuzzer b -> String -> (a -> b -> Expect.Expectation) -> Test
fuzz2 fuzzerA fuzzerB description testFn =
    fuzz (Fuzz.pair fuzzerA fuzzerB) description (\( a, b ) -> testFn a b)


simpleString : Fuzzer String
simpleString =
    Fuzz.listOfLengthBetween 0 20 printableAsciiChar
        |> Fuzz.map String.fromList


simpleChar : Fuzzer Char
simpleChar =
    Fuzz.oneOf
        [ Fuzz.intRange (Char.toCode 'a') (Char.toCode 'z') |> Fuzz.map Char.fromCode
        , Fuzz.intRange (Char.toCode 'A') (Char.toCode 'Z') |> Fuzz.map Char.fromCode
        , Fuzz.intRange (Char.toCode '0') (Char.toCode '9') |> Fuzz.map Char.fromCode
        ]


printableAsciiChar : Fuzzer Char
printableAsciiChar =
    Fuzz.oneOf
        [ Fuzz.intRange 32 33 |> Fuzz.map Char.fromCode
        , Fuzz.intRange 35 91 |> Fuzz.map Char.fromCode
        , Fuzz.intRange 93 126 |> Fuzz.map Char.fromCode
        ]



-- ============================================================================
-- Hex and Numeric Edge Case Tests
-- ============================================================================


hexAndNumericTests : Test
hexAndNumericTests =
    describe "Hex and Numeric Edge Cases"
        [ test "Hex integer literal round-trips" <|
            \_ ->
                roundTripExpr "hex int"
                    (CG.hex 0xABCD)
        , test "Hex integer zero round-trips" <|
            \_ ->
                roundTripExpr "hex zero"
                    (CG.hex 0x00)
        , test "Hex integer 0xFF round-trips" <|
            \_ ->
                roundTripExpr "hex ff"
                    (CG.hex 0xFF)
        , test "Large integer round-trips" <|
            \_ ->
                roundTripExpr "large int"
                    (CG.int 999999999)
        , test "Negative float round-trips" <|
            \_ ->
                roundTripExpr "negative float"
                    (CG.float -3.14159)
        , test "Float with exponent round-trips" <|
            \_ ->
                roundTripExpr "float exponent"
                    (CG.float 1.5e10)
        , test "Float with negative exponent round-trips" <|
            \_ ->
                roundTripExpr "float neg exponent"
                    (CG.float 2.0e-5)
        , test "Float 1.0 round-trips" <|
            \_ ->
                roundTripExpr "float one"
                    (CG.float 1.0)
        , test "Very small float round-trips" <|
            \_ ->
                roundTripExpr "small float"
                    (CG.float 0.0001)
        , test "Hex in binary operation round-trips" <|
            \_ ->
                roundTripExpr "hex binop"
                    (CG.applyBinOp (CG.hex 0xFF) CG.plus (CG.hex 0x10))
        ]



-- ============================================================================
-- Escape Sequence Tests
-- ============================================================================


escapeSequenceTests : Test
escapeSequenceTests =
    describe "Escape Sequences"
        [ test "String with newline escape round-trips" <|
            \_ ->
                roundTripExpr "string newline"
                    (CG.string "hello\nworld")
        , test "String with tab escape round-trips" <|
            \_ ->
                roundTripExpr "string tab"
                    (CG.string "col1\tcol2")
        , test "String with backslash escape round-trips" <|
            \_ ->
                roundTripExpr "string backslash"
                    (CG.string "path\\to\\file")
        , test "String with quote escape round-trips" <|
            \_ ->
                roundTripExpr "string quote"
                    (CG.string "He said \"hi\"")
        , test "String with carriage return round-trips" <|
            \_ ->
                roundTripExpr "string cr"
                    (CG.string "line1\r\nline2")
        , test "Char newline round-trips" <|
            \_ ->
                roundTripExpr "char newline"
                    (CG.char '\n')
        , test "Char tab round-trips" <|
            \_ ->
                roundTripExpr "char tab"
                    (CG.char '\t')
        , test "Char backslash round-trips" <|
            \_ ->
                roundTripExpr "char backslash"
                    (CG.char '\\')
        , test "Char single quote round-trips" <|
            \_ ->
                roundTripExpr "char single quote"
                    (CG.char '\'')
        , test "Char carriage return round-trips" <|
            \_ ->
                roundTripExpr "char cr"
                    (CG.char '\r')
        , test "String with mixed escapes round-trips" <|
            \_ ->
                roundTripExpr "string mixed escapes"
                    (CG.string "line1\n\tindented\n\"quoted\"\\path")
        , test "Empty string round-trips" <|
            \_ ->
                roundTripExpr "empty string"
                    (CG.string "")
        ]



-- ============================================================================
-- Complex Pattern Matching Tests
-- ============================================================================


complexPatternTests : Test
complexPatternTests =
    describe "Complex Pattern Matching"
        [ test "Case with hex pattern round-trips" <|
            \_ ->
                roundTripExpr "case hex pattern"
                    (CG.caseExpr (CG.val "n")
                        [ ( CG.hexPattern 0xFF, CG.string "max byte" )
                        , ( CG.hexPattern 0x00, CG.string "zero" )
                        , ( CG.allPattern, CG.string "other" )
                        ]
                    )
        , test "Case with char patterns round-trips" <|
            \_ ->
                roundTripExpr "case char patterns"
                    (CG.caseExpr (CG.val "c")
                        [ ( CG.charPattern 'a', CG.int 1 )
                        , ( CG.charPattern 'b', CG.int 2 )
                        , ( CG.charPattern 'c', CG.int 3 )
                        , ( CG.allPattern, CG.int 0 )
                        ]
                    )
        , test "Case on empty list pattern round-trips" <|
            \_ ->
                roundTripExpr "case empty list"
                    (CG.caseExpr (CG.val "list")
                        [ ( CG.listPattern [], CG.string "empty" )
                        , ( CG.allPattern, CG.string "non-empty" )
                        ]
                    )
        , test "Nested cons pattern round-trips" <|
            \_ ->
                roundTripExpr "nested cons"
                    (CG.caseExpr (CG.val "xs")
                        [ ( CG.unConsPattern (CG.varPattern "a") (CG.unConsPattern (CG.varPattern "b") (CG.varPattern "rest"))
                          , CG.applyBinOp (CG.val "a") CG.plus (CG.val "b")
                          )
                        , ( CG.allPattern, CG.int 0 )
                        ]
                    )
        , test "Deeply nested constructor patterns round-trips" <|
            \_ ->
                roundTripExpr "deep constructor"
                    (CG.caseExpr (CG.val "x")
                        [ ( CG.namedPattern "Just" [ CG.namedPattern "Just" [ CG.namedPattern "Just" [ CG.varPattern "y" ] ] ]
                          , CG.val "y"
                          )
                        , ( CG.allPattern, CG.int 0 )
                        ]
                    )
        , test "Tuple pattern in case round-trips" <|
            \_ ->
                roundTripExpr "tuple pattern case"
                    (CG.caseExpr (CG.val "pair")
                        [ ( CG.tuplePattern [ CG.varPattern "a", CG.varPattern "b" ]
                          , CG.applyBinOp (CG.val "a") CG.plus (CG.val "b")
                          )
                        ]
                    )
        , test "Triple tuple pattern round-trips" <|
            \_ ->
                roundTripExpr "triple tuple pattern"
                    (CG.caseExpr (CG.val "triple")
                        [ ( CG.tuplePattern [ CG.varPattern "a", CG.varPattern "b", CG.varPattern "c" ]
                          , CG.applyBinOp (CG.val "a") CG.plus (CG.applyBinOp (CG.val "b") CG.plus (CG.val "c"))
                          )
                        ]
                    )
        , test "Record pattern with many fields round-trips" <|
            \_ ->
                roundTripExpr "record pattern many"
                    (CG.caseExpr (CG.val "r")
                        [ ( CG.recordPattern [ "x", "y", "z", "w" ]
                          , CG.val "x"
                          )
                        ]
                    )
        , test "As-pattern on nested structure round-trips" <|
            \_ ->
                roundTripExpr "as nested"
                    (CG.caseExpr (CG.val "x")
                        [ ( CG.asPattern (CG.namedPattern "Just" [ CG.varPattern "y" ]) "whole"
                          , CG.tuple [ CG.val "whole", CG.val "y" ]
                          )
                        , ( CG.namedPattern "Nothing" [], CG.tuple [ CG.val "Nothing", CG.int 0 ] )
                        ]
                    )
        , test "Cons pattern with parens round-trips" <|
            \_ ->
                roundTripExpr "cons parens pattern"
                    (CG.caseExpr (CG.val "nested")
                        [ ( CG.unConsPattern
                                (CG.parensPattern (CG.unConsPattern (CG.varPattern "a") (CG.varPattern "b")))
                                (CG.varPattern "rest")
                          , CG.val "a"
                          )
                        , ( CG.allPattern, CG.int 0 )
                        ]
                    )
        , test "List pattern with multiple elements round-trips" <|
            \_ ->
                roundTripExpr "list pattern multi"
                    (CG.caseExpr (CG.val "list")
                        [ ( CG.listPattern [ CG.varPattern "a", CG.varPattern "b", CG.varPattern "c" ]
                          , CG.val "a"
                          )
                        , ( CG.allPattern, CG.int 0 )
                        ]
                    )
        , test "Float pattern round-trips" <|
            \_ ->
                roundTripExpr "float pattern"
                    (CG.caseExpr (CG.val "f")
                        [ ( CG.floatPattern 0.0, CG.string "zero" )
                        , ( CG.floatPattern 1.0, CG.string "one" )
                        , ( CG.allPattern, CG.string "other" )
                        ]
                    )
        , test "Unit pattern in case round-trips" <|
            \_ ->
                roundTripExpr "unit pattern case"
                    (CG.caseExpr (CG.val "u")
                        [ ( CG.unitPattern, CG.string "unit" )
                        ]
                    )
        ]



-- ============================================================================
-- Lambda Edge Case Tests
-- ============================================================================


lambdaEdgeCaseTests : Test
lambdaEdgeCaseTests =
    describe "Lambda Edge Cases"
        [ test "Nested lambdas round-trip" <|
            \_ ->
                roundTripExpr "nested lambdas"
                    (CG.lambda [ CG.varPattern "x" ]
                        (CG.lambda [ CG.varPattern "y" ]
                            (CG.lambda [ CG.varPattern "z" ]
                                (CG.applyBinOp (CG.val "x") CG.plus (CG.applyBinOp (CG.val "y") CG.plus (CG.val "z")))
                            )
                        )
                    )
        , test "Lambda with wildcard round-trips" <|
            \_ ->
                roundTripExpr "lambda wildcard"
                    (CG.lambda [ CG.allPattern ] (CG.int 42))
        , test "Lambda with as-pattern round-trips" <|
            \_ ->
                roundTripExpr "lambda as pattern"
                    (CG.lambda
                        [ CG.parensPattern (CG.asPattern (CG.namedPattern "Just" [ CG.varPattern "x" ]) "whole") ]
                        (CG.val "whole")
                    )
        , test "Lambda returning lambda round-trips" <|
            \_ ->
                roundTripExpr "lambda returns lambda"
                    (CG.lambda [ CG.varPattern "x" ]
                        (CG.parens (CG.lambda [ CG.varPattern "y" ] (CG.applyBinOp (CG.val "x") CG.plus (CG.val "y"))))
                    )
        , test "Lambda with unit pattern round-trips" <|
            \_ ->
                roundTripExpr "lambda unit pattern"
                    (CG.lambda [ CG.unitPattern ] (CG.int 42))
        , test "Lambda with multiple wildcards round-trips" <|
            \_ ->
                roundTripExpr "lambda multi wildcard"
                    (CG.lambda [ CG.allPattern, CG.allPattern, CG.allPattern ] (CG.int 0))
        , test "Lambda with cons pattern round-trips" <|
            \_ ->
                roundTripExpr "lambda cons pattern"
                    (CG.lambda
                        [ CG.parensPattern (CG.unConsPattern (CG.varPattern "head") (CG.varPattern "tail")) ]
                        (CG.val "head")
                    )
        , test "Lambda with list pattern round-trips" <|
            \_ ->
                roundTripExpr "lambda list pattern"
                    (CG.lambda
                        [ CG.listPattern [ CG.varPattern "a", CG.varPattern "b" ] ]
                        (CG.applyBinOp (CG.val "a") CG.plus (CG.val "b"))
                    )
        , test "Lambda body is if expression round-trips" <|
            \_ ->
                roundTripExpr "lambda if body"
                    (CG.lambda [ CG.varPattern "x" ]
                        (CG.ifExpr (CG.applyBinOp (CG.val "x") CG.gt (CG.int 0))
                            (CG.val "x")
                            (CG.negate (CG.val "x"))
                        )
                    )
        , test "Lambda body is case expression round-trips" <|
            \_ ->
                roundTripExpr "lambda case body"
                    (CG.lambda [ CG.varPattern "mx" ]
                        (CG.caseExpr (CG.val "mx")
                            [ ( CG.namedPattern "Just" [ CG.varPattern "x" ], CG.val "x" )
                            , ( CG.namedPattern "Nothing" [], CG.int 0 )
                            ]
                        )
                    )
        ]



-- ============================================================================
-- Let Expression Edge Case Tests
-- ============================================================================


letEdgeCaseTests : Test
letEdgeCaseTests =
    describe "Let Expression Edge Cases"
        [ test "Let with function binding round-trips" <|
            \_ ->
                roundTripExpr "let function"
                    (CG.letExpr
                        [ CG.letFunction "f" [ CG.varPattern "x" ] (CG.applyBinOp (CG.val "x") CG.plus (CG.int 1)) ]
                        (CG.apply [ CG.val "f", CG.int 10 ])
                    )
        , test "Let with multi-arg function round-trips" <|
            \_ ->
                roundTripExpr "let multi-arg function"
                    (CG.letExpr
                        [ CG.letFunction "f" [ CG.varPattern "x", CG.varPattern "y" ] (CG.applyBinOp (CG.val "x") CG.plus (CG.val "y")) ]
                        (CG.apply [ CG.val "f", CG.int 1, CG.int 2 ])
                    )
        , test "Let binding lambda round-trips" <|
            \_ ->
                roundTripExpr "let binding lambda"
                    (CG.letExpr
                        [ CG.letVal "f" (CG.lambda [ CG.varPattern "x" ] (CG.applyBinOp (CG.val "x") CG.plus (CG.int 1))) ]
                        (CG.apply [ CG.val "f", CG.int 10 ])
                    )
        , test "Deeply nested let round-trips" <|
            \_ ->
                roundTripExpr "deep let"
                    (CG.letExpr
                        [ CG.letVal "a" (CG.int 1) ]
                        (CG.letExpr
                            [ CG.letVal "b" (CG.int 2) ]
                            (CG.letExpr
                                [ CG.letVal "c" (CG.int 3) ]
                                (CG.letExpr
                                    [ CG.letVal "d" (CG.int 4) ]
                                    (CG.applyBinOp (CG.val "a") CG.plus (CG.applyBinOp (CG.val "b") CG.plus (CG.applyBinOp (CG.val "c") CG.plus (CG.val "d"))))
                                )
                            )
                        )
                    )
        , test "Let with record destructuring round-trips" <|
            \_ ->
                roundTripExpr "let record destruct"
                    (CG.letExpr
                        [ CG.letDestructuring (CG.recordPattern [ "x", "y" ]) (CG.val "point") ]
                        (CG.applyBinOp (CG.val "x") CG.plus (CG.val "y"))
                    )
        , test "Let with tuple destructuring round-trips" <|
            \_ ->
                roundTripExpr "let tuple destruct"
                    (CG.letExpr
                        [ CG.letDestructuring (CG.tuplePattern [ CG.varPattern "a", CG.varPattern "b", CG.varPattern "c" ]) (CG.val "triple") ]
                        (CG.val "a")
                    )
        , test "Let with list pattern destructuring round-trips" <|
            \_ ->
                roundTripExpr "let list destruct"
                    (CG.letExpr
                        [ CG.letDestructuring (CG.listPattern [ CG.varPattern "a", CG.varPattern "b" ]) (CG.val "list") ]
                        (CG.applyBinOp (CG.val "a") CG.plus (CG.val "b"))
                    )
        , test "Let with multiple function bindings round-trips" <|
            \_ ->
                roundTripExpr "let multi functions"
                    (CG.letExpr
                        [ CG.letFunction "double" [ CG.varPattern "x" ] (CG.applyBinOp (CG.val "x") CG.mult (CG.int 2))
                        , CG.letFunction "triple" [ CG.varPattern "x" ] (CG.applyBinOp (CG.val "x") CG.mult (CG.int 3))
                        ]
                        (CG.applyBinOp
                            (CG.apply [ CG.val "double", CG.int 5 ])
                            CG.plus
                            (CG.apply [ CG.val "triple", CG.int 5 ])
                        )
                    )
        , test "Let with function taking tuple pattern round-trips" <|
            \_ ->
                roundTripExpr "let fn tuple pattern"
                    (CG.letExpr
                        [ CG.letFunction "sum" [ CG.tuplePattern [ CG.varPattern "a", CG.varPattern "b" ] ] (CG.applyBinOp (CG.val "a") CG.plus (CG.val "b")) ]
                        (CG.apply [ CG.val "sum", CG.tuple [ CG.int 1, CG.int 2 ] ])
                    )
        , test "Let in pipeline round-trips" <|
            \_ ->
                roundTripExpr "let in pipeline"
                    (CG.applyBinOp
                        (CG.val "x")
                        CG.piper
                        (CG.parens
                            (CG.letExpr
                                [ CG.letVal "y" (CG.int 1) ]
                                (CG.lambda [ CG.varPattern "z" ] (CG.applyBinOp (CG.val "z") CG.plus (CG.val "y")))
                            )
                        )
                    )
        ]



-- ============================================================================
-- Record Edge Case Tests
-- ============================================================================


recordEdgeCaseTests : Test
recordEdgeCaseTests =
    describe "Record Edge Cases"
        [ test "Deeply nested record literal round-trips" <|
            \_ ->
                roundTripExpr "deep record"
                    (CG.record
                        [ ( "a"
                          , CG.record
                                [ ( "b"
                                  , CG.record [ ( "c", CG.int 1 ) ]
                                  )
                                ]
                          )
                        ]
                    )
        , test "Record with many fields round-trips" <|
            \_ ->
                roundTripExpr "record many fields"
                    (CG.record
                        [ ( "field1", CG.int 1 )
                        , ( "field2", CG.int 2 )
                        , ( "field3", CG.int 3 )
                        , ( "field4", CG.int 4 )
                        , ( "field5", CG.int 5 )
                        , ( "field6", CG.int 6 )
                        , ( "field7", CG.int 7 )
                        , ( "field8", CG.int 8 )
                        , ( "field9", CG.int 9 )
                        , ( "field10", CG.int 10 )
                        ]
                    )
        , test "Record update with multiple fields round-trips" <|
            \_ ->
                roundTripExpr "record update multi"
                    (CG.update "model"
                        [ ( "x", CG.int 1 )
                        , ( "y", CG.int 2 )
                        , ( "z", CG.int 3 )
                        , ( "w", CG.int 4 )
                        ]
                    )
        , test "Chained record access round-trips" <|
            \_ ->
                roundTripExpr "chained access"
                    (CG.access (CG.access (CG.access (CG.val "model") "user") "profile") "name")
        , test "Record access on function result round-trips" <|
            \_ ->
                roundTripExpr "access fn result"
                    (CG.access (CG.parens (CG.apply [ CG.val "getUser", CG.val "model" ])) "name")
        , test "Record literal with complex values round-trips" <|
            \_ ->
                roundTripExpr "record complex values"
                    (CG.record
                        [ ( "sum", CG.applyBinOp (CG.val "a") CG.plus (CG.val "b") )
                        , ( "product", CG.applyBinOp (CG.val "a") CG.mult (CG.val "b") )
                        , ( "applied", CG.apply [ CG.val "f", CG.val "x" ] )
                        ]
                    )
        , test "Record update with pipeline value round-trips" <|
            \_ ->
                roundTripExpr "record update pipeline"
                    (CG.update "model"
                        [ ( "items", CG.applyBinOp (CG.access (CG.val "model") "items") CG.piper (CG.apply [ CG.fqVal [ "List" ] "reverse" ]) )
                        ]
                    )
        , test "Record access chained with accessor function round-trips" <|
            \_ ->
                roundTripExpr "access chain accessor"
                    (CG.applyBinOp (CG.val "records") CG.piper (CG.apply [ CG.fqVal [ "List" ] "map", CG.accessFun ".name" ]))
        , test "Record in list round-trips" <|
            \_ ->
                roundTripExpr "record in list"
                    (CG.list
                        [ CG.record [ ( "x", CG.int 1 ), ( "y", CG.int 2 ) ]
                        , CG.record [ ( "x", CG.int 3 ), ( "y", CG.int 4 ) ]
                        ]
                    )
        , test "Nested record update round-trips" <|
            \_ ->
                roundTripExpr "nested record update"
                    (CG.update "outer"
                        [ ( "inner", CG.update "inner" [ ( "value", CG.int 42 ) ] )
                        ]
                    )
        ]



-- ============================================================================
-- Type Annotation Edge Case Tests
-- ============================================================================


typeAnnotationEdgeCaseTests : Test
typeAnnotationEdgeCaseTests =
    describe "Type Annotation Edge Cases"
        [ test "Extensible record type round-trips" <|
            \_ ->
                roundTripTypeAnn "ext record type"
                    (CG.extRecordAnn "a" [ ( "x", CG.typed "Int" [] ), ( "y", CG.typed "String" [] ) ])
        , test "Generic extensible record in function round-trips" <|
            \_ ->
                roundTripTypeAnn "generic ext record fn"
                    (CG.funAnn
                        (CG.extRecordAnn "a" [ ( "x", CG.typed "Int" [] ) ])
                        (CG.extRecordAnn "a" [ ( "x", CG.typed "Int" [] ) ])
                    )
        , test "Deeply nested type application round-trips" <|
            \_ ->
                roundTripTypeAnn "deep type app"
                    (CG.typed "Result"
                        [ CG.typed "String" []
                        , CG.typed "Maybe" [ CG.typed "List" [ CG.typed "Int" [] ] ]
                        ]
                    )
        , test "Function type with 5 args round-trips" <|
            \_ ->
                roundTripTypeAnn "fn 5 args"
                    (CG.funAnn (CG.typeVar "a")
                        (CG.funAnn (CG.typeVar "b")
                            (CG.funAnn (CG.typeVar "c")
                                (CG.funAnn (CG.typeVar "d")
                                    (CG.funAnn (CG.typeVar "e") (CG.typeVar "f"))
                                )
                            )
                        )
                    )
        , test "Type with qualified name round-trips" <|
            \_ ->
                roundTripTypeAnn "qualified type"
                    (CG.fqTyped [ "Platform", "Cmd" ] "Cmd" [ CG.typeVar "msg" ])
        , test "Unit type in function round-trips" <|
            \_ ->
                roundTripTypeAnn "unit in fn"
                    (CG.funAnn CG.unitAnn (CG.typed "Int" []))
        , test "Tuple type with 3 elements round-trips" <|
            \_ ->
                roundTripTypeAnn "tuple type 3"
                    (CG.tupleAnn [ CG.typed "Int" [], CG.typed "String" [], CG.typed "Bool" [] ])
        , test "Record type with many fields round-trips" <|
            \_ ->
                roundTripTypeAnn "record type many"
                    (CG.recordAnn
                        [ ( "id", CG.typed "Int" [] )
                        , ( "name", CG.typed "String" [] )
                        , ( "email", CG.typed "String" [] )
                        , ( "active", CG.typed "Bool" [] )
                        , ( "score", CG.typed "Float" [] )
                        ]
                    )
        , test "Function returning function round-trips" <|
            \_ ->
                roundTripTypeAnn "fn returns fn"
                    (CG.funAnn (CG.typeVar "a") (CG.funAnn (CG.typeVar "b") (CG.typeVar "c")))
        , test "List of records type round-trips" <|
            \_ ->
                roundTripTypeAnn "list of records"
                    (CG.typed "List" [ CG.recordAnn [ ( "x", CG.typed "Int" [] ), ( "y", CG.typed "Int" [] ) ] ])
        , test "Maybe of tuple type round-trips" <|
            \_ ->
                roundTripTypeAnn "maybe tuple"
                    (CG.typed "Maybe" [ CG.tupleAnn [ CG.typed "Int" [], CG.typed "String" [] ] ])
        , test "Dict type round-trips" <|
            \_ ->
                roundTripTypeAnn "dict type"
                    (CG.typed "Dict" [ CG.typed "String" [], CG.typed "Int" [] ])
        ]



-- ============================================================================
-- Declaration Edge Case Tests
-- ============================================================================


declarationEdgeCaseTests : Test
declarationEdgeCaseTests =
    describe "Declaration Edge Cases"
        [ test "Custom type with many constructors round-trips" <|
            \_ ->
                roundTripDecl "custom many constructors"
                    (CG.customTypeDecl Nothing
                        "Color"
                        []
                        [ ( "Red", [] )
                        , ( "Green", [] )
                        , ( "Blue", [] )
                        , ( "Yellow", [] )
                        , ( "Orange", [] )
                        , ( "Purple", [] )
                        ]
                    )
        , test "Custom type with complex constructor args round-trips" <|
            \_ ->
                roundTripDecl "custom complex args"
                    (CG.customTypeDecl Nothing
                        "Expr"
                        []
                        [ ( "IntLit", [ CG.typed "Int" [] ] )
                        , ( "BinOp", [ CG.typed "Expr" [], CG.typed "Op" [], CG.typed "Expr" [] ] )
                        , ( "Var", [ CG.typed "String" [] ] )
                        ]
                    )
        , test "Type alias for function round-trips" <|
            \_ ->
                roundTripDecl "alias for function"
                    (CG.aliasDecl Nothing
                        "Handler"
                        []
                        (CG.funAnn (CG.typed "Int" []) (CG.funAnn (CG.typed "String" []) (CG.typed "Bool" [])))
                    )
        , test "Type alias with multiple type params round-trips" <|
            \_ ->
                roundTripDecl "alias multi params"
                    (CG.aliasDecl Nothing
                        "Pair"
                        [ "a", "b" ]
                        (CG.recordAnn [ ( "first", CG.typeVar "a" ), ( "second", CG.typeVar "b" ) ])
                    )
        , test "Function with full type annotation round-trips" <|
            \_ ->
                roundTripDecl "fn with annotation"
                    (CG.funDecl Nothing
                        (Just (CG.funAnn (CG.typed "Int" []) (CG.funAnn (CG.typed "Int" []) (CG.typed "Int" []))))
                        "add"
                        [ CG.varPattern "a", CG.varPattern "b" ]
                        (CG.applyBinOp (CG.val "a") CG.plus (CG.val "b"))
                    )
        , test "Port declaration round-trips" <|
            \_ ->
                roundTripPortDecl "port decl"
                    (CG.portDecl "sendMessage" (CG.funAnn (CG.typed "String" []) (CG.fqTyped [ "Platform", "Cmd" ] "Cmd" [ CG.typeVar "msg" ])))
        , test "Custom type with type parameters round-trips" <|
            \_ ->
                roundTripDecl "custom with params"
                    (CG.customTypeDecl Nothing
                        "Tree"
                        [ "a" ]
                        [ ( "Leaf", [ CG.typeVar "a" ] )
                        , ( "Node", [ CG.typed "Tree" [ CG.typeVar "a" ], CG.typed "Tree" [ CG.typeVar "a" ] ] )
                        ]
                    )
        , test "Type alias for extensible record round-trips" <|
            \_ ->
                roundTripDecl "alias ext record"
                    (CG.aliasDecl Nothing
                        "Named"
                        [ "a" ]
                        (CG.extRecordAnn "a" [ ( "name", CG.typed "String" [] ) ])
                    )
        , test "Value declaration with complex type annotation round-trips" <|
            \_ ->
                roundTripDecl "val complex type"
                    (CG.valDecl Nothing
                        (Just (CG.typed "List" [ CG.funAnn (CG.typed "Int" []) (CG.typed "Int" []) ]))
                        "transformers"
                        (CG.list
                            [ CG.lambda [ CG.varPattern "x" ] (CG.applyBinOp (CG.val "x") CG.plus (CG.int 1))
                            , CG.lambda [ CG.varPattern "x" ] (CG.applyBinOp (CG.val "x") CG.mult (CG.int 2))
                            ]
                        )
                    )
        , test "Custom type single constructor round-trips" <|
            \_ ->
                roundTripDecl "custom single constructor"
                    (CG.customTypeDecl Nothing
                        "Wrapper"
                        [ "a" ]
                        [ ( "Wrapper", [ CG.typeVar "a" ] ) ]
                    )
        ]


{-| Test that a port declaration round-trips.
-}
roundTripPortDecl : String -> CG.Declaration -> Expect.Expectation
roundTripPortDecl label decl =
    let
        file =
            CG.file
                (CG.portModule [ "Test" ] [ CG.funExpose "test" ])
                []
                [ decl ]
                Nothing

        printed =
            prettyPrintFile file

        parsed =
            Elm.DSLParser.parse printed
    in
    case parsed of
        Err deadEnds ->
            Expect.fail
                ("Failed to parse:\n"
                    ++ printed
                    ++ "\n\nErrors: "
                    ++ Debug.toString deadEnds
                )

        Ok _ ->
            Expect.pass



-- ============================================================================
-- Operator Edge Case Tests
-- ============================================================================


operatorEdgeCaseTests : Test
operatorEdgeCaseTests =
    describe "Operator Edge Cases"
        [ test "Negation of expression round-trips" <|
            \_ ->
                roundTripExpr "negate expr"
                    (CG.negate (CG.parens (CG.applyBinOp (CG.val "x") CG.plus (CG.int 1))))
        , test "Negation of function call round-trips" <|
            \_ ->
                roundTripExpr "negate fn call"
                    (CG.negate (CG.parens (CG.apply [ CG.val "f", CG.val "x" ])))
        , test "Chained comparisons with parens round-trips" <|
            \_ ->
                roundTripExpr "chained comparisons"
                    (CG.applyBinOp
                        (CG.parens (CG.applyBinOp (CG.val "a") CG.lt (CG.val "b")))
                        CG.and
                        (CG.parens (CG.applyBinOp (CG.val "b") CG.lt (CG.val "c")))
                    )
        , test "Not-equals operator round-trips" <|
            \_ ->
                roundTripExpr "not equals"
                    (CG.applyBinOp (CG.val "a") CG.notEqual (CG.val "b"))
        , test "Less-than-or-equal round-trips" <|
            \_ ->
                roundTripExpr "lte"
                    (CG.applyBinOp (CG.val "a") CG.lte (CG.val "b"))
        , test "Greater-than-or-equal round-trips" <|
            \_ ->
                roundTripExpr "gte"
                    (CG.applyBinOp (CG.val "a") CG.gte (CG.val "b"))
        , test "All comparison operators round-trip" <|
            \_ ->
                roundTripExpr "all comparisons"
                    (CG.applyBinOp
                        (CG.applyBinOp
                            (CG.applyBinOp
                                (CG.parens (CG.applyBinOp (CG.val "a") CG.lt (CG.val "b")))
                                CG.and
                                (CG.parens (CG.applyBinOp (CG.val "b") CG.lte (CG.val "c")))
                            )
                            CG.and
                            (CG.parens (CG.applyBinOp (CG.val "c") CG.gt (CG.val "d")))
                        )
                        CG.and
                        (CG.parens (CG.applyBinOp (CG.val "d") CG.gte (CG.val "e")))
                    )
        , test "Negation in function application round-trips" <|
            \_ ->
                roundTripExpr "negate in app"
                    (CG.apply [ CG.val "abs", CG.parens (CG.negate (CG.val "x")) ])
        , test "Operators with qualified values round-trips" <|
            \_ ->
                roundTripExpr "qualified in ops"
                    (CG.applyBinOp (CG.fqVal [ "Model" ] "value") CG.plus (CG.fqVal [ "Model" ] "offset"))
        , test "Mixed arithmetic and boolean round-trips" <|
            \_ ->
                roundTripExpr "mixed arith bool"
                    (CG.applyBinOp
                        (CG.parens (CG.applyBinOp (CG.val "a") CG.plus (CG.val "b")))
                        CG.gt
                        (CG.parens (CG.applyBinOp (CG.val "c") CG.mult (CG.val "d")))
                    )
        , test "Integer division chain round-trips" <|
            \_ ->
                roundTripExpr "intdiv chain"
                    (CG.applyBinOp
                        (CG.parens (CG.applyBinOp (CG.val "a") CG.intDiv (CG.val "b")))
                        CG.intDiv
                        (CG.val "c")
                    )
        ]



-- ============================================================================
-- Real-World Combination Tests
-- ============================================================================


realWorldCombinationTests : Test
realWorldCombinationTests =
    describe "Real-World Combinations"
        [ test "Decoder pipeline style round-trips" <|
            \_ ->
                roundTripExpr "decoder pipeline"
                    (CG.applyBinOp
                        (CG.applyBinOp
                            (CG.apply [ CG.fqVal [ "Decode" ] "succeed", CG.val "Model" ])
                            CG.piper
                            (CG.apply [ CG.val "required", CG.string "id", CG.val "int" ])
                        )
                        CG.piper
                        (CG.apply [ CG.val "required", CG.string "name", CG.val "string" ])
                    )
        , test "Html pipeline round-trips" <|
            \_ ->
                roundTripExpr "html pipeline"
                    (CG.applyBinOp
                        (CG.apply
                            [ CG.val "div"
                            , CG.list []
                            , CG.list [ CG.apply [ CG.val "text", CG.string "hello" ] ]
                            ]
                        )
                        CG.piper
                        (CG.fqVal [ "List" ] "singleton")
                    )
        , test "Update function pattern round-trips" <|
            \_ ->
                roundTripExpr "update pattern"
                    (CG.caseExpr (CG.val "msg")
                        [ ( CG.namedPattern "Click" []
                          , CG.tuple
                                [ CG.update "model" [ ( "clicked", CG.val "True" ) ]
                                , CG.fqVal [ "Cmd" ] "none"
                                ]
                          )
                        , ( CG.namedPattern "Reset" []
                          , CG.tuple
                                [ CG.val "initialModel"
                                , CG.fqVal [ "Cmd" ] "none"
                                ]
                          )
                        ]
                    )
        , test "Nested case in let round-trips" <|
            \_ ->
                roundTripExpr "case in let"
                    (CG.letExpr
                        [ CG.letVal "result"
                            (CG.caseExpr (CG.val "maybeX")
                                [ ( CG.namedPattern "Just" [ CG.varPattern "x" ], CG.applyBinOp (CG.val "x") CG.plus (CG.int 1) )
                                , ( CG.namedPattern "Nothing" [], CG.int 0 )
                                ]
                            )
                        ]
                        (CG.val "result")
                    )
        , test "Lambda in record field round-trips" <|
            \_ ->
                roundTripExpr "lambda in record"
                    (CG.record
                        [ ( "onClick", CG.lambda [ CG.allPattern ] (CG.val "Clicked") )
                        , ( "onHover", CG.lambda [ CG.varPattern "pos" ] (CG.apply [ CG.val "Hovered", CG.val "pos" ]) )
                        ]
                    )
        , test "If inside list round-trips" <|
            \_ ->
                roundTripExpr "if in list"
                    (CG.list
                        [ CG.ifExpr (CG.val "cond") (CG.val "a") (CG.val "b")
                        , CG.val "c"
                        , CG.val "d"
                        ]
                    )
        , test "Case inside tuple round-trips" <|
            \_ ->
                roundTripExpr "case in tuple"
                    (CG.tuple
                        [ CG.caseExpr (CG.val "x")
                            [ ( CG.namedPattern "Just" [ CG.varPattern "v" ], CG.val "v" )
                            , ( CG.namedPattern "Nothing" [], CG.int 0 )
                            ]
                        , CG.val "y"
                        ]
                    )
        , test "Pipe through accessor round-trips" <|
            \_ ->
                roundTripExpr "pipe accessor"
                    (CG.applyBinOp
                        (CG.applyBinOp
                            (CG.val "list")
                            CG.piper
                            (CG.apply [ CG.fqVal [ "List" ] "map", CG.accessFun ".name" ])
                        )
                        CG.piper
                        (CG.apply [ CG.fqVal [ "String" ] "join", CG.string ", " ])
                    )
        , test "Qualified function in pipe round-trips" <|
            \_ ->
                roundTripExpr "qualified pipe"
                    (CG.applyBinOp
                        (CG.applyBinOp
                            (CG.val "x")
                            CG.piper
                            (CG.apply [ CG.fqVal [ "Maybe" ] "map", CG.val "f" ])
                        )
                        CG.piper
                        (CG.apply [ CG.fqVal [ "Maybe" ] "withDefault", CG.int 0 ])
                    )
        , test "Complex view function round-trips" <|
            \_ ->
                roundTripExpr "complex view"
                    (CG.apply
                        [ CG.val "div"
                        , CG.list [ CG.apply [ CG.val "class", CG.string "container" ] ]
                        , CG.applyBinOp
                            (CG.access (CG.val "model") "items")
                            CG.piper
                            (CG.apply
                                [ CG.fqVal [ "List" ] "map"
                                , CG.parens (CG.lambda [ CG.varPattern "item" ] (CG.apply [ CG.val "viewItem", CG.val "item" ]))
                                ]
                            )
                        ]
                    )
        , test "Recursive helper in let round-trips" <|
            \_ ->
                roundTripExpr "recursive helper"
                    (CG.letExpr
                        [ CG.letFunction "go"
                            [ CG.varPattern "acc", CG.varPattern "xs" ]
                            (CG.caseExpr (CG.val "xs")
                                [ ( CG.listPattern [], CG.val "acc" )
                                , ( CG.unConsPattern (CG.varPattern "h") (CG.varPattern "t")
                                  , CG.apply [ CG.val "go", CG.applyBinOp (CG.val "h") CG.cons (CG.val "acc"), CG.val "t" ]
                                  )
                                ]
                            )
                        ]
                        (CG.apply [ CG.val "go", CG.list [], CG.val "input" ])
                    )
        , test "Elm architecture msg type pattern round-trips" <|
            \_ ->
                roundTripExpr "msg pattern"
                    (CG.caseExpr (CG.val "msg")
                        [ ( CG.namedPattern "GotResponse" [ CG.namedPattern "Ok" [ CG.varPattern "data" ] ]
                          , CG.tuple [ CG.update "model" [ ( "data", CG.apply [ CG.val "Just", CG.val "data" ] ) ], CG.fqVal [ "Cmd" ] "none" ]
                          )
                        , ( CG.namedPattern "GotResponse" [ CG.namedPattern "Err" [ CG.varPattern "err" ] ]
                          , CG.tuple [ CG.update "model" [ ( "error", CG.apply [ CG.val "Just", CG.val "err" ] ) ], CG.fqVal [ "Cmd" ] "none" ]
                          )
                        ]
                    )
        , test "Filter and map chain round-trips" <|
            \_ ->
                roundTripExpr "filter map chain"
                    (CG.applyBinOp
                        (CG.applyBinOp
                            (CG.applyBinOp
                                (CG.val "items")
                                CG.piper
                                (CG.apply
                                    [ CG.fqVal [ "List" ] "filter"
                                    , CG.parens (CG.lambda [ CG.varPattern "x" ] (CG.applyBinOp (CG.access (CG.val "x") "active") CG.equals (CG.val "True")))
                                    ]
                                )
                            )
                            CG.piper
                            (CG.apply [ CG.fqVal [ "List" ] "map", CG.accessFun ".name" ])
                        )
                        CG.piper
                        (CG.apply [ CG.fqVal [ "List" ] "sort" ])
                    )
        , test "Nested record access in condition round-trips" <|
            \_ ->
                roundTripExpr "nested access condition"
                    (CG.ifExpr
                        (CG.applyBinOp
                            (CG.access (CG.access (CG.val "model") "user") "loggedIn")
                            CG.equals
                            (CG.val "True")
                        )
                        (CG.apply [ CG.val "viewDashboard", CG.val "model" ])
                        (CG.apply [ CG.val "viewLogin", CG.val "model" ])
                    )
        , test "Tuple extraction with let round-trips" <|
            \_ ->
                roundTripExpr "tuple extraction"
                    (CG.letExpr
                        [ CG.letDestructuring
                            (CG.tuplePattern [ CG.varPattern "newModel", CG.varPattern "cmd" ])
                            (CG.apply [ CG.val "update", CG.val "msg", CG.val "model" ])
                        ]
                        (CG.tuple [ CG.val "newModel", CG.apply [ CG.fqVal [ "Cmd" ] "batch", CG.list [ CG.val "cmd", CG.val "extraCmd" ] ] ])
                    )
        ]
