module Fuzzers exposing
    ( -- Expression Fuzzers
      intExpr
    , floatExpr
    , stringExpr
    , charExpr
    , unitExpr
    , varExpr
    , qualifiedVarExpr
    , constructorExpr
    , simpleApplicationExpr
    , nestedApplicationExpr
    , ifExpr
    , caseExpr
    , letExpr
    , lambdaExpr
    , listExpr
    , tupleExpr
    , recordExpr
    , recordUpdateExpr
    , recordAccessExpr
    , binaryOpExpr
    , pipeRightExpr
    , pipeLeftExpr
    , negationExpr
    , parensExpr
    , literalExpr
    , expression

    -- Pattern Fuzzers
    , varPattern
    , wildcardPattern
    , intPattern
    , stringPattern
    , charPattern
    , tuplePattern
    , listPattern
    , consPattern
    , constructorPattern
    , asPattern
    , recordPattern
    , pattern

    -- Type Annotation Fuzzers
    , simpleTypeAnn
    , typeVarAnn
    , functionTypeAnn
    , tupleTypeAnn
    , recordTypeAnn
    , parameterizedTypeAnn
    , typeAnnotation

    -- Declaration Fuzzers
    , valDecl
    , funDecl
    , typeAliasDecl
    , customTypeDecl

    -- Helper Fuzzers
    , identifier
    , typeIdentifier
    , moduleName
    , simpleString
    , simpleChar
    )

{-| Fuzzers for generating random Elm AST constructs.

These fuzzers generate Elm code using Elm.CodeGen, which can then be
pretty-printed and re-parsed to test round-trip behavior.

-}

import Elm.CodeGen as CG
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Fuzz exposing (Fuzzer)


-- ============================================================================
-- Helper Fuzzers
-- ============================================================================


{-| Generate a valid Elm identifier (lowercase start).
Avoids reserved words.
-}
identifier : Fuzzer String
identifier =
    Fuzz.map2
        (\first rest ->
            let
                candidate =
                    String.cons first rest
            in
            -- Prefix with 'x' if it's a reserved word
            if isReservedWord candidate then
                "x" ++ candidate

            else
                candidate
        )
        lowerChar
        (Fuzz.map String.fromList (Fuzz.listOfLengthBetween 1 8 alphaNumChar))


{-| Check if a string is a reserved word in Elm.
-}
isReservedWord : String -> Bool
isReservedWord word =
    List.member word
        [ "if"
        , "then"
        , "else"
        , "case"
        , "of"
        , "let"
        , "in"
        , "type"
        , "module"
        , "where"
        , "import"
        , "exposing"
        , "as"
        , "port"
        ]


{-| Generate a valid Elm type identifier (uppercase start).
-}
typeIdentifier : Fuzzer String
typeIdentifier =
    Fuzz.map2
        (\first rest ->
            String.cons first rest
        )
        upperChar
        (Fuzz.map String.fromList (Fuzz.listOfLengthBetween 0 8 alphaNumChar))


{-| Generate a module name (list of capitalized parts).
-}
moduleName : Fuzzer (List String)
moduleName =
    Fuzz.listOfLengthBetween 1 3 typeIdentifier


lowerChar : Fuzzer Char
lowerChar =
    Fuzz.intRange (Char.toCode 'a') (Char.toCode 'z')
        |> Fuzz.map Char.fromCode


upperChar : Fuzzer Char
upperChar =
    Fuzz.intRange (Char.toCode 'A') (Char.toCode 'Z')
        |> Fuzz.map Char.fromCode


alphaNumChar : Fuzzer Char
alphaNumChar =
    Fuzz.oneOf
        [ lowerChar
        , upperChar
        , Fuzz.intRange (Char.toCode '0') (Char.toCode '9')
            |> Fuzz.map Char.fromCode
        ]


{-| Generate a simple string without problematic characters.
-}
simpleString : Fuzzer String
simpleString =
    Fuzz.listOfLengthBetween 0 20 printableAsciiChar
        |> Fuzz.map String.fromList


printableAsciiChar : Fuzzer Char
printableAsciiChar =
    -- Printable ASCII excluding backslash, quotes, and control chars
    Fuzz.oneOf
        [ Fuzz.intRange 32 33 |> Fuzz.map Char.fromCode -- space and !
        , Fuzz.intRange 35 91 |> Fuzz.map Char.fromCode -- # to [
        , Fuzz.intRange 93 126 |> Fuzz.map Char.fromCode -- ] to ~
        ]


simpleChar : Fuzzer Char
simpleChar =
    Fuzz.oneOf
        [ lowerChar
        , upperChar
        , Fuzz.intRange (Char.toCode '0') (Char.toCode '9')
            |> Fuzz.map Char.fromCode
        ]



-- ============================================================================
-- Expression Fuzzers
-- ============================================================================


{-| Generate an integer literal expression.
-}
intExpr : Fuzzer Expression
intExpr =
    Fuzz.int
        |> Fuzz.map CG.int


{-| Generate a float literal expression.
-}
floatExpr : Fuzzer Expression
floatExpr =
    Fuzz.niceFloat
        |> Fuzz.map CG.float


{-| Generate a string literal expression.
-}
stringExpr : Fuzzer Expression
stringExpr =
    simpleString
        |> Fuzz.map CG.string


{-| Generate a char literal expression.
-}
charExpr : Fuzzer Expression
charExpr =
    simpleChar
        |> Fuzz.map CG.char


{-| Generate a unit expression.
-}
unitExpr : Fuzzer Expression
unitExpr =
    Fuzz.constant CG.unit


{-| Generate a simple variable reference.
-}
varExpr : Fuzzer Expression
varExpr =
    identifier
        |> Fuzz.map CG.val


{-| Generate a qualified variable reference (e.g., Module.function).
-}
qualifiedVarExpr : Fuzzer Expression
qualifiedVarExpr =
    Fuzz.map2 CG.fqVal moduleName identifier


{-| Generate a constructor expression (e.g., Just, Nothing).
-}
constructorExpr : Fuzzer Expression
constructorExpr =
    Fuzz.oneOf
        [ Fuzz.constant (CG.val "Nothing")
        , Fuzz.map (\x -> CG.apply [ CG.val "Just", x ]) literalExpr
        , Fuzz.constant (CG.val "True")
        , Fuzz.constant (CG.val "False")
        ]


{-| Generate a simple function application (f x).
-}
simpleApplicationExpr : Fuzzer Expression
simpleApplicationExpr =
    Fuzz.map2
        (\fn arg -> CG.apply [ CG.val fn, arg ])
        identifier
        literalExpr


{-| Generate nested function application (f (g x)).
-}
nestedApplicationExpr : Fuzzer Expression
nestedApplicationExpr =
    Fuzz.map3
        (\f g arg ->
            CG.apply [ CG.val f, CG.parens (CG.apply [ CG.val g, arg ]) ]
        )
        identifier
        identifier
        literalExpr


{-| Generate an if-then-else expression.
-}
ifExpr : Fuzzer Expression
ifExpr =
    Fuzz.map3 CG.ifExpr
        (Fuzz.constant (CG.val "condition"))
        literalExpr
        literalExpr


{-| Generate a case expression.
-}
caseExpr : Fuzzer Expression
caseExpr =
    Fuzz.map2
        (\scrutinee result ->
            CG.caseExpr scrutinee
                [ ( CG.namedPattern "Just" [ CG.varPattern "x" ], CG.val "x" )
                , ( CG.namedPattern "Nothing" [], result )
                ]
        )
        (Fuzz.constant (CG.val "maybeValue"))
        literalExpr


{-| Generate a let expression.
-}
letExpr : Fuzzer Expression
letExpr =
    Fuzz.map3
        (\name1 val1 body ->
            CG.letExpr
                [ CG.letVal name1 val1 ]
                body
        )
        identifier
        literalExpr
        literalExpr


{-| Generate a lambda expression.
-}
lambdaExpr : Fuzzer Expression
lambdaExpr =
    Fuzz.map2
        (\argName body ->
            CG.lambda [ CG.varPattern argName ] body
        )
        identifier
        literalExpr


{-| Generate a list expression.
-}
listExpr : Fuzzer Expression
listExpr =
    Fuzz.listOfLengthBetween 0 5 literalExpr
        |> Fuzz.map CG.list


{-| Generate a tuple expression.
-}
tupleExpr : Fuzzer Expression
tupleExpr =
    Fuzz.map2
        (\a b -> CG.tuple [ a, b ])
        literalExpr
        literalExpr


{-| Generate a record expression.
-}
recordExpr : Fuzzer Expression
recordExpr =
    Fuzz.listOfLengthBetween 1 4
        (Fuzz.map2 Tuple.pair identifier literalExpr)
        |> Fuzz.map CG.record


{-| Generate a record update expression.
-}
recordUpdateExpr : Fuzzer Expression
recordUpdateExpr =
    Fuzz.map3
        (\recName fieldName newVal ->
            CG.update recName [ ( fieldName, newVal ) ]
        )
        identifier
        identifier
        literalExpr


{-| Generate a record access expression.
-}
recordAccessExpr : Fuzzer Expression
recordAccessExpr =
    Fuzz.map2 CG.access
        varExpr
        identifier


{-| Generate a binary operator expression.
-}
binaryOpExpr : Fuzzer Expression
binaryOpExpr =
    Fuzz.map3
        (\left op right ->
            CG.applyBinOp left op right
        )
        literalExpr
        binOp
        literalExpr


{-| Generate a pipe right expression (|>).
-}
pipeRightExpr : Fuzzer Expression
pipeRightExpr =
    Fuzz.map2
        (\start fns ->
            CG.pipe start (List.map CG.val fns)
        )
        literalExpr
        (Fuzz.listOfLengthBetween 1 3 identifier)


{-| Generate a pipe left expression (<|).
-}
pipeLeftExpr : Fuzzer Expression
pipeLeftExpr =
    Fuzz.map2
        (\fn arg ->
            CG.applyBinOp (CG.val fn) CG.pipel arg
        )
        identifier
        literalExpr


{-| Generate a negation expression.
-}
negationExpr : Fuzzer Expression
negationExpr =
    Fuzz.map CG.negate literalExpr


{-| Generate a parenthesized expression.
-}
parensExpr : Fuzzer Expression
parensExpr =
    literalExpr
        |> Fuzz.map CG.parens


{-| Generate any simple literal expression.
-}
literalExpr : Fuzzer Expression
literalExpr =
    Fuzz.oneOf
        [ intExpr
        , floatExpr
        , stringExpr
        , charExpr
        , unitExpr
        , varExpr
        ]


{-| Generate a binary operator.
-}
binOp : Fuzzer CG.BinOp
binOp =
    Fuzz.oneOf
        [ Fuzz.constant CG.plus
        , Fuzz.constant CG.minus
        , Fuzz.constant CG.mult
        , Fuzz.constant CG.div
        , Fuzz.constant CG.equals
        , Fuzz.constant CG.notEqual
        , Fuzz.constant CG.lt
        , Fuzz.constant CG.gt
        , Fuzz.constant CG.and
        , Fuzz.constant CG.or
        , Fuzz.constant CG.append
        , Fuzz.constant CG.cons
        ]


{-| Generate any expression (limited depth).
-}
expression : Fuzzer Expression
expression =
    Fuzz.oneOf
        [ literalExpr
        , simpleApplicationExpr
        , ifExpr
        , listExpr
        , tupleExpr
        , recordExpr
        , binaryOpExpr
        , lambdaExpr
        ]



-- ============================================================================
-- Pattern Fuzzers
-- ============================================================================


{-| Generate a variable pattern.
-}
varPattern : Fuzzer Pattern
varPattern =
    identifier
        |> Fuzz.map CG.varPattern


{-| Generate a wildcard pattern.
-}
wildcardPattern : Fuzzer Pattern
wildcardPattern =
    Fuzz.constant CG.allPattern


{-| Generate an integer pattern.
Note: Elm doesn't support negative integer patterns.
-}
intPattern : Fuzzer Pattern
intPattern =
    Fuzz.intRange 0 1000
        |> Fuzz.map CG.intPattern


{-| Generate a string pattern.
-}
stringPattern : Fuzzer Pattern
stringPattern =
    simpleString
        |> Fuzz.map CG.stringPattern


{-| Generate a char pattern.
-}
charPattern : Fuzzer Pattern
charPattern =
    simpleChar
        |> Fuzz.map CG.charPattern


{-| Generate a tuple pattern.
-}
tuplePattern : Fuzzer Pattern
tuplePattern =
    Fuzz.map2
        (\a b -> CG.tuplePattern [ a, b ])
        simplePattern
        simplePattern


{-| Generate a list pattern.
-}
listPattern : Fuzzer Pattern
listPattern =
    Fuzz.listOfLengthBetween 0 3 simplePattern
        |> Fuzz.map CG.listPattern


{-| Generate a cons pattern (x :: xs).
-}
consPattern : Fuzzer Pattern
consPattern =
    Fuzz.map2 CG.unConsPattern
        simplePattern
        (identifier |> Fuzz.map CG.varPattern)


{-| Generate a constructor pattern (e.g., Just x).
-}
constructorPattern : Fuzzer Pattern
constructorPattern =
    Fuzz.oneOf
        [ Fuzz.constant (CG.namedPattern "Nothing" [])
        , Fuzz.map (\p -> CG.namedPattern "Just" [ p ]) simplePattern
        , Fuzz.map2
            (\p1 p2 -> CG.namedPattern "Ok" [ p1 ])
            simplePattern
            simplePattern
        , Fuzz.constant (CG.namedPattern "True" [])
        , Fuzz.constant (CG.namedPattern "False" [])
        ]


{-| Generate an as-pattern (pattern as name).
-}
asPattern : Fuzzer Pattern
asPattern =
    Fuzz.map2 CG.asPattern
        simplePattern
        identifier


{-| Generate a record pattern.
-}
recordPattern : Fuzzer Pattern
recordPattern =
    Fuzz.listOfLengthBetween 1 4 identifier
        |> Fuzz.map CG.recordPattern


{-| A simple pattern (non-recursive).
-}
simplePattern : Fuzzer Pattern
simplePattern =
    Fuzz.oneOf
        [ varPattern
        , wildcardPattern
        , intPattern
        , stringPattern
        ]


{-| Generate any pattern (limited depth).
-}
pattern : Fuzzer Pattern
pattern =
    Fuzz.oneOf
        [ simplePattern
        , tuplePattern
        , listPattern
        , constructorPattern
        , recordPattern
        ]



-- ============================================================================
-- Type Annotation Fuzzers
-- ============================================================================


{-| Generate a simple type annotation (Int, String, Bool, etc.).
-}
simpleTypeAnn : Fuzzer TypeAnnotation
simpleTypeAnn =
    Fuzz.oneOf
        [ Fuzz.constant CG.intAnn
        , Fuzz.constant CG.floatAnn
        , Fuzz.constant CG.stringAnn
        , Fuzz.constant CG.charAnn
        , Fuzz.constant CG.boolAnn
        , Fuzz.constant CG.unitAnn
        ]


{-| Generate a type variable annotation.
-}
typeVarAnn : Fuzzer TypeAnnotation
typeVarAnn =
    Fuzz.oneOf
        [ Fuzz.constant (CG.typeVar "a")
        , Fuzz.constant (CG.typeVar "b")
        , Fuzz.constant (CG.typeVar "msg")
        , Fuzz.constant (CG.typeVar "model")
        ]


{-| Generate a function type annotation.
-}
functionTypeAnn : Fuzzer TypeAnnotation
functionTypeAnn =
    Fuzz.map2 CG.funAnn
        simpleTypeAnn
        simpleTypeAnn


{-| Generate a tuple type annotation.
-}
tupleTypeAnn : Fuzzer TypeAnnotation
tupleTypeAnn =
    Fuzz.map2
        (\a b -> CG.tupleAnn [ a, b ])
        simpleTypeAnn
        simpleTypeAnn


{-| Generate a record type annotation.
-}
recordTypeAnn : Fuzzer TypeAnnotation
recordTypeAnn =
    Fuzz.listOfLengthBetween 1 4
        (Fuzz.map2 Tuple.pair identifier simpleTypeAnn)
        |> Fuzz.map CG.recordAnn


{-| Generate a parameterized type annotation (e.g., List Int, Maybe String).
-}
parameterizedTypeAnn : Fuzzer TypeAnnotation
parameterizedTypeAnn =
    Fuzz.oneOf
        [ Fuzz.map CG.listAnn simpleTypeAnn
        , Fuzz.map CG.maybeAnn simpleTypeAnn
        , Fuzz.map2 CG.dictAnn simpleTypeAnn simpleTypeAnn
        ]


{-| Generate any type annotation (limited depth).
-}
typeAnnotation : Fuzzer TypeAnnotation
typeAnnotation =
    Fuzz.oneOf
        [ simpleTypeAnn
        , typeVarAnn
        , functionTypeAnn
        , tupleTypeAnn
        , recordTypeAnn
        , parameterizedTypeAnn
        ]



-- ============================================================================
-- Declaration Fuzzers
-- ============================================================================


{-| Generate a value declaration (constant).
-}
valDecl : Fuzzer CG.Declaration
valDecl =
    Fuzz.map3
        (\name sig expr ->
            CG.valDecl Nothing (Just sig) name expr
        )
        identifier
        simpleTypeAnn
        literalExpr


{-| Generate a function declaration.
-}
funDecl : Fuzzer CG.Declaration
funDecl =
    Fuzz.map4
        (\name argName retType body ->
            CG.funDecl Nothing
                (Just (CG.funAnn CG.intAnn retType))
                name
                [ CG.varPattern argName ]
                body
        )
        identifier
        identifier
        simpleTypeAnn
        literalExpr


{-| Generate a type alias declaration.
-}
typeAliasDecl : Fuzzer CG.Declaration
typeAliasDecl =
    Fuzz.map2
        (\name ann ->
            CG.aliasDecl Nothing name [] ann
        )
        typeIdentifier
        typeAnnotation


{-| Generate a custom type declaration.
-}
customTypeDecl : Fuzzer CG.Declaration
customTypeDecl =
    Fuzz.map2
        (\name constructors ->
            CG.customTypeDecl Nothing name [] constructors
        )
        typeIdentifier
        (Fuzz.listOfLengthBetween 1 4
            (Fuzz.map2 Tuple.pair
                typeIdentifier
                (Fuzz.listOfLengthBetween 0 2 simpleTypeAnn)
            )
        )
