## Version 6.0.0

Upgraded to `the-sett/elm-pretty-printer` version 3.0.0. This tags parts
of the pretty printer document to describe what kind of syntax it is; a
keyword, operator, type and so on. The tags can then be used to apply
fancy styling to the output; colour syntax highlighting being the main
use case.

## Version 5.3.0

Exposing `Elm.CodeGen.infixExpose`, so can expose custom operators from `elm/parser` and `elm/url`.

Exposing `Elm.Pretty.prettyModule` for pretty printing module definitions on
their own.

## Version 5.2.2

Fixed printing of numbers in hex format - like '0x0F'.

## Version 5.2.1

Fixed backslash escaping in `escapeChar` function.

## Version 5.2.0

Added the `binOpChain` function, for chaining any binary operator.
