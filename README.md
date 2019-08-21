# elm-syntax-dsl

Provides a DSL that makes it simpler to write code in Elm that generates Elm code; simpler
than `stil4m/elm-syntax` on which this DSL is based.

`stil4m/elm-syntax` uses a lot of records to capture parameters, and also wraps parts of the
AST in `Node`s that capture source code lines and rows corresponding to parts of the AST.
This DSL inlines most of the records as function parameters. Any parts of the AST needing to
be wrapped in nodes are automatically wrapped in nodes with zeroed out source code position
information. As the purpose of this package is code generation of Elm from scratch, rather
than the analysis or manipulation of existing Elm, it is ok to have the source code
positional information zeroed out.

A pretty printer that aims to be compatible to elm-format will be provided in a future release...
