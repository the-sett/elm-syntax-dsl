# elm-syntax-dsl

Provides a DSL that makes it simpler to write code in Elm that generates Elm code; simpler
than `stil4m/elm-syntax` on which this DSL is based.

## Simpler code generation as a DSL.

`stil4m/elm-syntax` uses a lot of records to capture parameters, and also wraps parts of the
AST in `Node`s that capture source code lines and rows corresponding to parts of the AST.
This DSL inlines most of the records as function parameters.

`stil4m/elm-syntax` is spread across around 17 modules for the complete syntax. This makes
working with it a little awkward because there are a lot of import statements to write. This
DSL condenses everything into a single exposed module, which helps to get started with
code generation more easily.

Any parts of the AST needing to be wrapped in nodes are automatically wrapped in nodes with
zeroed out source code position information. As the purpose of this package is code generation
of Elm from scratch, rather than the analysis or manipulation of existing Elm, it is ok to
have the source code positional information zeroed out.

## Pretty printing stable under `elm-format`.

The pretty printer aims to be fully stable with respect to `elm-format` in the sense that
running `elm-format` on the output should have no effect at all. The advantage of this is
that if generated code moves to being edited by hand, there will not be a large white-space
only diff created when `elm-format` is applied.

Broken stuff:

* Multi-line strings do something very weird (issue is in elm-syntax).
* End-line comments are deleted (as not supported well by `elm-syntax`).

Known deviations from `elm-format`:

* Not removing brackets that are not necessary.
* <| At start instead of end of line.
* If-else statement inside brackets needs an extra space before else.
* No brackets around expressions being consed.
* Else-if is split instead of carrying on at the same indent level.
* If-else not always aligned when nested inside something else.
* Function application args not indended when nested inside operator application.
* | not breaking when record update expressions break.
* List inside brackets not aligning.
