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

* Hex pattern.
* Hex literal.
* Lambda arguments.
* Record type annotation.
* Extensible record type annotation.

Broken stuff in elm-syntax:

* Not all Elm files I tried seem to parse.
* Multi-line strings do something very weird and get jumbled up in order.
* End-line comments are deleted.

Known deviations from `elm-format`:

* Not removing brackets that are not necessary.
* <| At start instead of end of line.
* Brackets not inserted around complex expressions that are nested - such as
when being consed, or a lambda passed as an argument. Operator precedence should
make it clear when brackets are needed - expression
* Else-if is split instead of carrying on at the same indent level.
* If-else not always aligned when nested inside something else.
* Function application args not indented when nested inside operator application.
* | not breaking when record update expressions break.
* List inside brackets not aligning.
