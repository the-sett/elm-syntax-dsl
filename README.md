**Contacts for Support**
- @rupertlssmith on https://elmlang.slack.com
- @rupert on https://discourse.elm-lang.org

# elm-syntax-dsl

Provides a DSL that makes it simpler to write code in Elm that generates Elm code; simpler
than `stil4m/elm-syntax` on which this DSL is based.

Provides a pretty printer that prints out the abstract syntax tree constructed using
the DSL as Elm source code, in a way that aims to be compatible with `elm-format`.

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

### How to get the results as a String.

To print the `Doc` created by the `pretty` functions, `the-sett/elm-pretty-printer`
is used:

```
import Elm.Pretty
import Pretty

elmAsString =
    Elm.Pretty.pretty someFile
      |> Pretty.pretty 120 -- Fit to a page width of 120 characters
```

### Broken stuff in elm-syntax:

* Not all Elm files I tried seem to parse.
* Multi-line strings do something very weird and get jumbled up in order.
* End-line comments are deleted.

### Unimplemented stuff:

* GLSL syntax.
* Any reformatting of comments.
* Re-formatting the module exposings to match @docs tags in comments.

### Known deviations from `elm-format`:

* Float literals not getting a .0 added at the end.
* Unicode characters.
* Type annotations on functions sometimes split onto next line without being
broken themselves.

## Testing it.

1. Put some .elm files in `test/examples`.
2. Create directories `test/pre` and `test/post`.
3. In `test/` run `npm start`.
4. Diff `/pre` and `/post` to see how well it compares with elm-format.

## Some other thoughts.

This could be used to create a command line tool `elm-pretty`. Needs to address the broken stuff first though. End-line comments is a deal breaker on `elm-syntax` too, so quite a bit of work to achieve this.

Due to the above, this is aimed at use for code generation purposes for the present.

Will always aim to be compatible with the latest `elm-format`, there is no point in introducing a new standard to compete with it. Any deviations from `elm-format`
invariance please create issues or pull requests, which will be gratefully received.
