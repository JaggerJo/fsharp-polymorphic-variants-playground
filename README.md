# F# polymorphic variants playground

[![Open in GitHub Codespaces](https://github.com/codespaces/badge.svg)](https://codespaces.new/JaggerJo/fsharp-polymorphic-variants-playground)

Try an experimental F# compiler with OCaml-style polymorphic variants: tags that need no type declaration.

```fsharp
let describe colour =
    match colour with
    | `Red -> "red"
    | `Rgb (r, g, b) -> $"rgb({r}, {g}, {b})"
// val describe: colour: [< `Red | `Rgb of 'a * 'b * 'c ] -> string

describe `Red           // "red"
describe `Purple        // error: this variant type allows at most `Red | `Rgb
```

This is a research prototype of the compiler from [JaggerJo/fsharp@poly-variants](https://github.com/JaggerJo/fsharp/tree/poly-variants), not a proposal of record for F#.

## Open it

- **In the browser:** click the Codespaces badge above. VS Code opens with everything set up.
- **Locally:** install Docker and the VS Code *Dev Containers* extension, clone this repository, open it in VS Code and choose **Reopen in Container**.

The container ([ghcr.io/jaggerjo/fsharp-poly-variants-demo](https://github.com/JaggerJo/fsharp/pkgs/container/fsharp-poly-variants-demo)) brings:

- the .NET 11 preview SDK, with the patched compiler used by every `dotnet build`, `dotnet run` and `dotnet test`;
- Ionide (the F# extension) backed by a language server built against the patched compiler, so errors, hovers and completion understand variants;
- `fsi-pv`, an F# Interactive with the feature switched on.

## Look around

Hover over values in [Samples](Samples) to see the types the compiler infers:

| File | Shows |
|---|---|
| [1-Tags.fs](Samples/1-Tags.fs) | tags, payloads, and how a match decides which tags a function accepts |
| [2-Types.fs](Samples/2-Types.fs) | named variant types, `#Type` patterns, upcasts, `[> ...]` and `[< ...]` annotations |
| [3-Recursive.fs](Samples/3-Recursive.fs) | recursive variant types: an expression evaluator without a type declaration |
| [4-Errors.fs](Samples/4-Errors.fs) | lines to uncomment, to see the errors |

`dotnet build` also writes every inferred signature to `obj/Inferred.fsi`.

To use the feature in a project of your own, set `<LangVersion>preview</LangVersion>` in its project file.

## Reading the types

| Type | Means |
|---|---|
| `` [ `A | `B ] `` | exactly the tags `` `A `` and `` `B `` |
| `` [> `A ] `` | at least `` `A ``: any value that may also be another tag |
| `` [< `A | `B ] `` | at most `` `A `` and `` `B ``: what a match without a wildcard accepts |
| `` [< `A | `B > `A ] `` | at most `` `A `` and `` `B ``, and certainly `` `A `` |
| `` ([ `Leaf | `Node of 'e * 'e ] as 'e) `` | a recursive type: `'e` stands for the type itself |

In signature files and in error messages, an open type is written as a constraint on a type parameter, as in `'a when 'a: [> `A ]`.

## Known limitations

- Hovering a tag itself shows nothing; rename, find references and semantic highlighting don't work on tags.
- Formatting with Fantomas fails on files that contain tags, because Fantomas has its own parser.
- At run time a variant is a `string * obj` pair, so `%A` prints it as a tuple.
- Rows can refer to themselves, but two rows can't refer to each other.
