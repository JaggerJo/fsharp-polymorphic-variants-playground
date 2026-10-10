/// Tags need no type declaration: write `Name, optionally with a payload.
/// Hover over the values to see the types the compiler infers.
module Samples.Tags

let red = `Red
let point = `Point(1, 2)
let colours = [ `Red; `Green; `Blue ]

/// Without a wildcard a match accepts exactly the tags it handles: describe `Purple is an error.
let describe colour =
    match colour with
    | `Red -> "red"
    | `Green -> "green"
    | `Rgb(r, g, b) -> $"rgb({r}, {g}, {b})"

/// With a wildcard any other tag is accepted too.
let isRed colour =
    match colour with
    | `Red -> true
    | _ -> false

let examples = [ describe red; describe (`Rgb(1, 2, 3)); string (isRed `Purple) ]
