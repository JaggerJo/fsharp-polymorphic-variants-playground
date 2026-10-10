/// Variant types can be named, combined and annotated.
module Samples.Types

/// An exact type: these three tags and no others.
type Primary = [ `Red | `Green | `Blue ]

/// Includes the tags of Primary.
type Colour = [ Primary | `Rgb of int * int * int ]

let primaryToRgb (primary: Primary) =
    match primary with
    | `Red -> (255, 0, 0)
    | `Green -> (0, 255, 0)
    | `Blue -> (0, 0, 255)

/// #Primary matches any tag of Primary and binds it at that type.
let toRgb (colour: Colour) =
    match colour with
    | #Primary as primary -> primaryToRgb primary
    | `Rgb(r, g, b) -> (r, g, b)

/// An exact type converts to a larger one with an upcast.
let widen (primary: Primary) = primary :> Colour

/// [> ...] means "at least these tags"; [< ...] means "at most these tags".
let atLeastRed (colour: [> `Red ]) = colour
let atMostPrimary (colour: [< `Red | `Green | `Blue ]) =
    match colour with
    | `Red -> "warm"
    | _ -> "cool"

let examples = [ toRgb `Blue; toRgb (widen `Green); toRgb (`Rgb(1, 2, 3)) ]
