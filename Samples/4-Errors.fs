/// Uncomment a line to see the error the compiler reports.
module Samples.Errors

// describe handles `Red, `Green and `Rgb only:
// let unknownTag = Tags.describe `Purple

// Primary is exact, so `Purple is not one of its tags:
// let notPrimary: Types.Primary = `Purple

// A tag has one payload type:
// let mixed = [ `Size 1; `Size "large" ]

// Colour is larger than Primary, so it cannot be converted back with an upcast:
// let narrow (colour: Types.Colour) = colour :> Types.Primary
