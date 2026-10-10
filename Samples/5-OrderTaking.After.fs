/// The same workflow as 5-OrderTaking.Before.fs, with polymorphic variants: each step fails with plain
/// tags, so there are no error types to declare and nothing to wrap. Hover over placeOrder to see the
/// error type the compiler infers.
module Samples.OrderTaking.After

type Order = { OrderId: string; Email: string; ProductCode: string; Quantity: int }
type PricedOrder = { Order: Order; Total: decimal }

let validateOrder productExists order =
    if order.OrderId = "" then Error `MissingOrderId
    elif not (order.Email.Contains "@") then Error(`InvalidEmail order.Email)
    elif not (productExists order.ProductCode) then Error(`UnknownProduct order.ProductCode)
    else Ok order

let priceOrder getPrice order =
    match getPrice order.ProductCode with
    | Some price -> Ok { Order = order; Total = price * decimal order.Quantity }
    | None -> Error(`NoPrice order.ProductCode)

let acknowledgeOrder sendEmail priced =
    if sendEmail priced.Order.Email then Ok priced else Error `EmailServiceDown

// The errors of all three steps combine on their own.
let placeOrder productExists getPrice sendEmail order =
    validateOrder productExists order
    |> Result.bind (priceOrder getPrice)
    |> Result.bind (acknowledgeOrder sendEmail)

// Still checked: leave out a case and calling this with placeOrder's errors no longer compiles.
let describeError error =
    match error with
    | `MissingOrderId -> "the order has no id"
    | `InvalidEmail email -> $"'{email}' is not an email address"
    | `UnknownProduct code -> $"product {code} does not exist"
    | `NoPrice code -> $"product {code} has no price"
    | `EmailServiceDown -> "the email service is down"

let example =
    let order = { OrderId = "1"; Email = "jo"; ProductCode = "G1"; Quantity = 2 }

    match placeOrder (fun _ -> true) (fun _ -> Some 9.99m) (fun _ -> true) order with
    | Ok priced -> $"placed, total {priced.Total}"
    | Error error -> describeError error
