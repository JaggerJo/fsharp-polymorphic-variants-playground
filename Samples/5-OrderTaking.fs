// Based on the order-taking workflow from "Domain Modeling Made Functional" by Scott Wlaschin
// (Pragmatic Bookshelf, 2018) and its code samples at https://github.com/swlaschin/DomainModelingMadeFunctional,
// which are licensed under the Apache License 2.0 (https://www.apache.org/licenses/LICENSE-2.0).
// Changes: rewritten as a condensed sketch of the workflow (validate, price, acknowledge) that uses
// polymorphic variants for its errors in place of the original error types.

/// In the book every step has its own error type, so the workflow needs a wrapper union and a
/// Result.mapError per step:
///
///     type PlaceOrderError =
///         | Validation of ValidationError
///         | Pricing of PricingError
///         | RemoteService of RemoteServiceError
///
///     validateOrder >> Result.mapError PlaceOrderError.Validation
///
/// With polymorphic variants each step returns the tags it can fail with, and the workflow's error
/// type is inferred as their union: no wrapper, no mapError. Hover over placeOrder to see it.
module Samples.OrderTaking

type OrderLine = { ProductCode: string; Quantity: int }
type Order = { OrderId: string; Email: string; Lines: OrderLine list }
type PricedOrder = { Order: Order; Total: decimal }

let validateOrder (productExists: string -> bool) (order: Order) =
    if order.OrderId = "" then
        Error `MissingOrderId
    elif not (order.Email.Contains "@") then
        Error(`InvalidEmail order.Email)
    else
        match order.Lines |> List.tryFind (fun line -> not (productExists line.ProductCode)) with
        | Some line -> Error(`UnknownProduct line.ProductCode)
        | None -> Ok order

let priceOrder (getPrice: string -> decimal option) (order: Order) =
    match order.Lines |> List.tryFind (fun line -> getPrice line.ProductCode = None) with
    | Some line -> Error(`NoPrice line.ProductCode)
    | None ->
        let total = order.Lines |> List.sumBy (fun line -> (getPrice line.ProductCode).Value * decimal line.Quantity)
        Ok { Order = order; Total = total }

let acknowledgeOrder (sendEmail: string -> string -> bool) (priced: PricedOrder) =
    if sendEmail priced.Order.Email $"Order {priced.Order.OrderId}: {priced.Total}" then
        Ok priced
    else
        Error(`RemoteService "email")

let placeOrder productExists getPrice sendEmail order =
    validateOrder productExists order
    |> Result.bind (priceOrder getPrice)
    |> Result.bind (acknowledgeOrder sendEmail)

/// Handles every error placeOrder can return. Delete a case and the call in run below stops compiling.
let describeError error =
    match error with
    | `MissingOrderId -> "the order has no id"
    | `InvalidEmail email -> $"'{email}' is not an email address"
    | `UnknownProduct code -> $"product {code} does not exist"
    | `NoPrice code -> $"product {code} has no price"
    | `RemoteService service -> $"the {service} service is unavailable"

let run order =
    let productExists code = code <> "X1"
    let getPrice code = if code = "G1" then Some 9.99m else None
    let sendEmail _ _ = true

    match placeOrder productExists getPrice sendEmail order with
    | Ok priced -> $"placed, total {priced.Total}"
    | Error error -> describeError error

let examples =
    [ run { OrderId = "1"; Email = "jo@example.com"; Lines = [ { ProductCode = "G1"; Quantity = 2 } ] }
      run { OrderId = "2"; Email = "jo"; Lines = [] }
      run { OrderId = "3"; Email = "jo@example.com"; Lines = [ { ProductCode = "X1"; Quantity = 1 } ] }
      run { OrderId = "4"; Email = "jo@example.com"; Lines = [ { ProductCode = "W1"; Quantity = 1 } ] } ]
