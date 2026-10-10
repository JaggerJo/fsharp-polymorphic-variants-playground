/// Error handling in F# today, in the style of the order-taking workflow from
/// "Domain Modeling Made Functional" by Scott Wlaschin. Compare with 5-OrderTaking.After.fs.
module Samples.OrderTaking.Before

type Order = { OrderId: string; Email: string; ProductCode: string; Quantity: int }
type PricedOrder = { Order: Order; Total: decimal }

// One error type per step...
type ValidationError =
    | MissingOrderId
    | InvalidEmail of string
    | UnknownProduct of string

type PricingError = NoPrice of string

type AcknowledgmentError = EmailServiceDown

// ...and one more to combine them.
type PlaceOrderError =
    | Validation of ValidationError
    | Pricing of PricingError
    | Acknowledgment of AcknowledgmentError

let validateOrder productExists order : Result<Order, ValidationError> =
    if order.OrderId = "" then Error MissingOrderId
    elif not (order.Email.Contains "@") then Error(InvalidEmail order.Email)
    elif not (productExists order.ProductCode) then Error(UnknownProduct order.ProductCode)
    else Ok order

let priceOrder getPrice order : Result<PricedOrder, PricingError> =
    match getPrice order.ProductCode with
    | Some price -> Ok { Order = order; Total = price * decimal order.Quantity }
    | None -> Error(NoPrice order.ProductCode)

let acknowledgeOrder sendEmail priced : Result<PricedOrder, AcknowledgmentError> =
    if sendEmail priced.Order.Email then Ok priced else Error EmailServiceDown

// Every step's error is wrapped into PlaceOrderError.
let placeOrder productExists getPrice sendEmail order =
    validateOrder productExists order
    |> Result.mapError Validation
    |> Result.bind (priceOrder getPrice >> Result.mapError Pricing)
    |> Result.bind (acknowledgeOrder sendEmail >> Result.mapError Acknowledgment)

let describeError error =
    match error with
    | Validation MissingOrderId -> "the order has no id"
    | Validation(InvalidEmail email) -> $"'{email}' is not an email address"
    | Validation(UnknownProduct code) -> $"product {code} does not exist"
    | Pricing(NoPrice code) -> $"product {code} has no price"
    | Acknowledgment EmailServiceDown -> "the email service is down"
