// The PlaceOrder workflow from "Domain Modeling Made Functional" by Scott Wlaschin
// (Pragmatic Bookshelf, 2018), copied from https://github.com/swlaschin/DomainModelingMadeFunctional
// (commit 8153616b1dc0d5a0bb9e965cbe14a46b0dd4f3cf, src/OrderTaking).
// Licensed under the Apache License 2.0: https://www.apache.org/licenses/LICENSE-2.0
//
// Changes: the files below are concatenated into one, in compile order. Result.fs is in
// OrderTaking.Result.fs, shared with OrderTaking.Modified.fs. PlaceOrder.Implementation.fs
// was a top-level module, so it is declared as a nested module with its body indented. The code itself
// is unchanged. Not included: PlaceOrder.Dto.fs, PlaceOrder.Api.fs, AssemblyInfo.fs and
// "PlaceOrder.Implementation(without effects).fs".

// ==================================
// Common.SimpleTypes.fs
// ==================================

// We are defining types and submodules, so we can use a namespace
// rather than a module at the top level
namespace OrderTaking.Common

open System

// ===============================
// Simple types and constrained types related to the OrderTaking domain.
//
// E.g. Single case discriminated unions (aka wrappers), enums, etc
// ===============================

/// Constrained to be 50 chars or less, not null
type String50 = private String50 of string

/// An email address
type EmailAddress = private EmailAddress of string

/// A zip code
type ZipCode = private ZipCode of string

/// An Id for Orders. Constrained to be a non-empty string < 10 chars
type OrderId = private OrderId of string

/// An Id for OrderLines. Constrained to be a non-empty string < 10 chars
type OrderLineId = private OrderLineId of string

/// The codes for Widgets start with a "W" and then four digits
type WidgetCode = private WidgetCode of string

/// The codes for Gizmos start with a "G" and then three digits.
type GizmoCode = private GizmoCode of string

/// A ProductCode is either a Widget or a Gizmo
type ProductCode =
    | Widget of WidgetCode
    | Gizmo of GizmoCode

/// Constrained to be a integer between 1 and 1000
type UnitQuantity = private UnitQuantity of int

/// Constrained to be a decimal between 0.05 and 100.00
type KilogramQuantity = private KilogramQuantity of decimal

/// A Quantity is either a Unit or a Kilogram
type OrderQuantity =
    | Unit of UnitQuantity
    | Kilogram of KilogramQuantity

/// Constrained to be a decimal between 0.0 and 1000.00
type Price = private Price of decimal

/// Constrained to be a decimal between 0.0 and 10000.00
type BillingAmount = private BillingAmount of decimal

/// Represents a PDF attachment
type PdfAttachment = {
    Name : string
    Bytes: byte[]
    }


// ===============================
// Reusable constructors and getters for constrained types
// ===============================

/// Useful functions for constrained types
module ConstrainedType =

    /// Create a constrained string using the constructor provided
    /// Return Error if input is null, empty, or length > maxLen
    let createString fieldName ctor maxLen str =
        if String.IsNullOrEmpty(str) then
            let msg = sprintf "%s must not be null or empty" fieldName
            Error msg
        elif str.Length > maxLen then
            let msg = sprintf "%s must not be more than %i chars" fieldName maxLen
            Error msg
        else
            Ok (ctor str)

    /// Create a optional constrained string using the constructor provided
    /// Return None if input is null, empty.
    /// Return error if length > maxLen
    /// Return Some if the input is valid
    let createStringOption fieldName ctor maxLen str =
        if String.IsNullOrEmpty(str) then
            Ok None
        elif str.Length > maxLen then
            let msg = sprintf "%s must not be more than %i chars" fieldName maxLen
            Error msg
        else
            Ok (ctor str |> Some)

    /// Create a constrained integer using the constructor provided
    /// Return Error if input is less than minVal or more than maxVal
    let createInt fieldName ctor minVal maxVal i =
        if i < minVal then
            let msg = sprintf "%s: Must not be less than %i" fieldName minVal
            Error msg
        elif i > maxVal then
            let msg = sprintf "%s: Must not be greater than %i" fieldName maxVal
            Error msg
        else
            Ok (ctor i)

    /// Create a constrained decimal using the constructor provided
    /// Return Error if input is less than minVal or more than maxVal
    let createDecimal fieldName ctor minVal maxVal i =
        if i < minVal then
            let msg = sprintf "%s: Must not be less than %M" fieldName minVal
            Error msg
        elif i > maxVal then
            let msg = sprintf "%s: Must not be greater than %M" fieldName maxVal
            Error msg
        else
            Ok (ctor i)

    /// Create a constrained string using the constructor provided
    /// Return Error if input is null. empty, or does not match the regex pattern
    let createLike fieldName  ctor pattern str =
        if String.IsNullOrEmpty(str) then
            let msg = sprintf "%s: Must not be null or empty" fieldName
            Error msg
        elif System.Text.RegularExpressions.Regex.IsMatch(str,pattern) then
            Ok (ctor str)
        else
            let msg = sprintf "%s: '%s' must match the pattern '%s'" fieldName str pattern
            Error msg


// F# VERSION DIFFERENCE
// Modules with the same name as a non-generic type will cause an error in versions of F# before v4.1 (VS2017)
// so change the module definition to include a [<CompilationRepresentation>] attribute like this:
(*
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String50 =
*)

module String50 =

    /// Return the value inside a String50
    let value (String50 str) = str

    /// Create an String50 from a string
    /// Return Error if input is null, empty, or length > 50
    let create fieldName str =
        ConstrainedType.createString fieldName String50 50 str

    /// Create an String50 from a string
    /// Return None if input is null, empty.
    /// Return error if length > maxLen
    /// Return Some if the input is valid
    let createOption fieldName str =
        ConstrainedType.createStringOption fieldName String50 50 str

module EmailAddress =

    /// Return the string value inside an EmailAddress
    let value (EmailAddress str) = str

    /// Create an EmailAddress from a string
    /// Return Error if input is null, empty, or doesn't have an "@" in it
    let create fieldName str =
        let pattern = ".+@.+" // anything separated by an "@"
        ConstrainedType.createLike fieldName EmailAddress pattern str

module ZipCode =

    /// Return the string value inside a ZipCode
    let value (ZipCode str) = str

    /// Create a ZipCode from a string
    /// Return Error if input is null, empty, or doesn't have 5 digits
    let create fieldName str =
        let pattern = "\d{5}"
        ConstrainedType.createLike fieldName ZipCode pattern str


module OrderId =

    /// Return the string value inside an OrderId
    let value (OrderId str) = str

    /// Create an OrderId from a string
    /// Return Error if input is null, empty, or length > 50
    let create fieldName str =
        ConstrainedType.createString fieldName OrderId 50 str

module OrderLineId =

    /// Return the string value inside an OrderLineId
    let value (OrderLineId str) = str

    /// Create an OrderLineId from a string
    /// Return Error if input is null, empty, or length > 50
    let create fieldName str =
        ConstrainedType.createString fieldName OrderLineId 50 str

module WidgetCode =

    /// Return the string value inside a WidgetCode
    let value (WidgetCode code) = code

    /// Create an WidgetCode from a string
    /// Return Error if input is null. empty, or not matching pattern
    let create fieldName code =
        // The codes for Widgets start with a "W" and then four digits
        let pattern = "W\d{4}"
        ConstrainedType.createLike fieldName WidgetCode pattern code

module GizmoCode =

    /// Return the string value inside a GizmoCode
    let value (GizmoCode code) = code

    /// Create an GizmoCode from a string
    /// Return Error if input is null, empty, or not matching pattern
    let create fieldName code =
        // The codes for Gizmos start with a "G" and then three digits.
        let pattern = "G\d{3}"
        ConstrainedType.createLike fieldName GizmoCode pattern code

module ProductCode =

    /// Return the string value inside a ProductCode
    let value productCode =
        match productCode with
        | Widget (WidgetCode wc) -> wc
        | Gizmo (GizmoCode gc) -> gc

    /// Create an ProductCode from a string
    /// Return Error if input is null, empty, or not matching pattern
    let create fieldName code =
        if String.IsNullOrEmpty(code) then
            let msg = sprintf "%s: Must not be null or empty" fieldName
            Error msg
        else if code.StartsWith("W") then
            WidgetCode.create fieldName code
            |> Result.map Widget
        else if code.StartsWith("G") then
            GizmoCode.create fieldName code
            |> Result.map Gizmo
        else
            let msg = sprintf "%s: Format not recognized '%s'" fieldName code
            Error msg


module UnitQuantity  =

    /// Return the value inside a UnitQuantity
    let value (UnitQuantity v) = v

    /// Create a UnitQuantity from a int
    /// Return Error if input is not an integer between 1 and 1000
    let create fieldName v =
        ConstrainedType.createInt fieldName UnitQuantity 1 1000 v

module KilogramQuantity =

    /// Return the value inside a KilogramQuantity
    let value (KilogramQuantity v) = v

    /// Create a KilogramQuantity from a decimal.
    /// Return Error if input is not a decimal between 0.05 and 100.00
    let create fieldName v =
        ConstrainedType.createDecimal fieldName KilogramQuantity 0.05M 100M v

module OrderQuantity  =

    /// Return the value inside a OrderQuantity
    let value qty =
        match qty with
        | Unit uq ->
            uq |> UnitQuantity.value |> decimal
        | Kilogram kq ->
            kq |> KilogramQuantity.value

    /// Create a OrderQuantity from a productCode and quantity
    let create fieldName productCode quantity  =
        match productCode with
        | Widget _ ->
            UnitQuantity.create fieldName (int quantity) // convert float to int
            |> Result.map OrderQuantity.Unit             // lift to OrderQuantity type
        | Gizmo _ ->
            KilogramQuantity.create fieldName quantity
            |> Result.map OrderQuantity.Kilogram         // lift to OrderQuantity type

module Price =

    /// Return the value inside a Price
    let value (Price v) = v

    /// Create a Price from a decimal.
    /// Return Error if input is not a decimal between 0.0 and 1000.00
    let create v =
        ConstrainedType.createDecimal "Price" Price 0.0M 1000M v

    /// Create a Price from a decimal.
    /// Throw an exception if out of bounds. This should only be used if you know the value is valid.
    let unsafeCreate v =
        create v
        |> function
            | Ok price ->
                price
            | Error err ->
                failwithf "Not expecting Price to be out of bounds: %s" err

    /// Multiply a Price by a decimal qty.
    /// Return Error if new price is out of bounds.
    let multiply qty (Price p) =
        create (qty * p)

module BillingAmount =

    /// Return the value inside a BillingAmount
    let value (BillingAmount v) = v

    /// Create a BillingAmount from a decimal.
    /// Return Error if input is not a decimal between 0.0 and 10000.00
    let create v =
        ConstrainedType.createDecimal "BillingAmount" BillingAmount 0.0M 10000M v

    /// Sum a list of prices to make a billing amount
    /// Return Error if total is out of bounds
    let sumPrices prices =
        let total = prices |> List.map Price.value |> List.sum
        create total

// ==================================
// Common.CompoundTypes.fs
// ==================================

// We are defining types and submodules, so we can use a namespace
// rather than a module at the top level
namespace OrderTaking.Common

open System

// ==================================
// Common compound types used throughout the OrderTaking domain
//
// Includes: customers, addresses, etc.
// Plus common errors.
//
// ==================================


// ==================================
// Customer-related types
// ==================================

type PersonalName = {
    FirstName : String50
    LastName : String50
    }

type CustomerInfo = {
    Name : PersonalName
    EmailAddress : EmailAddress
    }

// ==================================
// Address-related
// ==================================

type Address = {
    AddressLine1 : String50
    AddressLine2 : String50 option
    AddressLine3 : String50 option
    AddressLine4 : String50 option
    City : String50
    ZipCode : ZipCode
    }

// ==================================
// Product-related types
// ==================================

// Note that the definition of a Product is in a different bounded
// context, and in this context, products are only represented by a ProductCode
// (see the SimpleTypes module).



// ==================================
// PlaceOrder.PublicTypes.fs
// ==================================

// We are defining types and submodules, so we can use a namespace
// rather than a module at the top level
namespace OrderTaking.PlaceOrder

open OrderTaking.Common

// ==================================
// This file contains the definitions of PUBLIC types (exposed at the boundary of the bounded context)
// related to the PlaceOrder workflow
// ==================================

// ------------------------------------
// inputs to the workflow

type UnvalidatedCustomerInfo = {
    FirstName : string
    LastName : string
    EmailAddress : string
    }

type UnvalidatedAddress = {
    AddressLine1 : string
    AddressLine2 : string
    AddressLine3 : string
    AddressLine4 : string
    City : string
    ZipCode : string
    }

type UnvalidatedOrderLine =  {
    OrderLineId : string
    ProductCode : string
    Quantity : decimal
    }

type UnvalidatedOrder = {
    OrderId : string
    CustomerInfo : UnvalidatedCustomerInfo
    ShippingAddress : UnvalidatedAddress
    BillingAddress : UnvalidatedAddress
    Lines : UnvalidatedOrderLine list
    }


// ------------------------------------
// outputs from the workflow (success case)

/// Event will be created if the Acknowledgment was successfully posted
type OrderAcknowledgmentSent = {
    OrderId : OrderId
    EmailAddress : EmailAddress
    }


// priced state
type PricedOrderLine = {
    OrderLineId : OrderLineId
    ProductCode : ProductCode
    Quantity : OrderQuantity
    LinePrice : Price
    }

type PricedOrder = {
    OrderId : OrderId
    CustomerInfo : CustomerInfo
    ShippingAddress : Address
    BillingAddress : Address
    AmountToBill : BillingAmount
    Lines : PricedOrderLine list
    }

/// Event to send to shipping context
type OrderPlaced = PricedOrder

/// Event to send to billing context
/// Will only be created if the AmountToBill is not zero
type BillableOrderPlaced = {
    OrderId : OrderId
    BillingAddress: Address
    AmountToBill : BillingAmount
    }

/// The possible events resulting from the PlaceOrder workflow
/// Not all events will occur, depending on the logic of the workflow
type PlaceOrderEvent =
    | OrderPlaced of OrderPlaced
    | BillableOrderPlaced of BillableOrderPlaced
    | AcknowledgmentSent  of OrderAcknowledgmentSent



// ------------------------------------
// error outputs


/// All the things that can go wrong in this workflow
type ValidationError = ValidationError of string

type PricingError = PricingError of string

type ServiceInfo = {
    Name : string
    Endpoint: System.Uri
    }

type RemoteServiceError = {
    Service : ServiceInfo
    Exception : System.Exception
    }

type PlaceOrderError =
    | Validation of ValidationError
    | Pricing of PricingError
    | RemoteService of RemoteServiceError


// ------------------------------------
// the workflow itself

type PlaceOrder =
    UnvalidatedOrder -> AsyncResult<PlaceOrderEvent list,PlaceOrderError>




// ==================================
// PlaceOrder.Implementation.fs
// ==================================

namespace OrderTaking.PlaceOrder

module internal Implementation =

    open OrderTaking.Common

    // ======================================================
    // This file contains the final implementation for the PlaceOrder workflow
    //
    // This represents the code in chapter 10, "Working with Errors"
    //
    // There are two parts:
    // * the first section contains the (type-only) definitions for each step
    // * the second section contains the implementations for each step
    //   and the implementation of the overall workflow
    // ======================================================


    // ======================================================
    // Section 1 : Define each step in the workflow using types
    // ======================================================

    // ---------------------------
    // Validation step
    // ---------------------------

    // Product validation

    type CheckProductCodeExists =
        ProductCode -> bool

    // Address validation
    type AddressValidationError =
        | InvalidFormat
        | AddressNotFound

    type CheckedAddress = CheckedAddress of UnvalidatedAddress

    type CheckAddressExists =
        UnvalidatedAddress -> AsyncResult<CheckedAddress,AddressValidationError>

    // ---------------------------
    // Validated Order
    // ---------------------------

    type ValidatedOrderLine =  {
        OrderLineId : OrderLineId
        ProductCode : ProductCode
        Quantity : OrderQuantity
        }

    type ValidatedOrder = {
        OrderId : OrderId
        CustomerInfo : CustomerInfo
        ShippingAddress : Address
        BillingAddress : Address
        Lines : ValidatedOrderLine list
        }

    type ValidateOrder =
        CheckProductCodeExists  // dependency
         -> CheckAddressExists  // dependency
         -> UnvalidatedOrder    // input
         -> AsyncResult<ValidatedOrder, ValidationError> // output

    // ---------------------------
    // Pricing step
    // ---------------------------

    type GetProductPrice =
        ProductCode -> Price

    // priced state is defined Domain.WorkflowTypes

    type PriceOrder =
        GetProductPrice     // dependency
         -> ValidatedOrder  // input
         -> Result<PricedOrder, PricingError>  // output


    // ---------------------------
    // Send OrderAcknowledgment
    // ---------------------------

    type HtmlString =
        HtmlString of string

    type OrderAcknowledgment = {
        EmailAddress : EmailAddress
        Letter : HtmlString
        }

    type CreateOrderAcknowledgmentLetter =
        PricedOrder -> HtmlString

    /// Send the order acknowledgement to the customer
    /// Note that this does NOT generate an Result-type error (at least not in this workflow)
    /// because on failure we will continue anyway.
    /// On success, we will generate a OrderAcknowledgmentSent event,
    /// but on failure we won't.

    type SendResult = Sent | NotSent

    type SendOrderAcknowledgment =
        OrderAcknowledgment -> SendResult

    type AcknowledgeOrder =
        CreateOrderAcknowledgmentLetter  // dependency
         -> SendOrderAcknowledgment      // dependency
         -> PricedOrder                  // input
         -> OrderAcknowledgmentSent option // output

    // ---------------------------
    // Create events
    // ---------------------------

    type CreateEvents =
        PricedOrder                           // input
         -> OrderAcknowledgmentSent option    // input (event from previous step)
         -> PlaceOrderEvent list              // output


    // ======================================================
    // Section 2 : Implementation
    // ======================================================

    // ---------------------------
    // ValidateOrder step
    // ---------------------------

    let toCustomerInfo (unvalidatedCustomerInfo: UnvalidatedCustomerInfo) =
        result {
            let! firstName =
                unvalidatedCustomerInfo.FirstName
                |> String50.create "FirstName"
                |> Result.mapError ValidationError // convert creation error into ValidationError
            let! lastName =
                unvalidatedCustomerInfo.LastName
                |> String50.create "LastName"
                |> Result.mapError ValidationError // convert creation error into ValidationError
            let! emailAddress =
                unvalidatedCustomerInfo.EmailAddress
                |> EmailAddress.create "EmailAddress"
                |> Result.mapError ValidationError // convert creation error into ValidationError
            let customerInfo = {
                Name = {FirstName=firstName; LastName=lastName}
                EmailAddress = emailAddress
                }
            return customerInfo
        }

    let toAddress (CheckedAddress checkedAddress) =
        result {
            let! addressLine1 =
                checkedAddress.AddressLine1
                |> String50.create "AddressLine1"
                |> Result.mapError ValidationError // convert creation error into ValidationError
            let! addressLine2 =
                checkedAddress.AddressLine2
                |> String50.createOption "AddressLine2"
                |> Result.mapError ValidationError // convert creation error into ValidationError
            let! addressLine3 =
                checkedAddress.AddressLine3
                |> String50.createOption "AddressLine3"
                |> Result.mapError ValidationError // convert creation error into ValidationError
            let! addressLine4 =
                checkedAddress.AddressLine4
                |> String50.createOption "AddressLine4"
                |> Result.mapError ValidationError // convert creation error into ValidationError
            let! city =
                checkedAddress.City
                |> String50.create "City"
                |> Result.mapError ValidationError // convert creation error into ValidationError
            let! zipCode =
                checkedAddress.ZipCode
                |> ZipCode.create "ZipCode"
                |> Result.mapError ValidationError // convert creation error into ValidationError
            let address : Address = {
                AddressLine1 = addressLine1
                AddressLine2 = addressLine2
                AddressLine3 = addressLine3
                AddressLine4 = addressLine4
                City = city
                ZipCode = zipCode
                }
            return address
        }

    /// Call the checkAddressExists and convert the error to a ValidationError
    let toCheckedAddress (checkAddress:CheckAddressExists) address =
        address
        |> checkAddress
        |> AsyncResult.mapError (fun addrError ->
            match addrError with
            | AddressNotFound -> ValidationError "Address not found"
            | InvalidFormat -> ValidationError "Address has bad format"
            )

    let toOrderId orderId =
        orderId
        |> OrderId.create "OrderId"
        |> Result.mapError ValidationError // convert creation error into ValidationError

    /// Helper function for validateOrder
    let toOrderLineId orderId =
        orderId
        |> OrderLineId.create "OrderLineId"
        |> Result.mapError ValidationError // convert creation error into ValidationError

    /// Helper function for validateOrder
    let toProductCode (checkProductCodeExists:CheckProductCodeExists) productCode =

        // create a ProductCode -> Result<ProductCode,...> function
        // suitable for using in a pipeline
        let checkProduct productCode  =
            if checkProductCodeExists productCode then
                Ok productCode
            else
                let msg = sprintf "Invalid: %A" productCode
                Error (ValidationError msg)

        // assemble the pipeline
        productCode
        |> ProductCode.create "ProductCode"
        |> Result.mapError ValidationError // convert creation error into ValidationError
        |> Result.bind checkProduct

    /// Helper function for validateOrder
    let toOrderQuantity productCode quantity =
        OrderQuantity.create "OrderQuantity" productCode quantity
        |> Result.mapError ValidationError // convert creation error into ValidationError

    /// Helper function for validateOrder
    let toValidatedOrderLine checkProductExists (unvalidatedOrderLine:UnvalidatedOrderLine) =
        result {
            let! orderLineId =
                unvalidatedOrderLine.OrderLineId
                |> toOrderLineId
            let! productCode =
                unvalidatedOrderLine.ProductCode
                |> toProductCode checkProductExists
            let! quantity =
                unvalidatedOrderLine.Quantity
                |> toOrderQuantity productCode
            let validatedOrderLine = {
                OrderLineId = orderLineId
                ProductCode = productCode
                Quantity = quantity
                }
            return validatedOrderLine
        }

    let validateOrder : ValidateOrder =
        fun checkProductCodeExists checkAddressExists unvalidatedOrder ->
            asyncResult {
                let! orderId =
                    unvalidatedOrder.OrderId
                    |> toOrderId
                    |> AsyncResult.ofResult
                let! customerInfo =
                    unvalidatedOrder.CustomerInfo
                    |> toCustomerInfo
                    |> AsyncResult.ofResult
                let! checkedShippingAddress =
                    unvalidatedOrder.ShippingAddress
                    |> toCheckedAddress checkAddressExists
                let! shippingAddress =
                    checkedShippingAddress
                    |> toAddress
                    |> AsyncResult.ofResult
                let! checkedBillingAddress =
                    unvalidatedOrder.BillingAddress
                    |> toCheckedAddress checkAddressExists
                let! billingAddress  =
                    checkedBillingAddress
                    |> toAddress
                    |> AsyncResult.ofResult
                let! lines =
                    unvalidatedOrder.Lines
                    |> List.map (toValidatedOrderLine checkProductCodeExists)
                    |> Result.sequence // convert list of Results to a single Result
                    |> AsyncResult.ofResult
                let validatedOrder : ValidatedOrder = {
                    OrderId  = orderId
                    CustomerInfo = customerInfo
                    ShippingAddress = shippingAddress
                    BillingAddress = billingAddress
                    Lines = lines
                }
                return validatedOrder
            }

    // ---------------------------
    // PriceOrder step
    // ---------------------------

    let toPricedOrderLine (getProductPrice:GetProductPrice) (validatedOrderLine:ValidatedOrderLine) =
        result {
            let qty = validatedOrderLine.Quantity |> OrderQuantity.value
            let price = validatedOrderLine.ProductCode |> getProductPrice
            let! linePrice =
                Price.multiply qty price
                |> Result.mapError PricingError // convert to PlaceOrderError
            let pricedLine : PricedOrderLine = {
                OrderLineId = validatedOrderLine.OrderLineId
                ProductCode = validatedOrderLine.ProductCode
                Quantity = validatedOrderLine.Quantity
                LinePrice = linePrice
                }
            return pricedLine
        }


    let priceOrder : PriceOrder =
        fun getProductPrice validatedOrder ->
            result {
                let! lines =
                    validatedOrder.Lines
                    |> List.map (toPricedOrderLine getProductPrice)
                    |> Result.sequence // convert list of Results to a single Result
                let! amountToBill =
                    lines
                    |> List.map (fun line -> line.LinePrice)  // get each line price
                    |> BillingAmount.sumPrices                // add them together as a BillingAmount
                    |> Result.mapError PricingError           // convert to PlaceOrderError
                let pricedOrder : PricedOrder = {
                    OrderId  = validatedOrder.OrderId
                    CustomerInfo = validatedOrder.CustomerInfo
                    ShippingAddress = validatedOrder.ShippingAddress
                    BillingAddress = validatedOrder.BillingAddress
                    Lines = lines
                    AmountToBill = amountToBill
                }
                return pricedOrder
            }


    // ---------------------------
    // AcknowledgeOrder step
    // ---------------------------

    let acknowledgeOrder : AcknowledgeOrder =
        fun createAcknowledgmentLetter sendAcknowledgment pricedOrder ->
            let letter = createAcknowledgmentLetter pricedOrder
            let acknowledgment = {
                EmailAddress = pricedOrder.CustomerInfo.EmailAddress
                Letter = letter
                }

            // if the acknowledgement was successfully sent,
            // return the corresponding event, else return None
            match sendAcknowledgment acknowledgment with
            | Sent ->
                let event = {
                    OrderId = pricedOrder.OrderId
                    EmailAddress = pricedOrder.CustomerInfo.EmailAddress
                    }
                Some event
            | NotSent ->
                None

    // ---------------------------
    // Create events
    // ---------------------------

    let createOrderPlacedEvent (placedOrder:PricedOrder) : OrderPlaced =
        placedOrder

    let createBillingEvent (placedOrder:PricedOrder) : BillableOrderPlaced option =
        let billingAmount = placedOrder.AmountToBill |> BillingAmount.value
        if billingAmount > 0M then
            {
            OrderId = placedOrder.OrderId
            BillingAddress = placedOrder.BillingAddress
            AmountToBill = placedOrder.AmountToBill
            } |> Some
        else
            None

    /// helper to convert an Option into a List
    let listOfOption opt =
        match opt with
        | Some x -> [x]
        | None -> []

    let createEvents : CreateEvents =
        fun pricedOrder acknowledgmentEventOpt ->
            let acknowledgmentEvents =
                acknowledgmentEventOpt
                |> Option.map PlaceOrderEvent.AcknowledgmentSent
                |> listOfOption
            let orderPlacedEvents =
                pricedOrder
                |> createOrderPlacedEvent
                |> PlaceOrderEvent.OrderPlaced
                |> List.singleton
            let billingEvents =
                pricedOrder
                |> createBillingEvent
                |> Option.map PlaceOrderEvent.BillableOrderPlaced
                |> listOfOption

            // return all the events
            [
            yield! acknowledgmentEvents
            yield! orderPlacedEvents
            yield! billingEvents
            ]


    // ---------------------------
    // overall workflow
    // ---------------------------

    let placeOrder
        checkProductExists // dependency
        checkAddressExists // dependency
        getProductPrice    // dependency
        createOrderAcknowledgmentLetter  // dependency
        sendOrderAcknowledgment // dependency
        : PlaceOrder =       // definition of function

        fun unvalidatedOrder ->
            asyncResult {
                let! validatedOrder =
                    validateOrder checkProductExists checkAddressExists unvalidatedOrder
                    |> AsyncResult.mapError PlaceOrderError.Validation
                let! pricedOrder =
                    priceOrder getProductPrice validatedOrder
                    |> AsyncResult.ofResult
                    |> AsyncResult.mapError PlaceOrderError.Pricing
                let acknowledgementOption =
                    acknowledgeOrder createOrderAcknowledgmentLetter sendOrderAcknowledgment pricedOrder
                let events =
                    createEvents pricedOrder acknowledgementOption
                return events
            }
