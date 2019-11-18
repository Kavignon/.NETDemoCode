module ProcessCheckoutWorkflow

open Shared
open Shared.ShoppingCart
open Shared.CustomerInfo

// Should be depend on distance...
let shippingFee = 15.00m
let amountToPayForFreeShipping = 50.00m

type OrderSummary = {
    ItemsOrdered: list<StoreProduct>
    StartingSubtotal: decimal
    IsShippingFree: bool
    ShippingFee: decimal option
    Taxes: decimal
}
with
    member x.Total =
        let billTotal = x.StartingSubtotal + x.Taxes
        if  x.IsShippingFree then billTotal
        else
            match x.ShippingFee with
            | Some fee -> fee + billTotal
            | None -> billTotal

let getCartItemTotal (selectedProducts: Map<StoreProduct, int>) =
    ([], selectedProducts)
    ||> Map.fold(fun totalList product count -> (totalList, List.replicate count product) ||> List.append)

let getOrderTotalTaxes selectedProducts =
    (0.00m, selectedProducts)
    ||> Map.fold(fun accTaxes product count ->
        let decimalCount = count |> decimal
        let baseTaxes = 0.15m
        let environmentalTaxes = 0.05m

        match product with
        | Book (b, _) -> accTaxes + (baseTaxes * decimal b.Details.Price * decimalCount)
        | WirelessHeadphones (wh, _) ->
            accTaxes + (baseTaxes * decimal wh.Details.Price * decimalCount) + (environmentalTaxes * decimal wh.Details.Price * decimalCount)
        | _ -> accTaxes
    )

let processCartCheckout cart customerInfo =
    if not cart.SelectedPaymentMethod.IsCardValid  && customerInfo.ContactInfo.CustomerName.Fullname = cart.SelectedPaymentMethod.CardOwnerName then
        printfn "Cannot proceed to checkout with invalid payment method."
    else
        match cart.SelectedItems with
        | None -> printf "Your cart is empty. We can't process it for now."
        | Some items ->
            let itemCount = getCartItemTotal items
            let totalTaxes = getOrderTotalTaxes items
            let subTotal = cart.GetCartSubtotal
            let orderSummary = {
                ItemsOrdered = itemCount
                StartingSubtotal = decimal subTotal
                ShippingFee = Some shippingFee
                Taxes = totalTaxes
                IsShippingFree = decimal subTotal > amountToPayForFreeShipping
            }

            printfn "Order summary: %A" orderSummary
            printfn "Order Total %M" orderSummary.Total