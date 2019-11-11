module ShoppingCart

type PaymentMethod =
    | Visa of cardOwnerName: string * cardNumber: string * expirationDate: DateTime * cardSecurityCode: int * wasCloned: bool
    | Debit of cardOwnerName: string * cardNumber: string * BankName: string * wasCloned: bool

    member x.isCardValid =
        match x with
        | Visa(_, _, exprDate, _, wasCloned) -> DateTime.Now < exprDate && not wasCloned
        | Debit(_, _, _, wasCloned) -> not wasCloned

    member x.cardOwnerName =
        match x with
        | Visa (ownerName, _, _, _, _) -> ownerName
        | Debit (ownerName, _, _, _) -> ownerName

type ShoppingCart = {
    SelectedPaymentMethod: PaymentMethod
    SelectedItems: Map<StoreProduct, int> option
}
with
    member x.isCartEmpty = x.SelectedItems |> Option.toList |> List.isEmpty

    member x.getCartSubtotal =
        match x.SelectedItems with
        | None -> 0.00
        | Some storeProducts ->
            (0.00, storeProducts)
            ||> Map.fold(fun accumulatedSubtotal product qty -> accumulatedSubtotal + (product.ProductPrice * float qty))