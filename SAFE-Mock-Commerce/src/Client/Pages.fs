module Client.Pages

open Elmish.UrlParser

/// The different pages of the application. If you add a new page, then add an entry here.
[<RequireQualifiedAccess>]
type Page =
    | Home
    | Login
    | Checkout
    | NotFound
    | ShoppingCart
    | ProductDetails
    | CustomerAccount
    | CustomerOrders

let toPath =
    function
    | Page.Home -> "/"
    | Page.Login -> "/login"
    | Page.Checkout -> "/checkout"
    | Page.NotFound -> "/notfound"
    | Page.ShoppingCart -> "/shopping-cart"
    | Page.ProductDetails -> "/product-details"
    | Page.CustomerAccount -> "/customer-account"
    | Page.CustomerOrders -> "/customer-orders"

/// The URL is turned into a Result.
let pageParser : Parser<Page -> Page,_> =
    oneOf
        [ map Page.Home (s "")
          map Page.Login (s "login")
          map Page.NotFound (s "notfound")
          map Page.Checkout (s "checkout")
          map Page.ShoppingCart (s "shopping-cart")
          map Page.ProductDetails (s "product-details")
          map Page.CustomerAccount (s "customer-account")
          map Page.CustomerOrders (s "customer-orders")
        ]
let urlParser location = parsePath pageParser location