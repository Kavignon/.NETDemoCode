module CommonReactComponent

open Feliz

let makeProductImage imageSourceLinkPath (width: int) (height: int) =
    Html.img [
        prop.src imageSourceLinkPath
        prop.alt "product image"
        prop.style [
            style.width width
            style.height height
            style.paddingTop 35
        ]
    ]

let makePriceComponent amount =
    Html.div [
        prop.text (sprintf "Price: CDN$ %.2f" amount)
        prop.style [ style.color "blue" ]
    ]