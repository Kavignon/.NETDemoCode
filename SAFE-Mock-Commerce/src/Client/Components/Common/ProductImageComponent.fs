module ProductImageComponent

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