module ProductComponent

open Fable.React
open Fable.React.Props

open Shared
open ComponentUtils

type ProductProps = { Key: string; Product: StoreProduct }

let renderBook (bookElem: Book) =
    tr [] []

let renderHeadphone (headphoneElem: HeadphoneProduct) =
    tr [] []

let renderProductElement props =
    match props.Product with
    | Book (book, _) -> renderBook book
    | WirelessHeadphones (headphones, _) -> renderHeadphone headphones

let productComponent = elmishView "Product" renderProductElement

let viewProduct (products: StoreProduct list) =
    if products.Length = 0 then
        div [] []
    else
        table [Key "Products"; ClassName "table table-striped table-hover"] [
            thead [] [
                tr [] [
                    th [] [str "Image"]
                    th [] [str "Title"]
                    th [] [str "Review Average"]
                    th [] [str "Category"]
                    th [] [str "Price"]
                ]
            ]
            tbody [] [
                products
                |> List.map (fun product ->
                    elmishView "Book" productComponent {
                        Key = product.ProductId
                        Product = product
                    })
                |> ofList
            ]
        ]