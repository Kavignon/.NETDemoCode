module LandingPage

open Elmish
open Shared
open Shared.CatalogueDto
open Shared.DtoToDomain

type Model = {
    Products : list<StoreProduct>
}
with
    static member Empty : list<StoreProduct> = []

type Msg =
| LoadStoreProducts
| StoreProductsLoaded of StoreProduct list
| Error of exn

let init () = Model.Empty, Cmd.ofMsg LoadStoreProducts

let update (msg:Msg) model : Model*Cmd<Msg> =
    match msg with
    | LoadStoreProducts ->
        let storeProducts = productsFromDatabase |> hydrateProductSeq |> Seq.toList
        model, Cmd.ofMsg (StoreProductsLoaded storeProducts)

    | StoreProductsLoaded products ->
        { model with Products = products }, Cmd.none

    | Error e ->
        printfn "Error: %s" e.Message
        model, Cmd.none

type ProductProps = { Key: string; Product: StoreProduct }

let productComponent = elmishView "Book" (fun (props: BookProps) ->
    let book = props.Book
    tr [ Key props.Key ] [
        td [] [
            if String.IsNullOrWhiteSpace book.Link then
                yield str book.Title
            else
                yield a [ Href book.Link; Target "_blank" ] [str book.Title ] ]
        td [] [ str book.Authors ]
        td [] [ img [ Src book.ImageLink; Title book.Title ]]
    ]
)

let view = elmishView "Home" (fun (model:Model) ->
    match model.WishList with
    | Some wishList ->
        table [Key "Books"; ClassName "table table-striped table-hover"] [
            thead [] [
                tr [] [
                    th [] [str "Title"]
                    th [] [str "Authors"]
                    th [] [str "Image"]
                ]
            ]
            tbody [] [
                wishList.Books
                    |> List.map (fun book ->
                        elmishView "Book" bookComponent {
                            Key = book.Title + book.Authors
                            Book = book
                        })
                    |> ofList
            ]
        ]
    | _ ->
        div [] []
)