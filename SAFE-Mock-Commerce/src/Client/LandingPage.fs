module LandingPage

open System

open Elmish
open Fable.React
open Fable.React.Props
open Fable.Remoting.Client
open Shared

let remoteWebStoreApi =
  Remoting.createApi()
  |> Remoting.withRouteBuilder Route.builder
  |> Remoting.buildProxy<MockStoreWebApi>

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
        model, Cmd.OfAsync.perform remoteWebStoreApi.fetchProducts () StoreProductsLoaded

    | StoreProductsLoaded products ->
        { model with Products = products }, Cmd.none

    | Error e ->
        printfn "Error: %s" e.Message
        model, Cmd.none

type ProductProps = { Key: string; Product: StoreProduct }

let inline elmishView name render = FunctionComponent.Of(render, name, equalsButFunctions)

let renderBook (bookElem: Book) =
    tr [] []

let renderHeadphone (headphoneElem: HeadphoneProduct) =
    tr [] []

let renderTelevision (televisionElem: Device) =
    tr [] []

let renderComputer (computerElem: Computer) =
    tr [] []

let renderConsole (consoleElem: GameConsole) =
    tr [] []

let renderProductElement props =
    match props.Product with
    | Book (book, _) -> renderBook book
    | WirelessHeadphones (headphones, _) -> renderHeadphone headphones
    | Television (television, _) -> renderTelevision television
    | Laptop (computer, _) -> renderComputer computer
    | GameConsole (console, _) -> renderConsole console

let productComponent = elmishView "Product" renderProductElement

let viewModel model =
    match model.Products with
    | products when products.Length > 0 ->
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
    | _ ->
        div [] []

let viewProductComponent = elmishView "Home" viewModel