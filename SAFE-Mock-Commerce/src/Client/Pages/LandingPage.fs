module LandingPage

open System

open Elmish
open Fable.Remoting.Client


open Shared
open ComponentUtils
open ProductComponent

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

let update remoteApi msg model =
    match msg with
    | LoadStoreProducts ->
        model, Cmd.OfAsync.perform remoteApi.fetchProducts () StoreProductsLoaded

    | StoreProductsLoaded products ->
        { model with Products = products }, Cmd.none

    | Error e ->
        printfn "Error: %s" e.Message
        model, Cmd.none

let viewProductComponent = elmishView "LandingPage" viewProduct