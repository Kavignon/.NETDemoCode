module Client

open Elmish
open Elmish.React
open Feliz
open Feliz.Router
open Fable.Remoting.Client

open Shared
open BackendInteractions
open BackendInteractions.Cmd

module Cmd =
  let fromAsync (operation: Async<'msg>) : Cmd<'msg> =
    let delayedCmd (dispatch: 'msg -> unit) : unit =
      let delayedDispatch = async {
          let! msg = operation
          dispatch msg
      }

      Async.StartImmediate delayedDispatch

    Cmd.ofSub delayedCmd

type Model = {
    Products: DelayedResult<Result<StoreProduct list, string>>
}

type EventMessage =
    | FetchProducts of AsyncTransaction<Result<StoreProduct list, string>>

let init() = { Products = OperationNotStarted }, Cmd.ofMsg (FetchProducts Begin)

let remoteWebStoreApi =
  Remoting.createApi()
  |> Remoting.withRouteBuilder Route.builder
  |> Remoting.buildProxy<MockStoreWebApi>

let loadStoreProducts = async {
    do! Async.Sleep 500
    let! products = remoteWebStoreApi.fetchProducts()
    if products.Length = 0 then
        return FetchProducts (Completed (Error "No products were loaded from the catalogue"))
    else
        return FetchProducts (Completed (Ok products))
}

let update (msg : EventMessage) (currentModel : Model) : Model * Cmd<EventMessage> =
    match msg with
    | FetchProducts Begin ->
      let nextState = { currentModel with Products = OperationInProgress }
      let nextCmd = Cmd.fromAsync loadStoreProducts
      nextState, nextCmd

    | FetchProducts (Completed products) ->
      let nextState = { currentModel with Products = ResultFromServer products }
      nextState, Cmd.none

let renderError (errorMsg: string) =
  Html.h1 [
    prop.style [ style.color.red ]
    prop.text errorMsg
  ]

let div (classes: string list) (children: ReactElement list) =
  Html.div [
    prop.className classes
    prop.children children
  ]

let renderItem (product: StoreProduct) =
  Html.div [
    prop.className "box"
    prop.style [
      style.marginTop 15
      style.marginBottom 15
    ]
    prop.children [
      div [ "columns"; "is-mobile" ] [
        div [ "column"; "is-narrow" ] [
          Html.div [
            prop.className [ "icon" ]
            prop.style [ style.marginLeft 20 ]
            prop.children [
              Html.i [prop.className "fa fa-poll fa-2x"]
              Html.span [
                prop.style [ style.marginLeft 10; style.marginRight 10 ]
                prop.text product.ReviewAverage
              ]
            ]
          ]
        ]

        div [ "column" ] [
          Html.a [
            prop.style [ style.textDecoration.underline ]
            prop.custom("target", "_blank")
            prop.text product.Name
          ]
        ]
      ]
    ]
  ]

let renderItems (items: StoreProduct list) =  Html.fragment [ for item in items -> renderItem item ]

let spinner =
  Html.div [
    prop.style [ style.textAlign.center; style.marginTop 20 ]
    prop.children [
      Html.i [
        prop.className "fa fa-cog fa-spin fa-2x"
      ]
    ]
  ]

let renderStoreCatalogue = function
  | OperationNotStarted -> Html.none
  | OperationInProgress -> spinner
  | ResultFromServer (Error errorMsg) -> renderError errorMsg
  | ResultFromServer (Ok items) -> renderItems items


let render (state: Model) (dispatch: EventMessage -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      Html.h1 [
        prop.className "title"
        prop.text "Mock E-Commerce"
      ]

      renderStoreCatalogue state.Products
    ]
  ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update render
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run