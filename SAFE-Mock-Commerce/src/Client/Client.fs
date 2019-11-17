module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fable.Remoting.Client
open Fulma
open Thoth.Json

open Shared
open Pages

type WebApplicateModel = {
    Products: StoreProduct list
}

type EventMessage =
    | FetchingProductsFromCatalogue
    | LoadedProductsFromCatalogue of StoreProduct list

let remoteWebStoreApi =
  Remoting.createApi()
  |> Remoting.withRouteBuilder Route.builder
  |> Remoting.buildProxy<MockStoreWebApi>

// defines the initial state and initial command (= side-effect) of the application
let init () =
    let initialModel = { Products = [] }
    initialModel, Cmd.ofMsg FetchingProductsFromCatalogue

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : EventMessage) (currentModel : WebApplicateModel) : WebApplicateModel * Cmd<EventMessage> =
    match currentModel, msg with
    | _, FetchingProductsFromCatalogue ->
        currentModel, Cmd.OfAsync.perform remoteWebStoreApi.fetchProducts () LoadedProductsFromCatalogue
    | _, LoadedProductsFromCatalogue products ->
        { currentModel with Products = products }, Cmd.none

let safeComponents =
    let components =
        span [ ]
           [ a [ Href "https://github.com/SAFE-Stack/SAFE-template" ]
               [ str "SAFE  "
                 str Version.template ]
             str ", "
             a [ Href "https://saturnframework.github.io" ] [ str "Saturn" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io" ] [ str "Elmish" ]
             str ", "
             a [ Href "https://fulma.github.io/Fulma" ] [ str "Fulma" ]
             str ", "
             a [ Href "https://zaid-ajaj.github.io/Fable.Remoting/" ] [ str "Fable.Remoting" ]

           ]

    span [ ]
        [ str "Version "
          strong [ ] [ str Version.app ]
          str " powered by: "
          components ]

let show model =
    match model with
    | m when m.Products.Length > 0 -> sprintf "%A" model.Products
    | _  -> "Loading..."

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let view (model : WebApplicateModel) (dispatch : EventMessage -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "SAFE Template" ] ] ]

          Container.container []
              [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str ("Press buttons to manipulate counter: " + show model) ] ]
                Columns.columns []
                    [
                        Column.column [] [ button "Hit Products Endpoint" (fun _ -> dispatch FetchingProductsFromCatalogue ) ]
                        Column.column [] [ label [] [ str (sprintf "%A" model.Products)] ]
                    ] ]

          Footer.footer [ ]
                [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ safeComponents ] ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run