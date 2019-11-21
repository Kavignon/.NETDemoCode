module Client

open Elmish
open Elmish.React
open Feliz
open Fable.Remoting.Client
open Fulma

open Shared
open BackendInteractions
open Pages

type Model = {
    Products: DelayedResult<Result<StoreProduct list, string>>
}

type EventMessage =
    | FetchProducts of AsyncTransaction<Result<StoreProduct list, string>>

let remoteWebStoreApi =
  Remoting.createApi()
  |> Remoting.withRouteBuilder Route.builder
  |> Remoting.buildProxy<MockStoreWebApi>

// defines the initial state and initial command (= side-effect) of the application
let init () =
    let initialModel = { Products = OperationNotStarted }
    initialModel, Cmd.ofMsg (FetchProducts Begin)

let renderError (errorMsg: string) =
  Html.h1 [
    prop.style [ style.color.red ]
    prop.text errorMsg
  ]

let spinner =
  Html.div [
    prop.style [ style.textAlign.center; style.marginTop 20 ]
    prop.children [
      Html.i [
        prop.className "fa fa-cog fa-spin fa-2x"
      ]
    ]
  ]

let render (state: Model) (dispatch: EventMessage -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      Html.h1 [
        prop.className "title"
        prop.text "Mock E-Commerce"
      ]
    ]
  ]

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : EventMessage) (currentModel : Model) : Model * Cmd<EventMessage> =
    match msg with
      | FetchProducts Begin ->
          let startingFetch = { currentModel with Products = OperationInProgress }
          let nextCmd = Cmd.fromAsync (loadStoryItems state.CurrentStories)
          startingFetch, nextCmd


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