module BackendInteractions

open Elmish
open Fable.Remoting.Client
open Shared

let remoteWebStoreApi =
  Remoting.createApi()
  |> Remoting.withRouteBuilder Route.builder
  |> Remoting.buildProxy<MockStoreWebApi>

type DelayedResult<'t> =
    | OperationNotStarted
    | OperationInProgress
    | ResultFromServer of 't

type AsyncTransaction<'t> =
    | Begin
    | Completed of 't

module Cmd =
  let fromAsync (operation: Async<'msg>) : Cmd<'msg> =
    let delayedCmd (dispatch: 'msg -> unit) : unit =
      let delayedDispatch = async {
          let! msg = operation
          dispatch msg
      }

      Async.StartImmediate delayedDispatch

    Cmd.ofSub delayedCmd