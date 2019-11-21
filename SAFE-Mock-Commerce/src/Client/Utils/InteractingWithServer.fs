module BackendInteractions

open Elmish
open Fable.Remoting.Client
open Shared

type DelayedResult<'t> =
    | OperationNotStarted
    | OperationInProgress
    | ResultFromServer of 't

type AsyncTransaction<'t> =
    | Begin
    | Completed of 't