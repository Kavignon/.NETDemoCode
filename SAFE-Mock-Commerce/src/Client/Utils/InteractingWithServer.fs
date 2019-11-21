module BackendInteractions

type DelayedResult<'t> =
    | OperationNotStarted
    | OperationInProgress
    | ResultFromServer of 't

type AsyncTransaction<'t> =
    | Begin
    | Completed of 't