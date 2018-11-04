namespace ROP

[<RequireQualifiedAccess>]
type TrialResult<'result, 'error> = 
    | Success of 'result
    | Errors of 'error list
    
module TrialResult = 
    let success = function 
        | TrialResult.Success p -> Some p
        | TrialResult.Errors _ -> None

    let errors = function
        | TrialResult.Success _ -> None
        | TrialResult.Errors p -> Some p

    let ofChoice choice = 
        match choice with
        | Choice1Of2 p -> TrialResult.Success p 
        | Choice2Of2 p -> TrialResult.Errors [p]

    let ofResult result = 
        match result with
        | Ok p -> TrialResult.Success p 
        | Error p -> TrialResult.Errors [p]

    let map mapping = function
        | TrialResult.Success p -> mapping p |> TrialResult.Success
        | TrialResult.Errors p -> TrialResult.Errors p

    let flatten = function
        | TrialResult.Success (TrialResult.Success p) -> TrialResult.Success p
        | TrialResult.Success (TrialResult.Errors p) -> TrialResult.Errors p
        | TrialResult.Errors p -> TrialResult.Errors p

    let mapErrors mapping = function 
        | TrialResult.Success p -> TrialResult.Success p  
        | TrialResult.Errors p -> mapping p |> TrialResult.Errors
    
type Trial<'result, 'warning, 'error> = {
    Result : TrialResult<'result, 'error>
    Warnings : 'warning list
}    

module Trial =                         
    let (|Success|Failure|) trial = 
        match trial.Result with
        | TrialResult.Success p -> Success (p, trial.Warnings)
        | TrialResult.Errors p -> Failure (p, trial.Warnings)
        
    let (|Pass|Warn|Fail|) trial = 
        match trial.Result, trial.Warnings with
        | TrialResult.Success p, [] -> Pass p
        | TrialResult.Success p, _ -> Warn (p, trial.Warnings)
        | TrialResult.Errors p, _ -> Fail (p, trial.Warnings)
    
    // Getters
    
    let warnings trial = 
        trial.Warnings

    let result trial = 
        trial.Result

    let success trial = 
        trial
        |> result 
        |> TrialResult.success

    let errors trial = 
        trial 
        |> result 
        |> TrialResult.errors

    let isSuccess trial = 
        trial |> success |> Option.isSome

    let isFail trial = 
        isSuccess trial |> not

    // Factories

    let create warnings result = {
        Result = result
        Warnings = warnings
    }

    let createSuccess warns success = 
        TrialResult.Success success
        |> create warns 

    let pass success = 
        createSuccess [] success

    /// Alias of `pass`.
    let ok success = 
        pass success 

    /// Alias of `createSuccess`.
    let warnAll messages success =
        createSuccess messages success

    let warn message success = 
        createSuccess [message] success

    let createFailure warnings errors = 
        TrialResult.Errors errors
        |> create warnings 

    let failAll errors = 
        createFailure [] errors 

    let fail error = 
        failAll [error]

    // Map and bind

    let mapResult mapping trial = {
        Result = mapping trial.Result
        Warnings = trial.Warnings
    }

    let mapResultApart successMapping failureMapping trial = {
        Result = 
            match trial.Result with
            | TrialResult.Success p -> 
                successMapping p |> TrialResult.Success
            | TrialResult.Errors p -> 
                failureMapping p |> TrialResult.Errors
        Warnings = trial.Warnings
    }

    let map mapping trial = 
        mapResult (TrialResult.map mapping) trial

    /// Alias of map.
    let mapSuccess = map

    let mapErrors mapping trial = 
        mapResult (TrialResult.mapErrors mapping) trial

    let mapWarnings mapping trial = {
        Result = trial.Result
        Warnings = trial.Warnings |> mapping
    }

    let flatten trial = 
        match trial.Result with
        | TrialResult.Success p -> 
            create (p.Warnings @ trial.Warnings) p.Result
        | TrialResult.Errors p -> 
            create trial.Warnings <| TrialResult.Errors p
        
    let bindResult binding trial = 
        match binding trial.Result with
        | Pass p -> [], TrialResult.Success p
        | Warn (p, warns) -> warns, TrialResult.Success p
        | Fail (errors, warns) -> warns, TrialResult.Errors errors 
        ||> fun warns -> create (warns @ trial.Warnings)

    //let bindResultApart if

    let bind binding trial = 
        trial
        |> map binding
        |> flatten

    /// Alias of bind.
    let bindSuccess = bind

    let bindErrors binding trial = 
        match trial.Result with
        | TrialResult.Success p -> 
            create trial.Warnings <| TrialResult.Success p
        | TrialResult.Errors p ->
            binding p 
            |> mapWarnings (fun p -> p @ trial.Warnings)

    // Map2
    
    let mapResult2 mapping trial1 trial2 =
        trial2
        |> bindResult (fun p2 -> 
            trial1 |> mapResult (fun p1 -> mapping p1 p2))
            
    let map2 mapping trial1 trial2 = 
        mapResult2 (fun c1 c2 -> 
            match c1, c2 with
            | TrialResult.Success s1, TrialResult.Success s2 -> 
                mapping s1 s2 |> TrialResult.Success
            | TrialResult.Errors f1, TrialResult.Errors f2 -> 
                f1 @ f2 |> TrialResult.Errors
            | TrialResult.Errors f, _ -> 
                TrialResult.Errors f 
            | _, TrialResult.Errors f -> 
                TrialResult.Errors f)                
            trial1 trial2

    /// Alias of map2
    let mapSuccess2 = map2
    
    // 

    let either ifSucces ifFauilure trial = 
        match trial.Result with
        | TrialResult.Success p -> ifSucces trial.Warnings p
        | TrialResult.Errors p -> ifFauilure trial.Warnings p

    let dropWarnings trial = { 
        Result = trial.Result
        Warnings = [] 
    }

    let dropWarningsWith f trial =
        f trial.Warnings
        dropWarnings trial

    let addWarnings warnings trial = 
        { trial with Warnings = warnings @ trial.Warnings }

    let addWarning warning trial = 
        { trial with Warnings = warning :: trial.Warnings }

    let addErrors errors trial = {
        Result = 
            match trial.Result with
            | TrialResult.Success _ -> errors
            | TrialResult.Errors oldWarnings -> errors @ oldWarnings 
            |> TrialResult.Errors
        Warnings = trial.Warnings
    }

    let addError error trial = 
        addErrors [error] trial

    let warningsToErrors trial = 
        match trial with
        | Pass _ -> trial
        | Warn (_, warns) -> createFailure [] warns
        | Fail (errors, warns) -> createFailure [] (warns @ errors)

    let defaultSuccess ifFailure trial = 
        trial
        |> bindErrors (fun _ -> pass ifFailure)

    let defaultSuccessWith ifFailureThunk trial = 
        trial 
        |> bindErrors (ignore >> ifFailureThunk >> pass)

    let ifFailTryAndFoldWith ifFailureThunk trial = 
        match trial with
        | Success _ -> trial
        | Failure (oldErrors, oldWarnings) -> 
            match ifFailureThunk () with
            | Success (success, newWarnings) -> 
                createSuccess (newWarnings @ oldWarnings) success
            | Failure (newErrors, newWarnings) -> 
                createFailure 
                    (newWarnings @ oldWarnings)
                    (newErrors @ oldErrors)

    let ifFailTryAndFold ifFailure trial =
        ifFailTryAndFoldWith (fun _ -> ifFailure) trial

    let collect trials = 
        trials 
        |> Seq.fold (
            mapSuccess2 (fun state item -> item :: state)
            ) (pass [])
        |> map List.rev

    let collectWhileSuccessed (trials : #seq<_>) = 
        let enumerator = trials.GetEnumerator()
        let mutable folded = pass []
        while enumerator.MoveNext() && isSuccess folded do 
            folded <-
                mapSuccess2 (fun state item -> item :: state)
                    folded 
                    enumerator.Current
        folded
        |> mapSuccess List.rev

    let failIfNone error option = 
        match option with
        | Some p -> pass p 
        | None -> fail error

    // TODO: Порядок аргументов?
    let tryIfNone warn ifNone option =
        match option with
        | Some p -> pass p
        | None -> 
            ifNone () |> addWarning warn

    let ofChoice choice = 
        match choice with
        | Choice1Of2 success -> pass success
        | Choice2Of2 error -> fail error

    let ofResult result = 
        match result with
        | Ok success -> pass success
        | Error error -> fail error

    // TODO: Тащить варнинги?
    let apply f trial = 
        map2 (fun f trial -> f trial) f trial

    let iter (action : _ -> unit) trial = 
        map action trial 
        |> ignore

    let iter2 (action : _ -> _ -> unit) trial1 trial2 = 
        map2 action trial1 trial2 
        |> ignore

    /// Крайне спорное API. 
    module Distincted =
        type Distinction<'a, 'b> = 
            | Distint
            | DistinctBy of ('a -> 'b)

        type Config<'a,'b,'c,'d,'e,'f> = {
            Success : Distinction<'a,'b> option
            Warnings : Distinction<'c,'d> option
            Errors : Distinction<'e,'f> option
        }

        module Config = 
            let empty = {
                Success = None
                Warnings = None
                Errors = None
            }

        type private Collection<'a> = {
            AddRange : 'a list -> unit
            Seq : 'a seq
            Used : unit -> bool
        }

        module private Collection =
            let withoutDistinction () = 
                let items = ResizeArray()
                let mutable used = false
                {   AddRange = 
                        fun added -> 
                            used <- true
                            items.AddRange added
                    Seq = items |> Seq.readonly
                    Used = fun () -> used }

            let ofDistinction distinction =
                let by =
                    match distinction with
                    | Distint -> id
                    | DistinctBy f -> f
                let set = System.Collections.Generic.HashSet<_>()
                let resize = ResizeArray()
                let mutable used = false
                {   AddRange = fun added -> 
                        used <- true
                        added
                        |> Seq.iter (fun item -> 
                            if set.Add (by item) then 
                                resize.Add item)
                    Seq = resize |> Seq.readonly
                    Used = fun () -> used }

            let ofOption option = 
                match option with
                | None -> withoutDistinction ()
                | Some option -> ofDistinction option

        let collect config trials =
            let successes = Collection.ofOption config.Success
            let warnings = Collection.ofOption config.Warnings
            let errors = Collection.ofOption config.Errors
            for trial in trials do 
                match trial.Result with
                | TrialResult.Success success -> 
                    if not <| errors.Used() then
                        successes.AddRange [success]
                | TrialResult.Errors ers -> 
                    errors.AddRange ers
                trial.Warnings |> warnings.AddRange
            if errors.Used() then 
                errors.Seq
                |> List.ofSeq
                |> TrialResult.Errors
            else 
                successes.Seq
                |> List.ofSeq
                |> TrialResult.Success
            |> create (List.ofSeq warnings.Seq)

type Trial<'result, 'warning, 'error> with
    member this.Success = 
        Trial.success this
    member this.Errors = 
        Trial.errors this        
    member this.IsSuccess = 
        Trial.isSuccess this
    member this.IsFail = 
        Trial.isFail this

// Почти чистая копипаста.
[<AutoOpen>]
module TrialBuilder = 
    open System

    type TrialBuilder () = 
        member this.Zero() = Trial.pass ()
        member this.Bind(m, f) = Trial.bind f m
        member this.Return trial = Trial.pass trial
        member this.ReturnFrom trial = trial
        member this.Combine (a, b) = Trial.bind b a
        member this.Delay f = f
        member this.Run f = f ()
        member this.TryWith (body, handler) =
            try body()
            with e -> handler e
        member __.TryFinally (body, compensation) =
            try body()
            finally compensation()
        member this.Using(d : #IDisposable, body) =
            let result = fun () -> body d
            this.TryFinally (result, fun () ->
                match d with
                | null -> ()
                | d -> d.Dispose())
        member this.While (guard, body) =
            if not <| guard () then
                this.Zero()
            else
                body()
                |> Trial.bind (fun () -> this.While(guard, body)) 
        member this.For(s : _ seq, body) =
            this.Using(s.GetEnumerator(), fun enum ->
                this.While(enum.MoveNext,
                    this.Delay(fun () -> body enum.Current)))

    let trial = TrialBuilder()

type AsyncTrial<'success, 'warning, 'error> = 
    Async<Trial<'success, 'warning, 'error>>

module AsyncTrial = 
    let (|T|) (aTrial : AsyncTrial<'success, 'warning, 'error>) = aTrial

    let ofTrialAsync (trial : Trial<Async<'success>,_,_>) = async {
        match trial with
        | Trial.Success (preSuccess, warnings) -> 
            let! success = preSuccess
            return Trial.createSuccess warnings success
        | Trial.Failure (errors, warnings) -> 
            return Trial.createFailure warnings errors
    }

    let bind binding (T trial) : AsyncTrial<_,_,_> = async {
        let! trial = trial            
        let! preResult = 
            Trial.map binding trial
            |> ofTrialAsync
        return 
            preResult
            |> Trial.flatten
    }

    let map mapping trial : AsyncTrial<_,_,_> = 
        bind (mapping >> Trial.pass >> async.Return) trial

    let flatten (trial : AsyncTrial<AsyncTrial<_,_,_>,_,_>) : AsyncTrial<_,_,_> = async {
        let! trial = trial
        let! preResult = trial |> ofTrialAsync
        return Trial.flatten preResult
    }
        

[<AutoOpen>]
module AsyncTrialBuilder = 
    type AsyncTrialBuilder() = 
        member this.Return value : AsyncTrial<'a,'b,'c> = 
            value 
            |> Trial.pass
            |> async.Return
        member this.ReturnFrom (AsyncTrial.T p) : AsyncTrial<'a,'b,'c> = 
            p 
        member this.Zero() : AsyncTrial<unit,'b,'c> = 
            this.Return ()
        member this.Delay generator : AsyncTrial<'a,'b,'c> = 
            async.Delay generator
        member this.Bind (trial, binding : 'd -> _) : AsyncTrial<'a,'b,'c> = 
            AsyncTrial.bind binding trial
        member this.Bind (trial, binding : 'd -> _) : AsyncTrial<'a,'b,'c> = 
            async.Return trial
            |> AsyncTrial.bind binding
        member this.Bind (async', binding : 'd -> _) : AsyncTrial<'a,'b,'c> = 
            async.Bind(async', Trial.pass >> async.Return)
            |> AsyncTrial.bind binding
        member this.Combine (a : AsyncTrial<unit,'b,'c>, b) : AsyncTrial<'a,'b,'c> = 
            AsyncTrial.bind (fun () -> b) a
        member this.TryWith (AsyncTrial.T trial, handler) : AsyncTrial<'a,'b,'c> = 
            async.TryWith(trial, handler)
        member this.TryFinally (AsyncTrial.T trial, compensation) : AsyncTrial<'a,'b,'c> = 
            async.TryFinally(trial, compensation)
        member this.Using (d : #System.IDisposable, body) : AsyncTrial<'a,'b,'c> = 
            async.Using(d, body)
        member this.While(guard, body) : AsyncTrial<unit,'b,'c> = 
            if not <| guard () then 
                this.Zero() 
            else 
                body
                |> AsyncTrial.bind (fun () -> 
                    this.While(guard, body))
        member this.For(s : _ seq, body) : AsyncTrial<unit,'b,'c> = 
            this.Using(s.GetEnumerator(), fun enum -> 
                this.While(
                    enum.MoveNext
                    , this.Delay(fun () -> body enum.Current)
                )
            )
            
    let asyncTrial = AsyncTrialBuilder()
