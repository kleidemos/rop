namespace ROP

module private ChoiceOf2 = 
    let try1Of2 = function 
        | Choice1Of2 p -> Some p 
        | _ -> None

    let try2Of2 = function 
        | Choice2Of2 p -> Some p 
        | _ -> None

    let map mapping = function 
        | Choice1Of2 p -> mapping p |> Choice1Of2
        | Choice2Of2 p -> Choice2Of2 p 

    let flatten = function 
        | Choice1Of2 (Choice1Of2 p) -> Choice1Of2 p
        | Choice1Of2 (Choice2Of2 p) -> Choice2Of2 p
        | Choice2Of2 p -> Choice2Of2 p

    let flattenSecond = function
        | Choice1Of2 p -> Choice1Of2 p
        | Choice2Of2 (Choice1Of2 p) -> Choice1Of2 p
        | Choice2Of2 (Choice2Of2 p) -> Choice2Of2 p
        
    let mapSecond mapping = function
        | Choice1Of2 p -> Choice1Of2 p
        | Choice2Of2 p -> mapping p |> Choice2Of2

type Trial<'result, 'warning, 'error> = {
    Result : Choice<'result, 'error list>
    Warnings : 'warning list
}

[<AutoOpen>]
module RopAuto = 
    let (|Pass|Warn|Fail|) (trial : Trial<_,_,_>) = 
        match trial.Result, trial.Warnings with
        | Choice1Of2 p, [] -> Pass p
        | Choice1Of2 p, _ -> Warn (p, trial.Warnings)
        | Choice2Of2 p, _ -> Fail (p, trial.Warnings)

module Trial = 
    let (|Success|Failure|) trial = 
        match trial.Result with
        | Choice1Of2 p -> Success (p, trial.Warnings)
        | Choice2Of2 p -> Failure (p, trial.Warnings)
    
    // Getters
    
    let warnings trial = 
        trial.Warnings

    let result trial = 
        trial.Result

    let success trial = 
        trial
        |> result 
        |> ChoiceOf2.try1Of2

    let errors trial = 
        trial 
        |> result 
        |> ChoiceOf2.try2Of2

    let isSuccess trial = 
        match trial.Result with
        | Choice1Of2 _ -> true
        | _ -> false

    let isFail trial = 
        isSuccess trial |> not

    // Factories

    let create warnings result = {
        Result = result
        Warnings = warnings
    }

    let warns warns success = 
        Choice1Of2 success
        |> create warns 

    let pass success = 
        warns [] success

    /// Alias of `pass`.
    let ok success = 
        pass success 

    let warn message success = 
        warns [message] success

    let failsWithWarnings warnings errors = 
        Choice2Of2 errors
        |> create warnings 

    let fails errors = 
        failsWithWarnings [] errors 

    let fail error = 
        fails [error]

    // Map and bind

    let mapResult mapping trial = {
        Result = mapping trial.Result
        Warnings = trial.Warnings
    }

    let mapResultApart successMapping failureMapping trial = {
        Result = 
            match trial.Result with
            | Choice1Of2 p -> successMapping p |> Choice1Of2
            | Choice2Of2 p -> failureMapping p |> Choice2Of2
        Warnings = trial.Warnings
    }

    let map mapping trial = 
        mapResult (ChoiceOf2.map mapping) trial

    /// Alias of map.
    let mapSuccess = map

    let mapErrors mapping trial = 
        mapResult (ChoiceOf2.mapSecond mapping) trial

    let mapWarnings mapping trial = {
        Result = trial.Result
        Warnings = trial.Warnings |> mapping
    }

    let flatten trial = 
        match trial.Result with
        | Choice1Of2 p -> 
            create (p.Warnings @ trial.Warnings) p.Result
        | Choice2Of2 p -> 
            create trial.Warnings <| Choice2Of2 p
        
    let bindResult binding trial = 
        match binding trial.Result with
        | Pass p -> [], Choice1Of2 p
        | Warn (p, warns) -> warns, Choice1Of2 p
        | Fail (errors, warns) -> warns, Choice2Of2 errors 
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
        | Choice1Of2 p -> 
            create trial.Warnings <| Choice1Of2 p
        | Choice2Of2 p ->
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
            | Choice1Of2 s1, Choice1Of2 s2 -> 
                mapping s1 s2 |> Choice1Of2
            | Choice2Of2 f1, Choice2Of2 f2 -> 
                f1 @ f2 |> Choice2Of2
            | Choice2Of2 f, _ -> 
                Choice2Of2 f 
            | _, Choice2Of2 f -> 
                Choice2Of2 f)                
            trial1 trial2

    /// Alias of map2
    let mapSuccess2 = map2
    
    // 

    let either ifSucces ifFauilure trial = 
        match trial.Result with
        | Choice1Of2 p -> ifSucces trial.Warnings p
        | Choice2Of2 p -> ifFauilure trial.Warnings p

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
            | Choice1Of2 _ -> errors
            | Choice2Of2 oldWarnings -> errors @ oldWarnings 
            |> Choice2Of2
        Warnings = trial.Warnings
    }

    let addError error trial = 
        addErrors [error] trial

    let warningsToErrors trial = 
        match trial with
        | Pass _ -> trial
        | Warn (_, warns) -> failsWithWarnings [] warns
        | Fail (errors, warns) -> failsWithWarnings [] (warns @ errors)

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
                warns (newWarnings @ oldWarnings) success
            | Failure (newErrors, newWarnings) -> 
                failsWithWarnings 
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
                | Choice1Of2 success -> 
                    if not <| errors.Used() then
                        successes.AddRange [success]
                | Choice2Of2 ers -> 
                    errors.AddRange ers
                trial.Warnings |> warnings.AddRange
            if errors.Used() then 
                errors.Seq
                |> List.ofSeq
                |> Choice2Of2
            else 
                successes.Seq
                |> List.ofSeq
                |> Choice1Of2
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

