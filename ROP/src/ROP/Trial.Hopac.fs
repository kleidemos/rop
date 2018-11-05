namespace ROP

open Hopac
open Hopac.Infixes

type JobTrial<'success, 'warning, 'error> = 
    Job<Trial<'success, 'warning, 'error>>

module JobTrial = 
    let (|T|) (trial : JobTrial<'success, 'warning, 'error>) = trial

    let ofTrialJob trialJob : JobTrial<'a,'b,'c> = job {
        match trialJob with
        | Trial.Success (success : Job<_>, warnings) -> 
            let! res = success 
            return Trial.createSuccess warnings res
        | Trial.Failure (errors, warnings) -> 
            return Trial.createFailure warnings errors
    }

    let ofJob job : JobTrial<'a,'b,'c> = 
        Job.map Trial.pass job

    let map mapping (T trial) : JobTrial<'a,'b,'c> = 
        Job.map (Trial.map mapping) trial

    let flatten 
            (trial : JobTrial<JobTrial<_,_,_>,_,_>) 
            : JobTrial<'a,'b,'c> = 
        trial
        |> Job.bind ofTrialJob
        |> Job.map Trial.flatten

    let bind (binding : _ -> JobTrial<_,_,_>) (T trial) : JobTrial<'a,'b,'c> = 
        trial
        |> map binding
        |> flatten


[<AutoOpen>]
module JobTrialBuilder =
    type JobTrialBuilder () = 
        member this.Return value : JobTrial<'a,'b,'c> = 
            value 
            |> Trial.pass
            |> Job.result
        member this.ReturnFrom (JobTrial.T trial) : JobTrial<'a,'b,'c> = 
            trial
        // Надо ли?
        member this.ReturnFrom (Trial.T trial) : JobTrial<'a,'b,'c> = 
            Job.result trial
        //member this.ReturnFrom (job : Job<_>) : JobTrial<'a,'b,'c> = 
        //    job
        //    |> Job.map Trial.pass
        member this.Zero () : JobTrial<unit,'b,'c> = 
            Trial.pass ()
            |> Job.result 
        member this.Delay generator : JobTrial<'a,'b,'c> =
            Job.delay generator
        member this.Bind (trial, binding : 'd -> _) : JobTrial<'a,'b,'c> = 
            JobTrial.bind binding trial
        member this.Bind (trial, binding : 'd -> _) : JobTrial<'a,'b,'c> = 
            Job.result trial
            |> JobTrial.bind binding
        //member this.Bind (job, binding : 'd -> _) : JobTrial<'a,'b,'c> = 
        //    job
        //    |> Job.map Trial.pass 
        //    |> JobTrial.bind binding
        member this.Combine (a, b) : JobTrial<'a,'b,'c> = 
            JobTrial.bind (fun () -> b) a
        member this.TryWith (trial, handler) : JobTrial<'a,'b,'c> =
            job.TryWith(trial, handler)
        member this.TryFinally (trial, compensation) : JobTrial<'a,'b,'c> =
            // Job.TryFinally имеет отличную от Async.TryFinally сигнатуру, как и Delay...
            job.TryFinally((fun () -> trial), compensation)
        member this.Using (d : #System.IDisposable, body) : JobTrial<'a,'b,'c> =
            job.Using(d, body)
        member this.While (guard, body) : JobTrial<unit,'b,'c> =
            if not <| guard () then 
                this.Zero()
            else 
                body
                |> JobTrial.bind (fun () -> 
                    this.While(guard, body)
                )
        member this.For (items : _ seq, body) : JobTrial<unit,'b,'c> =
            this.Using(items.GetEnumerator(), fun enum -> 
                this.While(
                    enum.MoveNext
                    , this.Delay(fun () -> body enum.Current)
                )
            )

    let jobTrial = JobTrialBuilder ()