namespace ROP.Hopac.Tests

open ROP
open Hopac
open Hopac.Infixes
open Expecto
open Expecto.Flip
open ROP.Tests
open FsCheck

module JobTrial = 

    [<Tests>]
    let tests = 
        testList "JobTrial" [
            testList "ofTrialJob" [
                testProperty "" <| fun trial -> 
                    trial 
                    |> Trial.map Job.result 
                    |> JobTrial.ofTrialJob
                    |> run
                    |> Expect.equal "" trial
            ]

            testList "map" [
                testProperty "" <| fun trial -> 
                    trial 
                    |> Job.result
                    |> JobTrial.map hash
                    |> run
                    |> Expect.equal "" (trial |> Trial.map hash)
            ]

            testList "flatten" [
                testProperty "" <| fun trial -> 
                    trial 
                    |> Trial.map Job.result 
                    |> Job.result
                    |> JobTrial.flatten
                    |> run 
                    |> Expect.equal "" (Trial.flatten trial)
            ]

            testList "bind" [
                testProperty (sprintf "_ -> %s" Literals.success) <| fun trial -> 
                    trial
                    |> Job.result
                    |> JobTrial.bind (
                        hash
                        >> Trial.pass
                        >> Job.result)
                    |> run 
                    |> Expect.equal "" (
                        Trial.map hash trial
                    )
                testProperty (sprintf "_ -> %s" Literals.fail) <| fun trial -> 
                    trial 
                    |> Job.result
                    |> JobTrial.bind (
                        Trial.fail
                        >> Job.result
                    )
                    |> run 
                    |> Expect.equal "" (
                        Trial.bind Trial.fail trial
                    )
            ]
        ]

module JobTrialBuilder = 
    
    [<Tests>]
    let tests = 
        testList "JobTrialBuilder" [
            testList "return" [
                testProperty "" <| fun value -> 
                    jobTrial {
                        return value
                    }
                    |> run 
                    |> Expect.equal "" (Trial.pass value)
            ]
            
            testList "return from" [
                //testProperty "Job" <| fun job -> 
                //    jobTrial {
                //        return! Job.map Trial.pass
                //    }
                //    |> run
                //    |> Expect.equal "" (run job |> Trial.pass)
                
                testProperty "Trial" <| fun (Trial.T trial) -> 
                    jobTrial {
                        return! trial 
                    }
                    |> run 
                    |> Expect.equal "" trial
                
                testProperty "JobTrial" <| fun (Trial.T trial) -> 
                    jobTrial {
                        return! Job.result trial
                    }
                    |> run
                    |> Expect.equal "" trial
            ]
            
            testList "bind" [
                testProperty "JobTrial" <| fun (Trial.T trial) -> 
                    jobTrial {
                        let! p = Job.result trial
                        return hash p
                    }
                    |> run
                    |> Expect.equal "" (trial |> Trial.map hash)
                
                testProperty "Trial" <| fun (Trial.T trial) -> 
                    jobTrial {
                        let! p = trial
                        return hash p
                    }
                    |> run
                    |> Expect.equal "" (trial |> Trial.map hash)
            ]
            
            testList "combine" [
                testProperty "" <| fun value -> 
                    jobTrial {
                        let! f = Job.result (Trial.pass ())
                        if value <> 42 then 
                            do f
                        return value
                    }
                    |> run
                    |> Expect.equal "" (Trial.pass value)
            ]

            testList "try with" [
                testProperty Literals.success <| fun value -> 
                    jobTrial {
                        return 
                            try value
                            with _ -> 42
                    }
                    |> run 
                    |> Expect.equal "" (Trial.pass value)

                testProperty Literals.fail <| fun value -> 
                    jobTrial {
                        return 
                            try failwith "BOO!!!"
                            with _ -> value
                    }
                    |> run 
                    |> Expect.equal "" (Trial.pass value)
            ]

            testList "try finally" [
                testProperty Literals.success <| fun value -> 
                    let ch = Ch()
                    jobTrial {
                        try return value 
                        finally Ch.Now.send ch value 
                    }
                    <&> ch
                    |> run
                    |> Expect.equal "" (Trial.pass value, value)
                
                testProperty Literals.fail <| fun value -> 
                    let ch = Ch()
                    fun () -> 
                        jobTrial {
                            try failwith "BOO!!!" 
                            finally Ch.Now.send ch value 
                        }
                        |> Job.Ignore
                        |> run
                    |> Expect.throws ""
                    Ch.take ch
                    |> run 
                    |> Expect.equal "" value
            ]

            testList "use" [
                testProperty Literals.success <| fun value -> 
                    let ch = Ch()
                    jobTrial {
                        use! s = 
                            { new System.IDisposable with
                                member this.Dispose () = Ch.Now.send ch value }
                            |> Trial.pass
                        return value
                    }
                    <&> ch
                    |> run 
                    |> Expect.equal "" (Trial.pass value, value)

                testProperty Literals.fail <| fun value -> 
                    let ch = Ch()
                    fun () -> 
                        jobTrial {
                            use! s =
                                { new System.IDisposable with
                                    member this.Dispose () = Ch.Now.send ch value }
                                |> Trial.pass
                            return failwith "BOO!!!"
                        }
                        |> Job.Ignore
                        |> run
                    |> Expect.throws "" 
                    Ch.take ch
                    |> run 
                    |> Expect.equal "" value
            ]

            testList "while" [
                testProperty "" <| fun (NonNegativeInt value) ->                     
                    jobTrial {
                        let mutable counter = 0
                        while counter < value do
                            counter <- counter + 1 
                        return counter                    
                    }
                    |> run
                    |> Expect.equal "" (Trial.pass value)
            ]
            testList "for" [
                testProperty "" <| fun items -> 
                    asyncTrial {
                        let mutable sum = 0
                        for item in items do sum <- sum + item
                        return sum
                    }
                    |> Async.RunSynchronously
                    |> Expect.equal "" (Trial.pass (items |> List.sum))
            ]
        ]