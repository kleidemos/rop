namespace ROP.Tests

open ROP
open Expecto
open Expecto.Flip

[<AutoOpen>]
module Utils = 
    let shouldNotCall () = 
        failtest "Should not call."

    let shouldNotCallT p = 
        failtest "Should not call."

    module Choice2 = 
        let shouldConvert1Of2 mapping = function 
            | Choice1Of2 p -> mapping p
            | _ -> shouldNotCall ()

        let shouldConvert2Of2 mapping = function 
            | Choice2Of2 p -> mapping p 
            | _ -> shouldNotCall ()

    module List = 
        let prepend list1 list2 = 
            list2 @ list1

    type NondetBuilder () =
        member this.Return x = 
            [x]

        member this.Bind (x, f) = 
            List.collect f x

        member this.Zero () = []

    let nondet = NondetBuilder()

module Literals = 
    let pow = "pass or warn"
    let fail = "fail"
    let success = "success"
module L = Literals

module Trial =
    open FsCheck
    
    [<Tests>]
    let tests = 
        testList "module Trial" [
            testList "map result" [
                let test sourceName sourceFactory sourceWay = 
                    let test name destinatioFactory destinationWay = 
                        testProperty name <| fun p warnings -> 
                            sourceFactory warnings p
                            |> Trial.mapResult (
                                sourceWay string >> List.singleton >> destinationWay)
                            |> Expect.equal "" (destinatioFactory warnings [string p])
                    testList (sprintf "%s -> " sourceName) [
                        yield test
                            L.pow
                            Trial.warns
                            Choice1Of2
                        yield test
                            L.fail
                            Trial.failsWithWarnings
                            Choice2Of2                        
                    ]

                yield test L.pow Trial.warns Choice2.shouldConvert1Of2
                yield test L.fail Trial.failsWithWarnings Choice2.shouldConvert2Of2
            ]

            testList "map result apart" [
                testProperty L.pow <| fun success warnings -> 
                    Trial.warns warnings success
                    |> Trial.mapResultApart 
                        hash
                        shouldNotCallT
                    |> Expect.equal "" (
                        hash success
                        |> Trial.warns warnings)

                testProperty L.fail <| fun errors warnings -> 
                    Trial.failsWithWarnings warnings errors 
                    |> Trial.mapResultApart
                        shouldNotCallT
                        (List.map hash)
                    |> Expect.equal "" (
                        errors
                        |> List.map hash 
                        |> Trial.failsWithWarnings warnings)
            ]

            testList "map (success)" [
                testProperty L.pow <| fun success warnings -> 
                    Trial.warns warnings success
                    |> Trial.map (string >> List.singleton)
                    |> Expect.equal "" (Trial.warns warnings [string success])

                testProperty L.fail <| fun errors warnings -> 
                    Trial.failsWithWarnings warnings errors 
                    |> Trial.map (string >> List.singleton)
                    |> Expect.equal "" (Trial.failsWithWarnings warnings errors)
            ]

            testList "map errors" [
                testProperty L.pow <| fun success warnings -> 
                    Trial.warns warnings success 
                    |> Trial.mapErrors (List.map string)
                    |> Expect.equal "" (Trial.warns warnings success)

                testProperty L.fail <| fun errors warnings -> 
                    Trial.failsWithWarnings warnings errors 
                    |> Trial.mapErrors (List.map string)
                    |> Expect.equal "" (
                        errors
                        |> List.map string
                        |> Trial.failsWithWarnings warnings)
            ]

            testProperty "map warnings" <| fun result warnings -> 
                Trial.create warnings result
                |> Trial.mapWarnings (List.map string)
                |> Expect.equal "" (
                    Trial.create (warnings |> List.map string) result)

            testList "flatten" [
                testProperty "pass or warn in pass or warn" <| fun success innerWarnings outerWarnings -> 
                    Trial.warns innerWarnings success
                    |> Trial.warns outerWarnings
                    |> Trial.flatten
                    |> Expect.equal "" (
                        Trial.warns (innerWarnings @ outerWarnings) success)

                testProperty "fail in pass or warn" <| fun errors innerWarnings outerWarnings -> 
                    Trial.failsWithWarnings innerWarnings errors 
                    |> Trial.warns outerWarnings
                    |> Trial.flatten
                    |> Expect.equal "" (
                        Trial.failsWithWarnings (innerWarnings @ outerWarnings) errors)

                testProperty L.fail <| fun errors warnings -> 
                    Trial.failsWithWarnings warnings errors 
                    |> Trial.flatten
                    |> Expect.equal "" (
                        Trial.failsWithWarnings warnings errors)
            ]                

            // Написано одним из первых, есть резон упростить.
            testList "bind result" [
                let testTo name factory = 
                    testList name [   
                        testProperty "pass" <| factory Trial.pass Trial.warns
                        testProperty "warn" <| fun newWarns -> 
                            factory (Trial.warns newWarns) (
                                List.append newWarns
                                >> Trial.warns)
                        testProperty L.fail <| fun newWarns -> 
                            factory (Trial.failsWithWarnings newWarns) (
                                List.append newWarns
                                >> Trial.failsWithWarnings) 
                    ]

                yield testTo "pass to" <| fun mapping _ success -> 
                    Trial.pass success
                    |> Trial.bindResult (
                        string 
                        >> List.singleton
                        >> mapping
                        |> Choice2.shouldConvert1Of2)
                    |> Expect.equal "" ([string success] |> mapping)

                yield testTo "warn to" <| fun mapping toResult success warns -> 
                    Trial.warns warns success
                    |> Trial.bindResult (
                        string 
                        >> List.singleton
                        >> mapping
                        |> Choice2.shouldConvert1Of2)
                    |> Expect.equal "" ([string success] |> toResult warns)

                yield testTo "fail to" <| fun mapping toResult errors warns -> 
                    Trial.failsWithWarnings warns errors 
                    |> Trial.bindResult (
                        string 
                        >> List.singleton
                        >> mapping
                        |> Choice2.shouldConvert2Of2)
                    |> Expect.equal "" ([string errors] |> toResult warns)
            ]

            testList "bind (success)" [
                let test name sourceFactory destinationFactory = 
                    let test name binding = 
                        testProperty name <| fun preResult oldWarnings newWarnings -> 
                            sourceFactory oldWarnings preResult 
                            |> Trial.bind (
                                string >> List.singleton >> binding newWarnings)
                            |> Expect.equal "" (
                                destinationFactory binding newWarnings oldWarnings preResult)
                    testList name [
                        test L.pow Trial.warns
                        test L.fail Trial.failsWithWarnings
                    ]
                yield test L.pow Trial.warns (fun binding new' old -> 
                    string 
                    >> List.singleton
                    >> binding (new' @ old))
                yield test L.fail Trial.failsWithWarnings (fun _ _ -> 
                    Trial.failsWithWarnings)
            ]

            testList "bind errors" [
                testProperty L.pow <| fun success oldWarnings newWarnings -> 
                    Trial.warns oldWarnings success
                    |> Trial.bindErrors (List.map string >> Trial.failsWithWarnings newWarnings)
                    |> Expect.equal "" (
                        Trial.warns oldWarnings success)

                testList (sprintf "%s -> " L.fail) [
                    testProperty L.fail <| fun errors oldWarnings newWarnings -> 
                        Trial.failsWithWarnings oldWarnings errors 
                        |> Trial.bindErrors (List.map string >> Trial.failsWithWarnings newWarnings)
                        |> Expect.equal "" (
                            errors 
                            |> List.map string 
                            |> Trial.failsWithWarnings (newWarnings @ oldWarnings))

                    testProperty L.pow <| fun errors oldWarnings newWarnings -> 
                        Trial.failsWithWarnings oldWarnings errors 
                        |> Trial.bindErrors (List.map string >> Trial.warns newWarnings)
                        |> Expect.equal "" (
                            errors
                            |> List.map string 
                            |> Trial.warns (newWarnings @ oldWarnings))
                ]
            ]

            testList "map result 2" <| nondet { 
                let source = [   
                    L.pow, Trial.warns, Choice2.shouldConvert1Of2
                    L.fail, Trial.failsWithWarnings, Choice2.shouldConvert2Of2 ]
                let! name1, factory1, way1 = source
                let! name2, factory2, way2 = source
                let! name3, factory3, way3 = [
                    L.pow, Trial.warns, Choice1Of2
                    L.fail, Trial.failsWithWarnings, Choice2Of2 ]
                return testProperty (sprintf "%s + %s -> %s" name1 name2 name3) <| fun r1 w1 r2 w2 -> 
                    (factory1 w1 r1, factory2 w2 r2)
                    ||> Trial.mapResult2 (
                        way1 (fun p -> 
                            way2 (
                                string 
                                >> List.singleton
                                >> List.append [string p]
                                >> way3))
                    )
                    |> Expect.equal "" (
                        factory3 (w1 @ w2) [string r1; string r2])
            }

            // Слишком сложно, лучше в лоб.
            testList "map 2 (success)" [
                yield! nondet {
                    let source = [
                        L.pow, Trial.warns, true
                        L.fail, Trial.failsWithWarnings, false
                    ]
                    let! name1, factory1, t1 = source
                    let! name2, factory2, t2 = source
                    return testProperty (sprintf "%s + %s" name1 name2) <| fun r1 w1 r2 w2 -> 
                        (factory1 w1 r1, factory2 w2 r2)
                        ||> Trial.map2 (fun a b -> 
                            [string a; string b])
                        |> Expect.equal "" (
                            if t1 && t2 
                                then
                                    [r1;r2]
                                    |> List.map string
                                    |> Trial.warns (w1 @ w2) 
                                else 
                                    [t1, r1; t2, r2] 
                                    |> List.filter (fst >> not)
                                    |> List.collect snd
                                    |> Trial.failsWithWarnings (w1 @ w2))
                }
            ]

            testList "either" [
                testProperty L.pow <| fun success warnings -> 
                    Trial.warns warnings success
                    |> Trial.either (sprintf "%A %A") (fun _ _ -> shouldNotCall())
                    |> Expect.equal "" (sprintf "%A %A" warnings success)

                testProperty L.fail <| fun errors warnings -> 
                    Trial.failsWithWarnings warnings errors 
                    |> Trial.either (fun _ _ -> shouldNotCall ()) (sprintf "%A %A")
                    |> Expect.equal "" (sprintf "%A %A" warnings errors)
            ]

            testList "drop warnings" [
                testProperty "can change 'warning type" <| fun result warnings -> 
                    let emptyStringList : string list = []
                    Trial.create warnings result
                    |> Trial.dropWarnings 
                    |> Expect.equal "" (
                        Trial.create emptyStringList result)
            ]

            // drop warnings with ?

            testProperty "add warnings" <| fun result oldWarnings newWarnings -> 
                Trial.create oldWarnings result
                |> Trial.addWarnings newWarnings
                |> Expect.equal "" (
                    Trial.create (newWarnings @ oldWarnings) result)

            testProperty "add warning" <| fun result oldWarnings newWarning -> 
                Trial.create oldWarnings result
                |> Trial.addWarning newWarning
                |> Expect.equal "" (
                    Trial.create (newWarning :: oldWarnings) result)

            testList "add errors" [
                testProperty L.pow <| fun success warnings errors -> 
                    Trial.warns warnings success 
                    |> Trial.addErrors errors 
                    |> Expect.equal "" (
                        Trial.failsWithWarnings warnings errors)

                testProperty L.fail <| fun oldErrors warnings newErrors -> 
                    Trial.failsWithWarnings warnings oldErrors
                    |> Trial.addErrors newErrors
                    |> Expect.equal "" (
                        newErrors @ oldErrors
                        |> Trial.failsWithWarnings warnings )
            ]

            // add error ?

            testList "warnings to errors" [
                testProperty "pass" <| fun success -> 
                    Trial.pass success
                    |> Trial.warningsToErrors
                    |> Expect.equal "" (Trial.pass success)

                testProperty "warn" <| fun success (NonEmptyArray preWarnings) -> 
                    Trial.warns (List.ofArray preWarnings) success
                    |> Trial.warningsToErrors 
                    |> Expect.equal "" (
                        preWarnings
                        |> List.ofArray
                        |> Trial.failsWithWarnings [])

                testProperty L.fail <| fun errors warnings -> 
                    Trial.failsWithWarnings warnings errors 
                    |> Trial.warningsToErrors 
                    |>  Expect.equal "" (
                        warnings @ errors
                        |> Trial.failsWithWarnings [])
            ]

            testList "default success" [
                testProperty L.pow <| fun success warnings defaultSuccess -> 
                    Trial.warns warnings success 
                    |> Trial.defaultSuccess defaultSuccess
                    |> Expect.equal "" (Trial.warns warnings success)

                testProperty L.fail <| fun errors warnings defaultSuccess -> 
                    Trial.failsWithWarnings warnings errors 
                    |> Trial.defaultSuccess defaultSuccess
                    |> Expect.equal "" (Trial.warns warnings defaultSuccess)
            ]

            testList "default success with" [
                testProperty L.pow <| fun success warnings defaultSuccess -> 
                    Trial.warns warnings success 
                    |> Trial.defaultSuccessWith (fun _ -> defaultSuccess)
                    |> Expect.equal "" (Trial.warns warnings success)

                testProperty L.fail <| fun errors warnings defaultSuccess -> 
                    Trial.failsWithWarnings warnings errors 
                    |> Trial.defaultSuccessWith (fun _ -> defaultSuccess)
                    |> Expect.equal "" (Trial.warns warnings defaultSuccess)
            ]

            // if fail try and fold with

            testList "if fail try and fold" [
                testProperty L.pow <| fun success warnings elsePreTrial -> 
                    Trial.warns warnings success
                    |> Trial.ifFailTryAndFold (Trial.create <|| elsePreTrial)
                    |> Expect.equal "" (Trial.warns warnings success)

                testProperty (sprintf "%s -> %s" L .fail L.fail)                 
                        <| fun oldErrors oldWarnings newErrors newWarnings -> 
                    Trial.failsWithWarnings oldWarnings oldErrors
                    |> Trial.ifFailTryAndFold (
                        Trial.failsWithWarnings newWarnings newErrors)
                    |> Expect.equal "" (
                        Trial.failsWithWarnings 
                            (newWarnings @ oldWarnings) 
                            (newErrors @ oldErrors))

                testProperty (sprintf "%s -> %s" L .fail L.pow)                 
                        <| fun errors oldWarnings success newWarnings -> 
                    Trial.failsWithWarnings oldWarnings errors 
                    |> Trial.ifFailTryAndFold (
                        Trial.warns newWarnings success)
                    |> Expect.equal "" (
                        Trial.warns 
                            (newWarnings @ oldWarnings)
                            success)
            ]

            testList "collect" [
                //testProperty "all warns" <| fun successesAndWarnings -> 
                //    let successes, warnings = successesAndWarnings |> List.unzip
                //    List.map2 Trial.warns warnings successes
                //    |> Trial.collect 
                //    |> Expect.equal "" (
                //        Trial.warns (List.collect id warnings) successes)
                
                testProperty "full" <| fun resultsAndWarnings -> 
                    let results, warnings = resultsAndWarnings |> List.unzip
                    List.map2 Trial.create warnings results
                    |> Trial.collect
                    |> Expect.equal "" (
                        results
                        |> List.choose (function 
                            | Choice2Of2 p -> Some p
                            | _ -> None)
                        |> function 
                            | [] -> 
                                results 
                                |> List.choose (function 
                                    | Choice1Of2 p -> Some p
                                    | _ -> None)
                                |> Trial.warns (List.collect id warnings)
                            | errors -> 
                                Trial.failsWithWarnings 
                                    (List.collect id warnings) 
                                    (List.collect id errors)
                        )
            ]

            testList "collect while successed" [
                testProperty "all pass or warn" <| fun successAndWarnings -> 
                    let successes, warnings = successAndWarnings |> List.unzip
                    List.map2 Trial.warns warnings successes
                    |> Trial.collectWhileSuccessed 
                    |> Expect.equal "" (
                        Trial.warns (List.collect id warnings) successes)

                testProperty L.fail <| fun successAndWarnings errors warningsInFailed other -> 
                    let successes, warnings = successAndWarnings |> List.unzip
                    List.map2 Trial.warns warnings successes
                    |> List.append <| (
                        other
                        |> List.unzip
                        ||> List.map2 Trial.create
                        |> List.append [Trial.failsWithWarnings warningsInFailed errors])
                    |> Trial.collectWhileSuccessed
                    |> Expect.equal "" (
                        Trial.failsWithWarnings 
                            (List.collect id warnings @ warningsInFailed)
                            errors)
            ]

            testList "fail if None" [
                testProperty "None" <| fun error -> 
                    Trial.failIfNone error None
                    |> Expect.equal "" (Trial.fail error)

                testProperty "Some" <| fun value error -> 
                    Trial.failIfNone error (Some value)
                    |> Expect.equal "" (Trial.pass value)
            ]

            testList "try if None" [
                testProperty "Some" <| fun value warn -> 
                    Some value
                    |> Trial.tryIfNone warn shouldNotCall
                    |> Expect.equal "" (Trial.pass value)

                testProperty "None"  <| fun warn result warnings -> 
                    Trial.tryIfNone warn (fun () -> Trial.create warnings result) None
                    |> Expect.equal "" (
                        Trial.create (warn::warnings) result)
            ]

            testList "of choice" [
                testProperty "1 of 2" <| fun success ->  
                    Choice1Of2 success
                    |> Trial.ofChoice
                    |> Expect.equal "" (Trial.pass success)

                testProperty "2 of 2" <| fun error -> 
                    Choice2Of2 error
                    |> Trial.ofChoice
                    |> Expect.equal "" (Trial.fail error)
            ]

            testList "of result" [
                testProperty "Ok" <| fun success ->  
                    Ok success
                    |> Trial.ofResult
                    |> Expect.equal "" (Trial.pass success)

                testProperty "Error" <| fun error -> 
                    Error error
                    |> Trial.ofResult
                    |> Expect.equal "" (Trial.fail error)
            ]

            testList "apply" [
                testProperty L.pow <| fun fWarnings result warnings -> 
                    Trial.create warnings result 
                    |> Trial.apply (Trial.warns fWarnings hash)
                    |> Expect.equal "" (
                        Trial.create warnings result 
                        |> Trial.map hash 
                        |> Trial.addWarnings fWarnings)

                testProperty L.fail <| fun fErrors fWarnings result warnings -> 
                    Trial.create warnings result 
                    |> Trial.apply (Trial.failsWithWarnings fWarnings fErrors)
                    |> Expect.equal "" (
                        Trial.create warnings result 
                        |> Trial.addErrors fErrors
                        |> Trial.addWarnings fWarnings)
            ]

            testList "iter" [
                testProperty L.pow <| fun value warnings -> 
                    let mutable passTest = None
                    Trial.warns warnings value
                    |> Trial.iter (fun value -> passTest <- Some <| hash value)
                    |> Expect.equal "" ()
                    passTest |> Expect.equal "" (Some <| hash value)
                    
                testProperty L.fail <| fun errors warnings -> 
                    let mutable passTest = None
                    Trial.failsWithWarnings warnings errors
                    |> Trial.iter (fun value -> passTest <- Some <| hash value)
                    |> Expect.equal "" ()
                    passTest |> Expect.equal "" None                    
            ]

            testList "iter2" <| nondet {
                let config = [
                    L.pow, Trial.warns
                    L.fail, Trial.failsWithWarnings
                ]
                let name = fst
                let factory = snd
                let! left = config
                let! right = config
                return testProperty (sprintf "%s + %s" (name left) (name right)) <| fun a b -> 
                    let mutable passTest = None
                    Trial.iter2 
                        (fun a b -> passTest <- a @ b |> Some)
                        (factory left <|| a)
                        (factory right <|| b)
                    |> Expect.equal "" ()
                    passTest 
                    |> Expect.equal "" (
                        if name left = name right && name left = L.pow
                        then Some (snd a @ snd b)
                        else None)
                }
        ]

    module Distincted = 
        [<Tests>]
        let tests = 
            testList "module Distincted" [
                testList "collect" [
                    //yield testProperty "default should equal `Trial.collect`" <| fun resultsAndWarnings -> 
                    //    let results, warnings = resultsAndWarnings |> List.unzip
                    //    List.map2 Trial.create warnings results 
                    //    |> Trial.Distincted.collect Trial.Distincted.Config.empty
                    //    |> Expect.equal "" (
                    //        List.map2 Trial.create warnings results
                    //        |> Trial.collect)

                    yield! nondet {
                        let gen() = [
                            None, id
                            Some Trial.Distincted.Distint, List.distinct
                            Some (Trial.Distincted.DistinctBy hash), List.distinctBy hash
                        ]
                        let! success = gen()
                        let! warnings = gen()
                        let! errors = gen()
                        let genName = fst >> sprintf "%A"
                        return testProperty (
                            sprintf "%s %s %s" 
                                <| genName success
                                <| genName warnings
                                <| genName errors) <| fun resultsAndWarnings -> 
                            let results, warns = List.unzip resultsAndWarnings
                            List.map2 Trial.create results warns
                            |> Trial.Distincted.collect {
                                Success = fst success
                                Warnings = fst warnings
                                Errors = fst errors
                            }
                            |> Expect.equal "" (
                                List.map2 Trial.create results warns
                                |> Trial.collect
                                |> Trial.mapSuccess (snd success)
                                |> Trial.mapWarnings (snd warnings)
                                |> Trial.mapErrors (snd errors)
                                )
                        }
                ]
            ]
            

module TrialBuilder = 
    [<Tests>]
    let tests = 
        testList "TrialBuilder" [
            testList "bind" [
                testProperty L.pow <| fun success warnings -> 
                    trial {
                        let! p = Trial.warns warnings success
                        return hash p
                    }
                    |> Expect.equal "" (
                        hash success
                        |> Trial.warns warnings)

                testProperty L.fail <| fun errors warnings -> 
                    trial {
                        let! p = Trial.failsWithWarnings warnings errors
                        return hash p
                    }
                    |> Expect.equal "" (
                        Trial.failsWithWarnings warnings errors)
            ]

            testProperty "combine" <| fun value ->
                trial {
                    let! f = Trial.pass ()
                    if value <> 42 then 
                        do f
                    return  value
                }
                |> Expect.equal "" (Trial.pass value)

            testList "try with" [
                testProperty L.success <| fun value ->
                    trial { 
                        return 
                            try value 
                            with _ -> 42
                    }
                    |> Expect.equal "" (Trial.pass value)

                testProperty L.fail <| fun value -> 
                    trial {
                        return 
                            try 
                                failwith "BOO!!!"
                                42
                            with _ -> value
                    }
                    |> Expect.equal "" (Trial.pass value)
            ]

            testList "try finally" [
                testProperty L.success <| fun value -> 
                    let mutable passTest = false
                    trial {
                        try value
                        finally passTest <- true
                    }
                    |> Expect.equal "" (Trial.pass value)
                    passTest |> Expect.isTrue ""

                testCase L.fail <| fun () -> 
                    let mutable passTest = false
                    fun () -> 
                        trial {
                            try failwith "BOO!!!" 
                            finally passTest <- true
                        }
                        |> shouldNotCallT
                    |> Expect.throws ""
                    passTest |> Expect.isTrue ""
            ]

            testList "use" [
                testProperty L.success <| fun value -> 
                    let mutable passTest = false
                    trial {
                        use! s = 
                            { new System.IDisposable with 
                                member this.Dispose () = passTest <- true }
                            |> Trial.pass
                        return value
                    }
                    |> Expect.equal "" (Trial.pass value)
                    passTest |> Expect.isTrue ""
                    
                testCase L.fail <| fun () -> 
                    let mutable passTest = false
                    fun () -> 
                        trial {
                            use! s = 
                                { new System.IDisposable with 
                                    member this.Dispose () = passTest <- true }
                                |> Trial.pass
                            failwith "BOO!!!"
                            return 42
                        } 
                        |> shouldNotCallT
                    |> Expect.throws ""
                    passTest |> Expect.isTrue ""
            ]

            testCase "while" <| fun () -> 
                trial {
                    let mutable counter = 0
                    while counter < 42 do
                        counter <- counter + 1 
                    return 42
                }
                |> Expect.equal "" (Trial.pass 42)

            testProperty "for" <| fun items -> 
                trial {
                    let mutable counter = 0
                    for item in items do counter <- counter + item
                    return counter
                }
                |> Expect.equal "" (Trial.pass (items |> List.sum))
        ]