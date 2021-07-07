module SpanAlgebraTests
open NUnit.Framework
open SpanAlgebra


let intersectionChar = (char)0x2229
let unionChar = (char)0x222A
let plusChar = '+'

type Combination =
    | Nothing = 0
    | A = 1
    | B = 2
    | AnB = 3 // A ||| B

[<Test>]
let checkIntersection () =
    let spans1 = [ Span.create Combination.A 0 1
                   Span.create Combination.A 4 5
                   Span.create Combination.A 7 10
                   Span.create Combination.A 13 15
                   Span.create Combination.A 17 20 
                   Span.create Combination.A 25 200 ]

    let spans2 = [ Span.create Combination.B 2 3
                   Span.create Combination.B 5 6 
                   Span.create Combination.B 8 11 
                   Span.create Combination.B 12 14
                   Span.create Combination.B 16 21 
                   Span.create Combination.B 26 40
                   Span.create Combination.B 45 50
                   Span.create Combination.B 80 100 ]

    let res = Span.intersect (|||) spans1 spans2
    printfn "input1:"
    spans1 |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)
    printfn ""
    printfn "input2:"
    spans2 |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)
    printfn ""
    printfn "result:"
    res |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)

    let expected = [ Span.create Combination.AnB 8 10
                     Span.create Combination.AnB 13 14
                     Span.create Combination.AnB 17 20
                     Span.create Combination.AnB 26 40
                     Span.create Combination.AnB 45 50
                     Span.create Combination.AnB 80 100 ]
    Assert.AreEqual(expected, res)


[<Test>]
let checkUnion () =
    let spans1 = [ Span.create Combination.A 0 1
                   Span.create Combination.A 4 5
                   Span.create Combination.A 7 10
                   Span.create Combination.A 13 15
                   Span.create Combination.A 17 20 
                   Span.create Combination.A 25 200 ]

    let spans2 = [ Span.create Combination.B 2 3
                   Span.create Combination.B 5 6 
                   Span.create Combination.B 8 11 
                   Span.create Combination.B 12 14
                   Span.create Combination.B 16 21 
                   Span.create Combination.B 26 40
                   Span.create Combination.B 45 50
                   Span.create Combination.B 80 100 ]

    printfn "input1:"
    spans1 |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)
    printfn ""
    printfn "input2:"
    spans2 |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)
    printfn ""
    printfn "result:"
    let combinator x y =
        match x,y with
        | Some x, Some y -> x ||| y
        | Some x, _ -> x
        | _, Some y -> y
        | _ -> Combination.Nothing

    let res = Span.union combinator spans1 spans2
    res |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)

    let expected = [ Span.create Combination.A 0 1
                     Span.create Combination.B 2 3
                     Span.create Combination.A 4 5
                     Span.create Combination.B 5 6
                     Span.create Combination.A 7 8 
                     Span.create Combination.AnB 8 10
                     Span.create Combination.B 10 11
                     Span.create Combination.B 12 13
                     Span.create Combination.AnB 13 14
                     Span.create Combination.A 14 15
                     Span.create Combination.B 16 17
                     Span.create Combination.AnB 17 20
                     Span.create Combination.B 20 21
                     Span.create Combination.A 25 26
                     Span.create Combination.AnB 26 40
                     Span.create Combination.A 40 45
                     Span.create Combination.AnB 45 50
                     Span.create Combination.A 50 80
                     Span.create Combination.AnB 80 100
                     Span.create Combination.A 100 200 ]
    Assert.AreEqual(expected, res)

[<Test>]
let checkMerge () =
    let spans = [ Span.create Combination.A 0 1
                  Span.create Combination.A 2 3
                  Span.create Combination.A 3 10
                  Span.create Combination.B 13 15
                  Span.create Combination.B 15 20 ]

    printfn "input:"
    spans |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)
    let res = Span.merge spans
    printfn ""
    printfn "result:"
    res |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)
    
    let expected = [ Span.create Combination.A 0 1
                     Span.create Combination.A 2 10
                     Span.create Combination.B 13 20 ]
    Assert.AreEqual(expected, res)

[<Test>]
let checkSingleton () =
    let res = Span.singleton Combination.A 1 10
    let expected = [ { Value = Combination.A; Start = 1; Stop = 10 } ]
    Assert.AreEqual(expected, res)



[<Test>]
let checkCreateNominal () =
    let int = Span.create "toto" 10 20
    let expected = { Value = "toto"; Start = 10; Stop = 20 }
    Assert.AreEqual(expected, int)

[<Test>]
let failureIfStartAndStopEqual () =
    try
        Span.create "toto" 10 10 |> ignore
        failwithf "Can't create interval with Start and Stop equal"
    with
        _ -> ()
    
[<Test>]
let failureIfStartGreaterThanStop () =
    try
        Span.create "toto" 10 8 |> ignore
        failwithf "Can't create interval with Start greater than Stop"
    with
        _ -> ()

[<Test>]
let checkValidateNominal () =
    let segs = [ Span.create Combination.A 0 1
                 Span.create Combination.A 2 3
                 Span.create Combination.A 3 10
                 Span.create Combination.B 13 15
                 Span.create Combination.B 15 20 ]
    let res = segs |> Span.validate
    Assert.AreEqual(segs, res)

[<Test>]
let checkValidateRejectsInvalidSpan () =
    let segs = [ Span.create Combination.A 0 1
                 Span.create Combination.A 2 3
                 { Value = Combination.A; Start = 10; Stop = 3 }
                 Span.create Combination.B 13 15
                 Span.create Combination.B 15 20 ]
    try
        segs |> Span.validate |> ignore
        failwithf "Validation should have detected invalid span"
    with
        _ -> ()

[<Test>]
let checkValidateRejectsUnorderedSpans () =
    let segs = [ Span.create Combination.A 0 1
                 Span.create Combination.A 3 10
                 Span.create Combination.A 2 3
                 Span.create Combination.B 13 15
                 Span.create Combination.B 15 20 ]
    try
        segs |> Span.validate |> ignore
        failwithf "Validation should have detected unordered spans"
    with
        _ -> ()
