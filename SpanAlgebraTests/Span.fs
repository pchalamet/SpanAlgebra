module SpanAlgebraTests
open NUnit.Framework
open SpanAlgebra


let intersectionChar = (char)0x2229
let unionChar = (char)0x222A
let plusChar = '+'

type Combine =
    | A = 1
    | B = 2
    | AnB = 3 // A ||| B

[<Test>]
let checkIntersection () =
    let spans1 = [ Span.create Combine.A 0 1
                   Span.create Combine.A 4 5
                   Span.create Combine.A 7 10
                   Span.create Combine.A 13 15
                   Span.create Combine.A 17 20 
                   Span.create Combine.A 25 200 ]

    let spans2 = [ Span.create Combine.B 2 3
                   Span.create Combine.B 5 6 
                   Span.create Combine.B 8 11 
                   Span.create Combine.B 12 14
                   Span.create Combine.B 16 21 
                   Span.create Combine.B 26 40
                   Span.create Combine.B 45 50
                   Span.create Combine.B 80 100 ]

    let res = Span.intersect (|||) spans1 spans2
    printfn "input1:"
    spans1 |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)
    printfn ""
    printfn "input2:"
    spans2 |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)
    printfn ""
    printfn "result:"
    res |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)

    let expected = [ Span.create Combine.AnB 8 10
                     Span.create Combine.AnB 13 14
                     Span.create Combine.AnB 17 20
                     Span.create Combine.AnB 26 40
                     Span.create Combine.AnB 45 50
                     Span.create Combine.AnB 80 100 ]
    Assert.AreEqual(expected, res)


[<Test>]
let checkUnion () =
    let spans1 = [ Span.create Combine.A 0 1
                   Span.create Combine.A 4 5
                   Span.create Combine.A 7 10
                   Span.create Combine.A 13 15
                   Span.create Combine.A 17 20 
                   Span.create Combine.A 25 200 ]

    let spans2 = [ Span.create Combine.B 2 3
                   Span.create Combine.B 5 6 
                   Span.create Combine.B 8 11 
                   Span.create Combine.B 12 14
                   Span.create Combine.B 16 21 
                   Span.create Combine.B 26 40
                   Span.create Combine.B 45 50
                   Span.create Combine.B 80 100 ]

    printfn "input1:"
    spans1 |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)
    printfn ""
    printfn "input2:"
    spans2 |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)
    printfn ""
    printfn "result:"
    let res = Span.union (|||) spans1 spans2
    res |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)

    let expected = [ Span.create Combine.A 0 1
                     Span.create Combine.B 2 3
                     Span.create Combine.A 4 5
                     Span.create Combine.B 5 6
                     Span.create Combine.A 7 8 
                     Span.create Combine.AnB 8 10
                     Span.create Combine.B 10 11
                     Span.create Combine.B 12 13
                     Span.create Combine.AnB 13 14
                     Span.create Combine.A 14 15
                     Span.create Combine.B 16 17
                     Span.create Combine.AnB 17 20
                     Span.create Combine.B 20 21
                     Span.create Combine.A 25 26
                     Span.create Combine.AnB 26 40
                     Span.create Combine.A 40 45
                     Span.create Combine.AnB 45 50
                     Span.create Combine.A 50 80
                     Span.create Combine.AnB 80 100
                     Span.create Combine.A 100 200 ]
    Assert.AreEqual(expected, res)

[<Test>]
let checkMerge () =
    let spans = [ Span.create Combine.A 0 1
                  Span.create Combine.A 2 3
                  Span.create Combine.A 3 10
                  Span.create Combine.B 13 15
                  Span.create Combine.B 15 20 ]

    printfn "input:"
    spans |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)
    let res = Span.merge spans
    printfn ""
    printfn "result:"
    res |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)
    
    let expected = [ Span.create Combine.A 0 1
                     Span.create Combine.A 2 10
                     Span.create Combine.B 13 20 ]
    Assert.AreEqual(expected, res)

[<Test>]
let checkSingleton () =
    let res = Span.singleton Combine.A 1 10
    let expected = [ { Value = Combine.A; Start = 1; Stop = 10 } ]
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
    let segs = [ Span.create Combine.A 0 1
                 Span.create Combine.A 2 3
                 Span.create Combine.A 3 10
                 Span.create Combine.B 13 15
                 Span.create Combine.B 15 20 ]
    segs |> Span.validate

[<Test>]
let checkValidateRejectsInvalidSpan () =
    let segs = [ Span.create Combine.A 0 1
                 Span.create Combine.A 2 3
                 { Value = Combine.A; Start = 10; Stop = 3 }
                 Span.create Combine.B 13 15
                 Span.create Combine.B 15 20 ]
    try
        segs |> Span.validate
        failwithf "Validation should have detected invalid span"
    with
        _ -> ()

[<Test>]
let checkValidateRejectsUnorderedSpans () =
    let segs = [ Span.create Combine.A 0 1
                 Span.create Combine.A 3 10
                 Span.create Combine.A 2 3
                 Span.create Combine.B 13 15
                 Span.create Combine.B 15 20 ]
    try
        segs |> Span.validate
        failwithf "Validation should have detected unordered spans"
    with
        _ -> ()
