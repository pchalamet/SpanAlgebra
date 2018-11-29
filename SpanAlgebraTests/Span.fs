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
    let spans1 = [ Span.createSpan 0 1 Combine.A
                   Span.createSpan 4 5 Combine.A
                   Span.createSpan 7 10 Combine.A
                   Span.createSpan 13 15 Combine.A
                   Span.createSpan 17 20 Combine.A 
                   Span.createSpan 25 200 Combine.A ]

    let spans2 = [ Span.createSpan 2 3 Combine.B
                   Span.createSpan 5 6 Combine.B
                   Span.createSpan 8 11 Combine.B 
                   Span.createSpan 12 14 Combine.B
                   Span.createSpan 16 21 Combine.B 
                   Span.createSpan 26 40 Combine.B
                   Span.createSpan 45 50 Combine.B
                   Span.createSpan 80 100 Combine.B ]

    let res = Span.intersect spans1 spans2 (|||)
    printfn "input1:"
    spans1 |> List.iter (fun x -> printfn "  [%d, %d] = %A" x.Start x.Stop x.Value)
    printfn ""
    printfn "input2:"
    spans2 |> List.iter (fun x -> printfn "  [%d, %d] = %A" x.Start x.Stop x.Value)
    printfn ""
    printfn "result:"
    res |> List.iter (fun x -> printfn "  [%d, %d] = %A" x.Start x.Stop x.Value)

    let expected = [ Span.createSpan 8 10 Combine.AnB
                     Span.createSpan 13 14 Combine.AnB
                     Span.createSpan 17 20 Combine.AnB 
                     Span.createSpan 26 40 Combine.AnB
                     Span.createSpan 45 50 Combine.AnB
                     Span.createSpan 80 100 Combine.AnB ]
    Assert.AreEqual(expected, res)


[<Test>]
let checkUnion () =
    let spans1 = [ Span.createSpan 0 1 Combine.A
                   Span.createSpan 4 5 Combine.A
                   Span.createSpan 7 10 Combine.A 
                   Span.createSpan 13 15 Combine.A
                   Span.createSpan 17 20 Combine.A 
                   Span.createSpan 25 200 Combine.A ]

    let spans2 = [ Span.createSpan 2 3 Combine.B
                   Span.createSpan 5 6 Combine.B
                   Span.createSpan 8 11 Combine.B
                   Span.createSpan 12 14 Combine.B
                   Span.createSpan 16 21 Combine.B
                   Span.createSpan 26 40 Combine.B
                   Span.createSpan 45 50 Combine.B
                   Span.createSpan 80 100 Combine.B ]

    printfn "input1:"
    spans1 |> List.iter (fun x -> printfn "  [%d, %d] = %A" x.Start x.Stop x.Value)
    printfn ""
    printfn "input2:"
    spans2 |> List.iter (fun x -> printfn "  [%d, %d] = %A" x.Start x.Stop x.Value)
    printfn ""
    printfn "result:"
    let res = Span.union spans1 spans2 (|||)
    res |> List.iter (fun x -> printfn "  [%d, %d] = %A" x.Start x.Stop x.Value)

    let expected = [ Span.createSpan 0 1 Combine.A
                     Span.createSpan 2 3 Combine.B
                     Span.createSpan 4 5 Combine.A
                     Span.createSpan 5 6 Combine.B
                     Span.createSpan 7 8 Combine.A
                     Span.createSpan 8 10 Combine.AnB
                     Span.createSpan 10 11 Combine.B
                     Span.createSpan 12 13 Combine.B
                     Span.createSpan 13 14 Combine.AnB
                     Span.createSpan 14 15 Combine.A
                     Span.createSpan 16 17 Combine.B
                     Span.createSpan 17 20 Combine.AnB
                     Span.createSpan 20 21 Combine.B 
                     Span.createSpan 25 26 Combine.A
                     Span.createSpan 26 40 Combine.AnB
                     Span.createSpan 40 45 Combine.A
                     Span.createSpan 45 50 Combine.AnB
                     Span.createSpan 50 80 Combine.A
                     Span.createSpan 80 100 Combine.AnB
                     Span.createSpan 100 200 Combine.A ]
    Assert.AreEqual(expected, res)

[<Test>]
let checkMerge () =
    let segs = [ Span.createSpan 0 1 Combine.A
                 Span.createSpan 2 3 Combine.A
                 Span.createSpan 3 10 Combine.A
                 Span.createSpan 13 15 Combine.B
                 Span.createSpan 15 20 Combine.B ]

    printfn "input:"
    segs |> List.iter (fun x -> printfn "  [%d, %d] = %A" x.Start x.Stop x.Value)
    let res = Span.merge segs
    printfn ""
    printfn "result:"
    res |> List.iter (fun x -> printfn "  [%d, %d] = %A" x.Start x.Stop x.Value)
    
    let expected = [ Span.createSpan 0 1 Combine.A
                     Span.createSpan 2 10 Combine.A
                     Span.createSpan 13 20 Combine.B ]
    Assert.AreEqual(expected, res)

[<Test>]
let checkCreateNominal () =
    let int = Span.createSpan 10 20 "toto"
    let expected = { Start = 10; Stop = 20; Value = "toto" }
    Assert.AreEqual(expected, int)

[<Test>]
let failureIfStartAndStopEqual () =
    try
        Span.createSpan 10 10 "toto" |> ignore
        failwithf "Can't create interval with Start and Stop equal"
    with
        _ -> ()
    
[<Test>]
let failureIfStartGreaterThanStop () =
    try
        Span.createSpan 10 8 "toto" |> ignore
        failwithf "Can't create interval with Start greater than Stop"
    with
        _ -> ()
