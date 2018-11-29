module SpanAlgebraTests
open NUnit.Framework
open SpanAlgebra
open SpanAlgebra.Span


let intersectionChar = (char)0x2229
let unionChar = (char)0x222A
let plusChar = '+'
let fmt a c b = sprintf "%s %c %s" a c b

type Combine =
    | A = 1
    | B = 2
    | AnB = 3 // A ||| B

[<Test>]
let checkIntersection () =
    let segs1 = [ createSpan 0 1 Combine.A
                  createSpan 4 5 Combine.A
                  createSpan 7 10 Combine.A
                  createSpan 13 15 Combine.A
                  createSpan 17 20 Combine.A 
                  createSpan 25 200 Combine.A ]

    let segs2 = [ createSpan 2 3 Combine.B
                  createSpan 5 6 Combine.B
                  createSpan 8 11 Combine.B 
                  createSpan 12 14 Combine.B
                  createSpan 16 21 Combine.B 
                  createSpan 26 40 Combine.B
                  createSpan 45 50 Combine.B
                  createSpan 80 100 Combine.B ]

    let res = intersect segs1 segs2 (|||)
    printfn "input1:"
    segs1 |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)
    printfn "input2:"
    segs2 |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)
    printfn "result:"
    res |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)

    let expected = [ createSpan 8 10 Combine.AnB
                     createSpan 13 14 Combine.AnB
                     createSpan 17 20 Combine.AnB 
                     createSpan 26 40 Combine.AnB
                     createSpan 45 50 Combine.AnB
                     createSpan 80 100 Combine.AnB ]
    Assert.AreEqual(expected, res)


[<Test>]
let checkUnion () =
    let segs1 = [ createSpan 0 1 Combine.A
                  createSpan 4 5 Combine.A
                  createSpan 7 10 Combine.A 
                  createSpan 13 15 Combine.A
                  createSpan 17 20 Combine.A 
                  createSpan 25 200 Combine.A ]

    let segs2 = [ createSpan 2 3 Combine.B
                  createSpan 5 6 Combine.B
                  createSpan 8 11 Combine.B
                  createSpan 12 14 Combine.B
                  createSpan 16 21 Combine.B
                  createSpan 26 40 Combine.B
                  createSpan 45 50 Combine.B
                  createSpan 80 100 Combine.B ]

    printfn "input1:"
    segs1 |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)
    printfn "input2:"
    segs2 |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)
    printfn "result:"
    let res = union segs1 segs2 (|||)
    res |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)

    let expected = [ createSpan 0 1 Combine.A
                     createSpan 2 3 Combine.B
                     createSpan 4 5 Combine.A
                     createSpan 5 6 Combine.B
                     createSpan 7 8 Combine.A
                     createSpan 8 10 Combine.AnB
                     createSpan 10 11 Combine.B
                     createSpan 12 13 Combine.B
                     createSpan 13 14 Combine.AnB
                     createSpan 14 15 Combine.A
                     createSpan 16 17 Combine.B
                     createSpan 17 20 Combine.AnB
                     createSpan 20 21 Combine.B 
                     createSpan 25 26 Combine.A
                     createSpan 26 40 Combine.AnB
                     createSpan 40 45 Combine.A
                     createSpan 45 50 Combine.AnB
                     createSpan 50 80 Combine.A
                     createSpan 80 100 Combine.AnB
                     createSpan 100 200 Combine.A ]
    Assert.AreEqual(expected, res)

[<Test>]
let checkMerge () =
    let segs = [ createSpan 0 1 Combine.A
                 createSpan 2 3 Combine.A
                 createSpan 3 10 Combine.A
                 createSpan 13 15 Combine.B
                 createSpan 15 20 Combine.B ]

    printfn "input:"
    segs |> List.iter (fun x -> printfn "  [%d, %d] = %A" x.Start x.Stop x.Value)
    let res = merge segs
    res |> List.iter (fun x -> printfn "  [%d, %d] = %A" x.Start x.Stop x.Value)
    
    let expected = [ createSpan 0 1 Combine.A
                     createSpan 2 10 Combine.A
                     createSpan 13 20 Combine.B ]
    Assert.AreEqual(expected, res)

[<Test>]
let checkCreateNominal () =
    let int = createSpan 10 20 "toto"
    let expected = { Start = 10; Stop = 20; Value = "toto" }
    Assert.AreEqual(expected, int)

[<Test>]
let failureIfStartAndStopEqual () =
    try
        let int = createSpan 10 10 "toto"
        failwithf "Can't create interval with Start and Stop equal"
    with
        _ -> ()
    
[<Test>]
let failureIfStartGreaterThanStop () =
    try
        let int = createSpan 10 8 "toto"
        failwithf "Can't create interval with Start greater than Stop"
    with
        _ -> ()