module SpanAlgebraTests
open NUnit.Framework
open SpanAlgebra


let intersectionChar = (char)0x2229
let unionChar = (char)0x222A
let plusChar = '+'

type Combination =
    | A = 1
    | B = 2
    | AnB = 3 // A ||| B


[<Test>]
let checkCombine () =
    let spans1 = [ Span.create Combination.A 0 1
                   Span.create Combination.A 4 5
                   Span.create Combination.A 7 10
                   Span.create Combination.A 13 15
                   Span.create Combination.A 17 20 
                   Span.create Combination.A 25 200 ]

    let spans2 = [ Span.create 1 2 3
                   Span.create 2 5 6 
                   Span.create 3 8 11 
                   Span.create 4 12 14
                   Span.create 5 16 21 
                   Span.create 6 26 40
                   Span.create 7 45 50
                   Span.create 8 80 100 ]

    printfn "input1:"
    spans1 |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)
    printfn ""
    printfn "input2:"
    spans2 |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)
    printfn ""
    printfn "result:"

    let combinator x y = (x, y)

    let res = Span.combine combinator spans1 spans2
    res |> List.iter (fun x -> printfn "  [%d, %d[ = %A" x.Start x.Stop x.Value)

    let expected = [ Span.create (Combination.A, 3) 8 10
                     Span.create (Combination.A, 4) 13 14
                     Span.create (Combination.A, 5) 17 20
                     Span.create (Combination.A, 6) 26 40 ]
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

// [<Test>]
// let checkClamp () =
//     let spans1 = [ Span.create Combination.A 0 1
//                    Span.create Combination.A 4 5
//                    Span.create Combination.A 7 10
//                    Span.create Combination.A 13 15
//                    Span.create Combination.A 17 20 
//                    Span.create Combination.A 25 200 ]

//     let res = Span.clamp spans1 8 19
//     res |> Span.print

//     let expected = [ Span.create Combination.A 8 10
//                      Span.create Combination.A 13 15
//                      Span.create Combination.A 17 19 ]
//     Assert.AreEqual(expected, res)
