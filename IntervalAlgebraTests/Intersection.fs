module IntervalAlgebraTests
open NUnit.Framework
open IntervalAlgebra


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
    let segs1 = [ createInterval 0 1 Combine.A
                  createInterval 4 5 Combine.A
                  createInterval 7 10 Combine.A
                  createInterval 13 15 Combine.A
                  createInterval 17 20 Combine.A 
                  createInterval 25 200 Combine.A ]

    let segs2 = [ createInterval 2 3 Combine.B
                  createInterval 5 6 Combine.B
                  createInterval 8 11 Combine.B 
                  createInterval 12 14 Combine.B
                  createInterval 16 21 Combine.B 
                  createInterval 26 40 Combine.B
                  createInterval 45 50 Combine.B
                  createInterval 80 100 Combine.B ]

    let res = intersect segs1 segs2 (|||)
    printfn "input1:"
    segs1 |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)
    printfn "input2:"
    segs2 |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)
    printfn "result:"
    res |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)

    let expected = [ createInterval 8 10 Combine.AnB
                     createInterval 13 14 Combine.AnB
                     createInterval 17 20 Combine.AnB 
                     createInterval 26 40 Combine.AnB
                     createInterval 45 50 Combine.AnB
                     createInterval 80 100 Combine.AnB ]
    Assert.AreEqual(expected, res)


[<Test>]
let checkUnion () =
    let segs1 = [ createInterval 0 1 Combine.A
                  createInterval 4 5 Combine.A
                  createInterval 7 10 Combine.A 
                  createInterval 13 15 Combine.A
                  createInterval 17 20 Combine.A 
                  createInterval 25 200 Combine.A ]

    let segs2 = [ createInterval 2 3 Combine.B
                  createInterval 5 6 Combine.B
                  createInterval 8 11 Combine.B
                  createInterval 12 14 Combine.B
                  createInterval 16 21 Combine.B
                  createInterval 26 40 Combine.B
                  createInterval 45 50 Combine.B
                  createInterval 80 100 Combine.B ]

    printfn "input1:"
    segs1 |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)
    printfn "input2:"
    segs2 |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)
    printfn "result:"
    let res = union segs1 segs2 (|||)
    res |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)

    let expected = [ createInterval 0 1 Combine.A
                     createInterval 2 3 Combine.B
                     createInterval 4 5 Combine.A
                     createInterval 5 6 Combine.B
                     createInterval 7 8 Combine.A
                     createInterval 8 10 Combine.AnB
                     createInterval 10 11 Combine.B
                     createInterval 12 13 Combine.B
                     createInterval 13 14 Combine.AnB
                     createInterval 14 15 Combine.A
                     createInterval 16 17 Combine.B
                     createInterval 17 20 Combine.AnB
                     createInterval 20 21 Combine.B 
                     createInterval 25 26 Combine.A
                     createInterval 26 40 Combine.AnB
                     createInterval 40 45 Combine.A
                     createInterval 45 50 Combine.AnB
                     createInterval 50 80 Combine.A
                     createInterval 80 100 Combine.AnB
                     createInterval 100 200 Combine.A ]
    Assert.AreEqual(expected, res)

[<Test>]
let checkMerge () =
    let segs = [ createInterval 0 1 Combine.A
                 createInterval 2 3 Combine.A
                 createInterval 3 10 Combine.A
                 createInterval 13 15 Combine.B
                 createInterval 15 20 Combine.B ]

    printfn "input:"
    segs |> List.iter (fun x -> printfn "  [%d, %d] = %A" x.Start x.Stop x.Value)
    let res = merge segs
    res |> List.iter (fun x -> printfn "  [%d, %d] = %A" x.Start x.Stop x.Value)
    
    let expected = [ createInterval 0 1 Combine.A
                     createInterval 2 10 Combine.A
                     createInterval 13 20 Combine.B ]
    Assert.AreEqual(expected, res)

[<Test>]
let checkCreateNominal () =
    let int = createInterval 10 20 "toto"
    let expected = { Start = 10; Stop = 20; Value = "toto" }
    Assert.AreEqual(expected, int)

[<Test>]
let failureIfStartAndStopEqual () =
    try
        let int = createInterval 10 10 "toto"
        failwithf "Can't create interval with Start and Stop equal"
    with
        _ -> ()
    
[<Test>]
let failureIfStartGreaterThanStop () =
    try
        let int = createInterval 10 8 "toto"
        failwithf "Can't create interval with Start greater than Stop"
    with
        _ -> ()
