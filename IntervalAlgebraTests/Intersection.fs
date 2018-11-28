module IntervalAlgebraTests
open NUnit.Framework
open IntervalAlgebra


let intersectionChar = (char)0x2229
let unionChar = (char)0x222A
let plusChar = '+'
let fmt a c b = sprintf "%s %c %s" a c b

[<Test>]
let checkIntersection () =
    let segs1 = [ createInterval 0 1 1
                  createInterval 4 5 1
                  createInterval 7 10 1 
                  createInterval 13 15 1
                  createInterval 17 20 1 ]

    let segs2 = [ createInterval 2 3 2
                  createInterval 5 6 2
                  createInterval 8 11 2 
                  createInterval 12 14 2
                  createInterval 16 21 2 ]

    let res = intersect segs1 segs2 (|||)
    printfn "input1:"
    segs1 |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)
    printfn "input2:"
    segs2 |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)
    printfn "result:"
    res |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)

    let expected = [ createInterval 8 10 3
                     createInterval 13 14 3
                     createInterval 17 20 3 ]
    Assert.AreEqual(expected, res)


[<Test>]
let checkUnion () =
    let segs1 = [ createInterval 0 1 1
                  createInterval 4 5 1
                  createInterval 7 10 1 
                  createInterval 13 15 1
                  createInterval 17 20 1 ]

    let segs2 = [ createInterval 2 3 2
                  createInterval 5 6 2
                  createInterval 8 11 2 
                  createInterval 12 14 2
                  createInterval 16 21 2 ]

    printfn "input1:"
    segs1 |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)
    printfn "input2:"
    segs2 |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)
    printfn "result:"
    let res = union segs1 segs2 (|||)
    res |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)

    let expected = [ createInterval 0 1 1
                     createInterval 2 3 2
                     createInterval 4 5 1
                     createInterval 5 6 2
                     createInterval 7 8 1
                     createInterval 8 10 3
                     createInterval 10 11 2
                     createInterval 12 13 2
                     createInterval 13 14 3
                     createInterval 14 15 1
                     createInterval 16 17 2
                     createInterval 17 20 3
                     createInterval 20 21 2 ]
    Assert.AreEqual(expected, res)

[<Test>]
let checkCombine () =
    let seg = [ createInterval 0 1 1
                createInterval 4 5 1
                createInterval 7 10 1 
                createInterval 13 15 1
                createInterval 17 20 1 ]

    let elt = createInterval 1 5 2

    let res = combine elt seg (|||)
    res |> List.iter (fun x -> printfn "[%d, %d] = %A" x.Start x.Stop x.Value)

    let expected = [ createInterval 0 1 1
                     createInterval 1 4 2
                     createInterval 4 5 3
                     createInterval 7 10 1 
                     createInterval 13 15 1
                     createInterval 17 20 1 ]
    Assert.AreEqual(expected, res)
