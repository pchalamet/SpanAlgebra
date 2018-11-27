module SegmentAlgebraTests
open NUnit.Framework
open SegmentAlgebra


let intersectionChar = (char)0x2229
let unionChar = (char)0x222A
let plusChar = '+'
let fmt a c b = sprintf "%s %c %s" a c b

[<Test>]
let checkIntersection () =
    let segs1 = [ createSegment 0 1 "[0-1]"
                  createSegment 4 5 "[4-5]"
                  createSegment 7 10 "[7-10]" 
                  createSegment 13 15 "[13-15]"
                  createSegment 17 20 "17-20]" ]

    let segs2 = [ createSegment 2 3 "[2-3]"
                  createSegment 5 6 "[5-6]"
                  createSegment 8 11 "[8-11]" 
                  createSegment 12 14 "[12-14]"
                  createSegment 16 21 "[16-21]" ]

    let res = intersect segs1 segs2 (fun a b -> fmt a intersectionChar b)
    printfn "input1:"
    segs1 |> List.iter (fun x -> printfn "[%d, %d] = %s" x.Start x.Stop x.Value)
    printfn "input2:"
    segs2 |> List.iter (fun x -> printfn "[%d, %d] = %s" x.Start x.Stop x.Value)
    printfn "result:"
    res |> List.iter (fun x -> printfn "[%d, %d] = %s" x.Start x.Stop x.Value)

    let expected = [ createSegment 8 10 (fmt "[7-10]" intersectionChar "[8-11]")
                     createSegment 13 14 (fmt "[12-14]" intersectionChar "[13-15]")
                     createSegment 17 20 (fmt "[16-21]" intersectionChar "17-20]") ]
    Assert.AreEqual(expected, res)


[<Test>]
let checkUnion () =
    let segs1 = [ createSegment 0 1 "[0-1]"
                  createSegment 4 5 "[4-5]"
                  createSegment 7 10 "[7-10]" 
                  createSegment 13 15 "[13-15]"
                  createSegment 17 20 "17-20]" ]

    let segs2 = [ createSegment 2 3 "[2-3]"
                  createSegment 5 6 "[5-6]"
                  createSegment 8 11 "[8-11]" 
                  createSegment 12 14 "[12-14]"
                  createSegment 16 21 "[16-21]" ]

    printfn "input1:"
    segs1 |> List.iter (fun x -> printfn "[%d, %d] = %s" x.Start x.Stop x.Value)
    printfn "input2:"
    segs2 |> List.iter (fun x -> printfn "[%d, %d] = %s" x.Start x.Stop x.Value)
    printfn "result:"
    let res = union segs1 segs2 (fun a b -> fmt a unionChar b)
    res |> List.iter (fun x -> printfn "[%d, %d] = %s" x.Start x.Stop x.Value)

    let expected = [ createSegment 0 1 "[0-1]"
                     createSegment 2 3 "[2-3]"
                     createSegment 4 6 (fmt "[4-5]" unionChar "[5-6]")
                     createSegment 7 11 (fmt "[7-10]" unionChar "[8-11]")
                     createSegment 12 15 (fmt "[12-14]" unionChar "[13-15]")
                     createSegment 16 21 (fmt "[16-21]" unionChar "17-20]") ]
    Assert.AreEqual(expected, res)

[<Test>]
let checkCombine () =
    let seg = [ createSegment 0 1 "[0-1]"
                createSegment 4 5 "[4-5]"
                createSegment 7 10 "[7-10]" 
                createSegment 13 15 "[13-15]"
                createSegment 17 20 "17-20]" ]

    let elt = createSegment 1 4 "[1-4]"

    let res = combine elt seg (fun a b -> fmt a plusChar b)
    res |> List.iter (fun x -> printfn "[%d, %d] = %s" x.Start x.Stop x.Value)

    let expected = [ createSegment 0 5 (fmt (fmt "[0-1]" plusChar "[1-4]") plusChar "[4-5]")
                     createSegment 7 10 "[7-10]" 
                     createSegment 13 15 "[13-15]"
                     createSegment 17 20 "17-20]" ]
    Assert.AreEqual(expected, res)
