namespace SpanAlgebra

type Span<'t, 'v when 't : comparison and 'v : equality> = 
    { Start : 't
      Stop : 't
      Value : 'v}

module Span =

    // helper function to create a valid span
    // if you do not use it you are at your own risks :-)
    let create start stop value = 
        if start < stop then { Start = start; Stop = stop; Value = value }
        else failwithf "start must be strictly lower than stop"

    // compute the intersection of two spans lists
    // the result is not necessarily optimal - see merge
    let intersect span1 span2 comb =
        let rec intersect span1 span2 =
            seq {
                match span1 |> Seq.tryHead, span2 |> Seq.tryHead with
                | Some head1, Some head2 -> let tail1 = span1 |> Seq.tail
                                            let tail2 = span2 |> Seq.tail
                                            if (head2.Start < head1.Start) || (head1.Start = head2.Start && head2.Stop < head1.Stop) then 
                                                yield! intersect span2 span1
                                            elif head1.Stop <= head2.Start then 
                                                yield! intersect tail1 span2
                                            else
                                                let head = { Start = max head1.Start head2.Start
                                                             Stop = min head1.Stop head2.Stop
                                                             Value = comb head1.Value head2.Value }
                                                yield head
                                                if head1.Stop = head.Stop && head2.Stop = head.Stop then
                                                    yield! intersect tail1 tail2
                                                elif head1.Stop = head.Stop then
                                                    let remainder = { head2 with Start = head.Stop } |> Seq.singleton
                                                    yield! intersect tail1 (Seq.append remainder tail2)
                                                else
                                                    let remainder = { head1 with Start = head.Stop } |> Seq.singleton
                                                    yield! intersect (Seq.append remainder tail1) tail2
                | _ -> ()
            }
        intersect span1 span2

    // compute the union of two span lists
    // the result is not necessarily optimal - see merge
    let union span1 span2 comb =
        let rec union span1 span2 =
            seq {
                match span1 |> Seq.tryHead, span2 |> Seq.tryHead with
                | Some head1, Some head2 -> let tail1 = span1 |> Seq.tail
                                            let tail2 = span2 |> Seq.tail
                                            if (head2.Start < head1.Start) || (head1.Start = head2.Start && head1.Stop < head2.Stop) then 
                                                yield! union span2 span1
                                            elif head1.Stop <= head2.Start then 
                                                yield head1
                                                yield! union tail1 span2
                                            elif head1.Start < head2.Start then
                                                let head = { head1 with Stop = head2.Start }
                                                yield head
                                                let remainder = { head1 with Start = head2.Start } |> Seq.singleton
                                                yield! union (Seq.append remainder tail1) span2
                                            else
                                                let head = { head1 with Stop = head2.Stop
                                                                        Value = comb head1.Value head2.Value }                                        
                                                yield head
                                                if head1.Start = head2.Start && head1.Stop = head2.Stop then
                                                    yield! union tail1 tail2 
                                                else
                                                    let remainder = { head1 with Start = head2.Stop } |> Seq.singleton
                                                    yield! union (Seq.append remainder tail1) tail2
                | _ -> yield! span1
                       yield! span2
            }
        union span1 span2                                

    // merge adjacent spans in the list if the value is the same
    // typically intersect and union do not produce optimal result (for code simplicity)
    let rec merge spans =
        seq {
            match spans |> Seq.tryHead with
            | Some head1 -> let tail1 = spans |> Seq.tail
                            match tail1 |> Seq.tryHead with
                            | Some head2 -> let tail2 = tail1 |> Seq.tail
                                            if head1.Stop = head2.Start && head1.Value = head2.Value then
                                                let head = { head1 with Stop = head2.Stop }
                                                yield head
                                                yield! merge tail2
                                            else
                                                yield head1
                                                yield! merge tail1
                            | _ -> yield! spans
            | _ -> yield! spans
        }

    let validate spans =
        let rec validate { Span.Start = prevStart; Span.Stop = prevStop; Span.Value = prevValue } spans =
            if prevStop <= prevStart then failwithf "Start must be stricly lower than Stop: %A" { Span.Start = prevStart; Span.Stop = prevStop; Span.Value = prevValue }
            match spans |> Seq.tryHead with
            | Some { Span.Start = start; Span.Stop = _; Span.Value = _ } as head -> let tail = spans |> Seq.tail
                                                                                    if start < prevStop then failwithf "Span %A starts before %A"  head { Span.Start = prevStart; Span.Stop = prevStop; Span.Value = prevValue }
                                                                                    validate head.Value tail
            | _ -> ()

        match spans |> Seq.tryHead with
        | Some head -> let tail = spans |> Seq.tail
                       validate head tail
        | _ -> ()
