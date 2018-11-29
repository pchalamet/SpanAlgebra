namespace SpanAlgebra

type Span<'t, 'v when 't : comparison> = 
    { Start : 't
      Stop : 't
      Value : 'v}

module Span =

    // helper function to create a valid span
    // if you do not use it you are at your own risks :-)
    let createSpan start stop value = 
        if start < stop then { Start = start; Stop = stop; Value = value }
        else failwithf "start must be strictly lower than stop"

    // compute the intersection of two spans lists
    // the result is not necessarily optimal - see merge
    let intersect span1 span2 comb =
        let rec intersect span1 span2 =
            match span1, span2 with
            | head1::tail1, head2::tail2 -> if (head2.Start < head1.Start) || (head1.Start = head2.Start && head2.Stop < head1.Stop) then intersect span2 span1
                                            elif head1.Stop <= head2.Start then intersect tail1 span2
                                            else
                                                let head = { Start = max head1.Start head2.Start
                                                             Stop = min head1.Stop head2.Stop
                                                             Value = comb head1.Value head2.Value }
                                                head :: if head1.Stop = head.Stop && head2.Stop = head.Stop then
                                                            intersect tail1 tail2
                                                        elif head1.Stop = head.Stop then
                                                            let remainder = { head2 with Start = head.Stop }
                                                            intersect tail1 (remainder::tail2)
                                                        else
                                                            let remainder = { head1 with Start = head.Stop }
                                                            intersect (remainder::tail1) tail2
            | _ -> []
        intersect span1 span2

    // compute the union of two span lists
    // the result is not necessarily optimal - see merge
    let union span1 span2 comb =
        let rec union span1 span2 =
            match span1, span2 with
            | head1::tail1, head2::tail2 -> if (head2.Start < head1.Start) || (head1.Start = head2.Start && head1.Stop < head2.Stop) then union span2 span1
                                            elif head1.Stop <= head2.Start then head1 :: union tail1 span2
                                            elif head1.Start < head2.Start then
                                                let head = { head1 with Stop = head2.Start }
                                                let remainder = { head1 with Start = head2.Start }
                                                head :: union (remainder::tail1) span2
                                            else
                                                let head = { head1 with Stop = head2.Stop
                                                                        Value = comb head1.Value head2.Value }                                        
                                                head :: if head1.Start = head2.Start && head1.Stop = head2.Stop then
                                                            union tail1 tail2 
                                                        else
                                                            let remainder = { head1 with Start = head2.Stop }
                                                            union (remainder::tail1) tail2
            | _ -> span1 @ span2
        union span1 span2                                

    // merge adjacent spans in the list if the value is the same
    // typically intersect and union do not produce optimal result (for code simplicity)
    let rec merge span =
        match span with
        | head1 :: head2 :: tail -> if head1.Stop = head2.Start && head1.Value = head2.Value then
                                        merge ({ head1 with Stop = head2.Stop } :: merge tail)
                                    else
                                        head1 :: merge (head2::tail)
        | _ -> span
