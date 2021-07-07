namespace SpanAlgebra

type Span<'v, 't when 'v : equality  and 't : comparison> = 
    { Value : 'v
      Start : 't
      Stop : 't }

type Temporal<'v, 't when 'v : equality  and 't : comparison> = Span<'v, 't> list


module Span =

    // helper function to create a valid span
    // if you do not use it you are at your own risks :-)
    let create value start stop = 
        if start < stop then { Value = value ; Start = start; Stop = stop }
        else failwithf "start must be strictly lower than stop"

    // compute the intersection of two spans lists
    // the result is not necessarily optimal - see merge
    let intersect comb span1 span2 =
        let rec intersect span1 span2 =
            match span1, span2 with
            | head1::tail1, head2::tail2 -> if (head2.Start < head1.Start) || (head1.Start = head2.Start && head2.Stop < head1.Stop) then intersect span2 span1
                                            elif head1.Stop <= head2.Start then intersect tail1 span2
                                            else
                                                let head = { Value = comb head1.Value head2.Value 
                                                             Start = max head1.Start head2.Start
                                                             Stop = min head1.Stop head2.Stop }
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
    let rec union<'a, 'b, 'c, 't when 'a : equality 
                                 and 'b : equality 
                                 and 'c : equality 
                                 and 't : comparison> (comb: 'a option -> 'b option -> 'c) (span1: Temporal<'a, 't>) (span2: Temporal<'b, 't>) : Temporal<'c, 't> =
        let rec runion span1 span2 =
            match span1, span2 with
            | head1::tail1, head2::tail2 -> // span1 must be before span2
                                            if (head2.Start < head1.Start) || (head1.Start = head2.Start && head1.Stop < head2.Stop) then
                                                union (fun x y -> comb y x) span2 span1
                                            // span1 does not overlap with span2
                                            elif head1.Stop <= head2.Start then
                                                let newHead = { Start = head1.Start
                                                                Stop = head1.Stop
                                                                Value = comb (Some head1.Value) None }
                                                newHead :: runion tail1 span2
                                            // span1 overlaps span2
                                            elif head1.Start < head2.Start then
                                                let newHead = { Start = head1.Start
                                                                Stop = head2.Start
                                                                Value = comb (Some head1.Value) None }
                                                let remainder = { head1 with Start = head2.Start }
                                                newHead :: runion (remainder::tail1) span2
                                            else
                                                let newHead = { Start = head1.Start
                                                                Stop = head2.Stop
                                                                Value = comb (Some head1.Value) (Some head2.Value) }                                        
                                                if head1.Start = head2.Start && head1.Stop = head2.Stop then
                                                    newHead :: runion tail1 tail2 
                                                else
                                                    let remainder = { head1 with Start = head2.Stop }
                                                    newHead :: runion (remainder::tail1) tail2
            | span1, [] -> span1 |> List.map (fun x -> { Start = x.Start
                                                         Stop = x.Stop
                                                         Value = comb (Some x.Value) None})
            | [], span2 -> span2 |> List.map (fun x -> { Start = x.Start
                                                         Stop = x.Stop
                                                         Value = comb None (Some x.Value) })
        runion span1 span2

    // merge adjacent spans in the list if the value is the same
    // typically intersect and union do not produce optimal result (for code simplicity)
    let rec merge spans =
        match spans with
        | head1 :: head2 :: tail -> if head1.Stop = head2.Start && head1.Value = head2.Value then
                                        merge ({ head1 with Stop = head2.Stop } :: merge tail)
                                    else
                                        head1 :: merge (head2::tail)
        | _ -> spans

    // create a single span list
    let singleton value start stop =
        create value start stop |> List.singleton
 
    // check list for correctness
    let validate spans =
        let rec validate ({ Span.Start = prevStart; Span.Stop = prevStop; Span.Value = prevValue } as prevHead) spans =
            if prevStop <= prevStart then failwithf "%A has Start after Stop" prevHead
            match spans with
            | [] -> [prevHead]
            | ({ Span.Start = start; Span.Stop = _; Span.Value = _ } as head) :: tail -> if start < prevStop then failwithf "%A starts before %A" head prevHead
                                                                                         prevHead :: validate spans.Head tail

        match spans with
        | [] -> []
        | head:: tail -> validate head tail

