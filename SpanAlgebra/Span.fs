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

    let print temporal =
        temporal |> List.iter (fun x -> printfn "  [%A, %A[ = %A" x.Start x.Stop x.Value)

    let clamp temporal start stop =
        let rec clamp (temporal: Temporal<'a, 't>) =
            match temporal with
            | head :: tail -> if head.Stop <= start then clamp tail
                              elif stop < head.Start then []
                              else
                                  let headClamp = { head with Start = max head.Start start
                                                              Stop = min head.Stop stop }
                                  headClamp :: clamp tail
            | _ -> []
        clamp temporal

    // compute the union of two span lists
    // the result is not necessarily optimal - see merge
    let rec combine<'a, 'b, 'c, 't when 'a : equality 
                                   and 'b : equality 
                                   and 'c : equality 
                                   and 't : comparison> (comb: 'a -> 'b -> 'c) (temporal1: Temporal<'a, 't>) (temporal2: Temporal<'b, 't>) : Temporal<'c, 't> =
        let rec runion temporal1 temporal2 =
            match temporal1, temporal2 with
            | head1::tail1, head2::tail2 -> // invariant: head1.Start <= head2.Start
                                            if head2.Start < head1.Start then
                                                combine (fun x y -> comb y x) temporal2 temporal1

                                            // no overlap
                                            // head1: [-------[
                                            // head2:               [------[
                                            elif head1.Stop <= head2.Start then
                                                runion tail1 temporal2

                                            // overlap
                                            // head1: [--------[
                                            // head2: [--------[
                                            // head2:   [---------[
                                            // head2: [------[
                                            // head2:   [----[
                                            else
                                                let headUnion = { Start = max head1.Start head2.Start
                                                                  Stop = min head1.Stop head2.Stop
                                                                  Value = comb head1.Value head2.Value }
                                                let tail1 = if head1.Stop = headUnion.Stop then tail1
                                                            else { head1 with Stop = headUnion.Stop} :: tail1
                                                let tail2 = if head2.Stop = headUnion.Stop then tail2
                                                            else { head2 with Stop = headUnion.Stop} :: tail2
                                                headUnion :: runion tail1 tail2
            | _ -> []
        runion temporal1 temporal2

    let coverage start stop temporal =
        let rec validate temporal curr =
            match temporal with
            | [ last ] -> last.Start = curr && last.Stop = stop
            | head :: tail -> if head.Start <> curr then false
                              else validate tail head.Stop
            | _ -> false
        validate temporal start

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
            if prevStop <= prevStart then false
            else
                match spans with
                | [] -> true
                | ({ Span.Start = start; Span.Stop = _; Span.Value = _ } as head) :: tail -> if start < prevStop then false
                                                                                             else validate spans.Head tail

        match spans with
        | [] -> true
        | head:: tail -> validate head tail
