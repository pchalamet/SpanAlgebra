module SegmentAlgebra
open System


type Segment<'t, 'v when 't : comparison> = 
    { Start : 't
      Stop : 't
      Value : 'v}

let createSegment start stop value = { Start = start; Stop = stop; Value = value }

let rec intersect<'t,'v when 't : comparison> (seg1 : Segment<'t, 'v> list) (seg2 : Segment<'t, 'v> list) (comb : 'v -> 'v -> 'v) =
    match seg1, seg2 with
    | head1 :: tail1, head2 :: tail2 -> if head1.Start > head2.Start then intersect seg2 seg1 comb
                                        else
                                            // from here we always have head1.Start <= head2.Start
                                            if head1.Stop <= head2.Start then intersect tail1 seg2 comb
                                            else
                                                let start = min head1.Stop head2.Start
                                                let stop = min head1.Stop head2.Stop
                                                let value = comb head1.Value head2.Value
                                                let seg = createSegment start stop value
                                                seg :: intersect tail1 seg2 comb
    | _, _ -> []

let rec union<'t,'v when 't : comparison> (seg1 : Segment<'t, 'v> list) (seg2 : Segment<'t, 'v> list) (comb : 'v -> 'v -> 'v) =
    match seg1, seg2 with
    | head1 :: tail1, head2 :: tail2 -> if head1.Start > head2.Start then union seg2 seg1 comb
                                        else
                                            // from here we always have head1.Start <= head2.Start
                                            if head1.Stop < head2.Start then head1 :: union tail1 seg2 comb
                                            else
                                                let start = head1.Start
                                                let stop = max head1.Stop head2.Stop
                                                let value = comb head1.Value head2.Value
                                                let seg = createSegment start stop value
                                                union tail1 (seg::tail2) comb
    | _, _ -> seg1 @ seg2

let rec combine<'t,'v when 't : comparison> (elt : Segment<'t, 'v>) (seg : Segment<'t, 'v> list) (comb : 'v -> 'v -> 'v) =
    union seg [elt] comb
