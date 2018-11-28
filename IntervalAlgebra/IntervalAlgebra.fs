module IntervalAlgebra
open System


type Interval<'t, 'v when 't : comparison> = 
    { Start : 't
      Stop : 't
      Value : 'v}

let createInterval start stop value = { Start = start; Stop = stop; Value = value }

let intersect<'t,'v when 't : comparison> (int1 : Interval<'t, 'v> list) (int2 : Interval<'t, 'v> list) (comb : 'v -> 'v -> 'v) =
    let rec intersect (int1 : Interval<'t, 'v> list) (int2 : Interval<'t, 'v> list) =
        match int1, int2 with
        | head1::tail1, head2::tail2 -> if (head2.Start < head1.Start) || (head1.Start = head2.Start && head2.Stop < head1.Stop) then intersect int2 int1
                                        elif head1.Stop <= head2.Start then intersect tail1 int2
                                        else
                                            let intersection = { Start = max head1.Start head2.Start
                                                                 Stop = min head1.Stop head2.Stop
                                                                 Value = comb head1.Value head2.Value }
                                            intersection :: if head1.Stop = intersection.Stop && head2.Stop = intersection.Stop then
                                                                intersect tail1 tail2
                                                            elif head1.Stop = intersection.Stop then
                                                                let remainder = { head2 with Start = intersection.Stop }
                                                                intersect tail1 (remainder::tail2)
                                                            else
                                                                let remainder = { head1 with Start = intersection.Stop }
                                                                intersect (remainder::tail1) tail2
        | _ -> []
    intersect int1 int2

let union<'t,'v when 't : comparison> (int1 : Interval<'t, 'v> list) (int2 : Interval<'t, 'v> list) (comb : 'v -> 'v -> 'v) =
    let rec union (int1 : Interval<'t, 'v> list) (int2 : Interval<'t, 'v> list) =
        match int1, int2 with
        | head1::tail1, head2::tail2 -> if (head2.Start < head1.Start) || (head1.Start = head2.Start && head1.Stop < head2.Stop) then union int2 int1
                                        elif head1.Stop <= head2.Start then head1 :: union tail1 int2
                                        elif head1.Start < head2.Start then
                                            let head = { head1 with Stop = head2.Start }
                                            let remainder = { head1 with Start = head2.Start }
                                            head :: union (remainder::tail1) int2
                                        elif head1.Start = head2.Start && head1.Stop = head2.Stop then
                                            let head = { head1 with Stop = head2.Stop; Value = comb head1.Value head2.Value }
                                            head :: union tail1 tail2 
                                        else
                                            let head = { head1 with Stop = head2.Stop; Value = comb head1.Value head2.Value }
                                            let remainder = { head1 with Start = head2.Stop }
                                            head :: union (remainder::tail1) tail2
        | _ -> int1 @ int2
    union int1 int2                                

let rec combine<'t,'v when 't : comparison> (elt : Interval<'t, 'v>) (int : Interval<'t, 'v> list) (comb : 'v -> 'v -> 'v) =
    union int [elt] comb
