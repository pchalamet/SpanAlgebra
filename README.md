# Span Algebra

This is a simple F# library to compute intersections and unions on list of spans.

# Operations

## Type
But, what is a span ? It is a type with a value spanning on an interval [start, end[ (ie: start is included but end is not). Also, according to the definition, the interval can't be empty.

Type has containts:
* start and end can be whatever you want until it's comparable (intersection & union)
* value can also be whatever you want until it's equatable (merge)

````
type Span<'t, 'v when 't : comparison and 'v : equality> = 
    { Start : 't
      Stop : 't
      Value : 'v}
````

Several operations are defined:
* new (`Span.create`) - validate parameters and create a new `Span`.
* intersection (`Span.intersect`) - a combinator is applied for resulting spans.
* union (`Span.union`) - a combinator is applied for resulting spans.
* merge (`Span.merge`)
* validate (`Span.validate`) - check for correctness.

Warnings: 
* the list of spans must be ordered with no overlaps before using operations. It's better to build a list using `Span.union`. If you are not sure, use `Span.validate` to check for correctness.
* Span can be constructed directly but it's advised to build them using `Span.create`. Again you can use `Span.validate` to check for correctness.

## Intersection
Compute the intersection of two lists of spans.

````
input1:
  [0, 1] = A
  [4, 5] = A
  [7, 10] = A
  [13, 15] = A
  [17, 20] = A
  [25, 200] = A

input2:
  [2, 3] = B
  [5, 6] = B
  [8, 11] = B
  [12, 14] = B
  [16, 21] = B
  [26, 40] = B
  [45, 50] = B
  [80, 100] = B

result:
  [8, 10] = AnB
  [13, 14] = AnB
  [17, 20] = AnB
  [26, 40] = AnB
  [45, 50] = AnB
  [80, 100] = AnB
````

## Union
Compute the union of two lists of spans.

````
input1:
  [0, 1] = A
  [4, 5] = A
  [7, 10] = A
  [13, 15] = A
  [17, 20] = A
  [25, 200] = A

input2:
  [2, 3] = B
  [5, 6] = B
  [8, 11] = B
  [12, 14] = B
  [16, 21] = B
  [26, 40] = B
  [45, 50] = B
  [80, 100] = B

result:
  [0, 1] = A
  [2, 3] = B
  [4, 5] = A
  [5, 6] = B
  [7, 8] = A
  [8, 10] = AnB
  [10, 11] = B
  [12, 13] = B
  [13, 14] = AnB
  [14, 15] = A
  [16, 17] = B
  [17, 20] = AnB
  [20, 21] = B
  [25, 26] = A
  [26, 40] = AnB
  [40, 45] = A
  [45, 50] = AnB
  [50, 80] = A
  [80, 100] = AnB
  [100, 200] = A
````

## Merge
Merge adjacent spans of a list.

````
input:
  [0, 1] = A
  [2, 3] = A
  [3, 10] = A
  [13, 15] = B
  [15, 20] = B

result:
  [0, 1] = A
  [2, 10] = A
  [13, 20] = B
````