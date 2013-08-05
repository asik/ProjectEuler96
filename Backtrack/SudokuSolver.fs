﻿module SudokuSolver

// The general approach taken here is a straightforward application of the backtrack algorithm.
// This is a very efficient optimisation on the brute-force algorithm, where we proceed
// by adding one element at a time and backing out early (backtracking) as soon as an invalid
// solution is encountered.

// Our backtrack implementation is a direct F# translation of its pseudo-code description
// on Wikipedia: http://en.wikipedia.org/wiki/Backtracking#Pseudocode
// We omit the output function and make root a value instead of a function for simplicity.

//  procedure bt(c)
//   if reject(P,c) then return
//   if accept(P,c) then output(P,c)
//   s ← first(P,c)
//   while s ≠ Λ do
//     bt(s)
//     s ← next(P,s)

let backtrack root reject accept first next =
    let rec bt c = seq {
        if not (reject c) then
            if accept c then yield c
            else
                let f = first c
                for s in f |> Seq.unfold (fun s -> if s = None then None else Some(s, (next s.Value))) do
                    yield! bt(s.Value)
    }
            
    bt root

// Our implementation is independent of board size, but since this is sudoku we'll take the standard size 9
let boardDimension = 9
let groupDimension = 3

/// Translate two-dimensional coordinates into one-dimensional coordinates.
let getIndex x y = y * boardDimension + x

/// Whether every square of the board has a valid (i.e. non-zero) value written to
let boardComplete board =
    not (board |> Array.exists (fun elem -> elem = 0))

/// Whether the collection of squares refered by coords contains the same element twice or more
/// Interesting optimisation here: instead of naively comparing each element with all the others,
/// we map every value to a bool array indicating whether it's been encountered before. This makes
/// the algorithm O(N) instead of O(N^2) and much faster.
let containsDuplicates coords (board:int[]) =
    let found = Array.zeroCreate (boardDimension + 1)
    coords |> Array.exists(fun index ->
        let elem = board.[index]
        let exists = elem > 0 && found.[elem]
        found.[elem] <- true
        exists)

/// An array of arrays of coordinates each refencering all squares of a line
let lineCoords = [| 
    for y in 0 .. boardDimension - 1 ->
        [| for x in 0 .. boardDimension - 1 ->
            getIndex x y |] |]

/// An array of arrays of coordinates each refencering all squares of a column
let columnCoords = [| 
    for x in 0 .. boardDimension - 1 ->
        [| for y in 0 .. boardDimension - 1 ->
            getIndex x y |] |]

/// An array of arrays of coordinates each refencering all squares of a group
let groupCoords = [|
    let gap = boardDimension / groupDimension
    for i in 0 .. gap .. boardDimension - 1 do
        for j in 0 .. gap .. boardDimension - 1 ->
            [|  for x in i .. i + groupDimension - 1 do
                    for y in j .. j + groupDimension - 1 ->
                        getIndex x y |]|]

/// Whether there are no duplicates alongside the coordinate arrays
let hasNoDuplicatesIn board coords =
    not (coords |> Array.exists(fun line -> containsDuplicates line board))

/// Whether the sudoku board respects all constraints, i.e. is a valid position
/// but not necessarily a solution
let satisfiesConstraints board =
    hasNoDuplicatesIn board lineCoords &&
    hasNoDuplicatesIn board columnCoords &&
    hasNoDuplicatesIn board groupCoords

/// One of the basic backtrack functions. The position is rejected if it's invalid.
let reject (board,_) =
    not (satisfiesConstraints board)

/// One of the basic backtrack functions. The position is accepted it it's valid and complete,
/// i.e. it's a solution of the problem.
let accept (board,_) =
    boardComplete board &&
    satisfiesConstraints board                            

/// One of the basic backtrack functions. The "first" of a position is the position with one of its
/// empty elements written a valid value to. If there are no empty elements (board is complete),
/// the "first" is None.
let first ((board:int[]),_) =
    let empty = board |> Array.tryFindIndex(fun elem -> elem = 0)
    if empty = None then None
    else
        let newBoard = Array.copy board
        let index = empty.Value
        newBoard.[index] <- 1
        Some(newBoard, index)

/// One of the basic backtrack functions. The "next" of a position is the same position with the current
/// square incremented to the next valid value. If it's already at its maximum value, the "next" is None.
let next ((board:int[]), index) =
    let n = board.[index]
    if n = boardDimension then None
    else
        let newBoard = Array.copy board
        newBoard.[index] <- n + 1
        Some(newBoard, index)

/// One of the basic backtrack functions. The "root" of a position is simply this position. The index 
/// chosen here is simply there to satisfies the type system and serves no purpose.
let root board = (board, 0)

/// "The sudoku solution" of a board is the board of the first solution.
let solve board =
    let rv,_ = Seq.head (backtrack (root board) reject accept first next)
    rv
   