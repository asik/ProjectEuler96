open System
open System.Diagnostics
open System.IO

/// Read the file in, creating an array of integers corresponding to the initial position
/// for each sudoku.
let sudokus =
    let lines = File.ReadAllLines("sudoku.txt")
    [| for i in 0 .. (lines.Length / 10 - 1) ->
          let start = 10 * i + 1
          [| for lineNum in start .. start + 8 ->
                 lines.[lineNum] |> Seq.map(fun c -> Int32.Parse(string(c))) |> Seq.toArray |]
          |> Array.concat |]

/// Get the Project Euler specific number (i.e. number formed of the first three digits of the solution)
let getPENumber (solution:int[]) =
    solution.[0] * 100 + solution.[1] * 10 + solution.[2]

[<EntryPoint>]
let main argv =  
    let sw = Stopwatch.StartNew()   
    // To solve Project Euler #95, we compute the desired number for each sudoku solution
    // and print out the sum of these numbers.
    // On my machine (i5-4670K), using Array.Parallel results in a 2X speedup. 
    // It still takes 5 seconds which isn't great - some implementations do this in less than a second
    // on much weaker CPUs - but it's way below the 60 seconds Project Euler constraint.   
    let numbers = sudokus |> Array.Parallel.mapi (fun i sudoku ->
        Console.WriteLine( "Solving sudoku {0} of {1}", (i + 1), sudokus.Length)
        let solution = SudokuSolver.solve sudoku
        getPENumber solution)

    let elapsed = sw.Elapsed
    printfn "\nAnswer = %d, time elapsed = %A" (Array.sum numbers) elapsed
    0