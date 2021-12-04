#load "../utils.fs"

let input = fsi.CommandLineArgs[1]
let result = 
    Utils.readLines input
    |> Seq.map int
    |> Seq.pairwise
    |> Seq.map (fun (x, y) -> if y > x then 1 else 0)
    |> Seq.sum
    
printfn "Solution: %A" result
