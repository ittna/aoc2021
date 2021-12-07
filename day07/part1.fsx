#load "../utils.fs"

let split (x:string) = x.Split [|','|] 

let distance x s =
  s |> Seq.map (fun y -> int (sqrt (double (pown (x - y) 2))))
    |> Seq.sum    

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.map (split >> Seq.map int)
  |> Seq.concat
  |> (fun s -> seq { for x in (Seq.min s) .. (Seq.max s) -> distance x s })
  |> Seq.min
    
printfn "Solution: %A" result
