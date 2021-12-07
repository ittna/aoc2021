#load "../utils.fs"

let split (x:string) = x.Split [|','|] 

let sum n = Seq.initInfinite id |> Seq.take (n + 1) |> Seq.sum
let distance x s =
  s |> Seq.map (fun y -> sum (abs (x - y)))
    |> Seq.sum    

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.map (split >> Seq.map int)
  |> Seq.concat
  |> (fun s -> seq { for x in (Seq.min s) .. (Seq.max s) -> distance x s })
  |> Seq.min
    
printfn "Solution: %A" result
