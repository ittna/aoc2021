#load "../utils.fs"

let split (x:string) = x.Split [|','|] 

let triangular n = n * (n + 1) / 2
let distance s m =
  s |> Seq.map (fun x -> triangular (abs (x - m)))
    |> Seq.sum

let mean s =
  let avg = s |> Seq.map double |> Seq.average
  (floor avg |> int, ceil avg |> int)

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.map (split >> Seq.map int)
  |> Seq.concat
  |> (fun s -> 
        let (f, c) = mean s
        min (distance s f) (distance s c))
    
printfn "Solution: %A" result
