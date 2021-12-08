#load "../utils.fs"

let split (x:string) = x.Split [|','|] 

let distance s m =
  s |> Seq.map (fun x -> abs (x - m))
    |> Seq.sum

let median s =
  let len = Seq.length s
  if len % 2 = 0
  then ((Seq.item (len / 2 - 1) s) + (Seq.item (len / 2) s)) / 2
  else Seq.item ((len + 1) / 2 - 1) s

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.map (split >> Seq.map int)
  |> Seq.concat
  |> Seq.sort
  |> (fun s -> distance s (median s))
    
printfn "Solution: %A" result
