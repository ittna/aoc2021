#load "../utils.fs"

let split (x:string) = x.Split [|','|]

let rec fish rounds (x:uint64) =
  if x >= rounds && rounds <= 7UL
  then 1UL
  else
    let step = min rounds 7UL
    let r = rounds - step
    if x < step
    then (fish r ((x - step + 7UL) % 7UL)) + (fish r (x - step + 9UL))
    else fish r (x - step)

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.map (split >> Seq.map uint64)
  |> Seq.concat
  |> Seq.map (fish 80UL)
  |> Seq.sum
    
printfn "Solution: %A" result