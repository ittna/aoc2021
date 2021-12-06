#load "../utils.fs"

open System.Collections.Generic

let split (x:string) = x.Split [|','|]

let memoize f =
  let dict = Dictionary<_, _>();
  fun a1 a2 ->
    let c = (a1, a2)
    let exist, value = dict.TryGetValue c
    match exist with
    | true -> value
    | _ -> 
      let value = f a1 a2
      dict.Add(c, value)
      value

let mutable memoFish = (fun r x -> 0UL)
let rec fish rounds (x:uint64) =
  if x >= rounds && rounds <= 7UL
  then 1UL
  else
    let step = min rounds 7UL
    let r = rounds - step
    if x < step
    then (memoFish r ((x - step + 7UL) % 7UL)) + (memoFish r (x - step + 9UL))
    else memoFish r (x - step)
memoFish <- memoize fish

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.map (split >> Seq.map uint64)
  |> Seq.concat
  |> Seq.map (fish 256UL)
  |> Seq.sum
    
printfn "Solution: %A" result