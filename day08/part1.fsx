#load "../utils.fs"

let split c (x:string) = x.Split [|c|]

let count acc (l, c) =
  match l with
  | 2 | 3 | 4 | 7 -> acc + c
  | _ -> acc

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.map (split '|' >> Seq.last >> split ' ')
  |> Seq.concat
  |> Seq.countBy Seq.length
  |> Seq.fold count 0  
    
printfn "Solution: %A" result