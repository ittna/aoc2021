#load "../utils.fs"

let split (x:string) = x.Split [|'-'|]

let allowed (n:string) p =
  if n = n.ToLower()
  then not (List.contains n p)
  else true

let rec paths p edges =
   let last = Seq.head p
   edges 
   |> Seq.filter (fun (n1, n2) -> (n1 = last && (allowed n2 p)) 
                               || (n2 = last && (allowed n1 p)))
   |> Seq.map (fun (n1, n2) -> if n1 = last then n2 :: p else n1 :: p)
   |> Seq.map (fun p -> if (List.head p) = "end"
                        then seq { p }
                        else paths p edges)
   |> Seq.concat

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.map (split >> (fun x -> (x[0], x[1])))
  |> paths ["start"]
  |> Seq.length
    
printfn "Solution: %A" result
