#load "../utils.fs"

let split (x:string) = x.Split [|' '|]
let parse (x:string[]) = (x[0], int x[1])
let move (x, y) (cmd, v) = 
  match cmd with
  | "forward" -> (x + v, y)
  | "down" -> (x, y + v)
  | "up" -> (x, y - v)
  | _ -> failwith $"Invalid command: {cmd}"

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.map split
  |> Seq.map parse
  |> Seq.fold move (0, 0)
  |> (fun (x, y) -> x * y) 

printfn "Solution: %A" result