#load "../utils.fs"

let split (x:string) = x.Split [|' '|]
let parse (x:string[]) = (x[0], int x[1])
let move (x, y, aim) (cmd, v) = 
  match cmd with
  | "forward" -> (x + v, y + aim * v, aim)
  | "down" -> (x, y, aim + v)
  | "up" -> (x, y, aim - v)
  | _ -> failwith $"Invalid command: {cmd}"

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.map split
  |> Seq.map parse
  |> Seq.fold move (0, 0, 0)
  |> (fun (x, y, _) -> x * y) 

printfn "Solution: %A" result