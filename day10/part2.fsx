#load "../utils.fs"

let endChar = char 0 
let completion (acc:char list) (c:char) =
  if Seq.length acc = 0
  then c :: acc
  else
    let c0 = Seq.head acc
    match (c0, c) with
    | ('(', ')') | ('[', ']') | ('{', '}') | ('<', '>') -> List.tail acc
    | (')', _) | (']', _) | ('}', _) | ('>', _) -> [c0]
    | (_, ')') | (_, ']') | (_, '}') | (_, '>') -> [c]
    | _ -> c :: acc

let score = function
| '(' -> 1UL
| '[' -> 2UL
| '{' -> 3UL
| '<' -> 4UL
| _ -> 0UL

let itemsToExclude = ['('; '['; '{'; '<']
let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.map (Seq.fold completion [] >> Seq.map score)
  |> Seq.filter (Seq.contains 0UL >> not)
  |> Seq.map (Seq.reduce (fun acc x -> 5UL * acc + x))
  |> Seq.sort
  |> (fun s -> Seq.item ((Seq.length s) / 2) s)
    
printfn "Solution: %A" result
