#load "../utils.fs"

let endChar = char 0 
let rec find i (s:string) =
  if i >= s.Length
  then (endChar, i)
  else
    let c1 = s[i]
    match c1 with
    | ')' | ']' | '}' | '>' -> (c1, i)
    | _ ->
      let (c2, j) = find (i + 1) s
      if j = -1
      then (c2, j)
      else 
        match (c1, c2) with
        | ('(', ')') | ('[', ']') | ('<', '>') | ('{', '}') -> find (j + 1) s
        | _ -> (c2, -1)

let score = function
| ')' -> 3
| ']' -> 57
| '}' -> 1197
| '>' -> 25137
| _ -> 0

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.map (find 0 >> fst)
  |> Seq.map score
  |> Seq.sum
    
printfn "Solution: %A" result
