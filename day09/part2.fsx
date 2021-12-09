#load "../utils.fs"

let rec fill (x, y) (s:int[][]) =
  let rows = Seq.length s
  let cols = Seq.length s[0]
  if x < 0 || x >= cols || y < 0 || y >= rows || s[y][x] = 9
  then 0
  else 
    s[y][x] <- 9
    (fill (x - 1, y) s) + (fill (x + 1, y) s) + (fill (x, y - 1) s) + (fill (x, y + 1) s) + 1

let findBasins (s:int[][]) =
  let rows = Seq.length s
  let cols = Seq.length s[0]
  seq { for y in 0 .. (rows - 1) do
          for x in 0 .. (cols - 1) do
            let h = s[y][x]
            if (y = 0 || (y > 0 && s[y - 1][x] > h)) &&
                (y = rows - 1 || (y < rows - 1 && s[y + 1][x] > h)) &&
                (x = 0 || (x > 0 && s[y][x - 1] > h)) &&
                (x = cols - 1 || (x < cols - 1 && s[y][x + 1] > h))
            then yield fill (x, y) s
  }

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.map (Seq.map (int >> (+) -48) >> Seq.toArray)
  |> Seq.toArray
  |> findBasins
  |> Seq.sortDescending
  |> Seq.take 3
  |> Seq.reduce (*)
    
printfn "Solution: %A" result
