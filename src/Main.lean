import Days

def Solution.print (solution:Solution) := do
  IO.println s!"Day{solution.day}.part{solution.part}"

  let arrLines ← IO.FS.lines s!"input/day{solution.day}.txt"
  let lines := arrLines.foldr List.cons []

  let startTime ←  IO.monoMsNow
  let result := solution.f lines
  let endTime ←  IO.monoMsNow

  let strResult := solution.toString.toString result
  IO.println strResult
  IO.println s!"time = {endTime - startTime}ms"

def test day part := days |> List.find? (fun (s:Solution) => s.day = day && s.part = part)

def main : List String → IO Unit
  | [] => do
    List.getLast days (by simp [days])
    |>.print
  | ["all"] => do
    for day in days do
      day.print
  | [dayS, partS] => do
    let day ← dayS.toNat?.toIO (IO.Error.userError "day not a number")
    let part ← partS.toNat?.toIO (IO.Error.userError "part not a number")
    days
    |>.find? (fun s => s.day = day && s.part = part)
    |> fun
      | some solution => do solution.print
      | none => do throw (IO.Error.userError "invalid day and part combination")

    /- sadly doesn't work, because `Solution`'s type universe is 1
    let solution ←
      days
      |>.find? (fun s => s.day = day && s.part = part)
      |>.toIO (IO.Error.userError "invalid day and part combination")
    solution.print
    ---/

  | _ => do throw (IO.Error.userError "invalid commandline")
