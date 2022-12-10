import LeanExt

namespace Day2

inductive RPS where | rock | paper | scissors
deriving Inhabited
open RPS

def RPS.parse! : String → RPS
  | "A" | "X" => rock
  | "B" | "Y" => paper
  | "C" | "Z" => scissors
  | _ => panic! "invalid input [they]"

def RPS.points : RPS → Nat
  | rock => 1
  | paper => 2
  | scissors => 3

inductive Result where | lose | draw | win
deriving Inhabited
open Result

def Result.parse! : String → Result
  | "X" => lose
  | "Y" => draw
  | "Z" => win
  | _ => panic! "invalid input [they]"

def Result.of : RPS → RPS → Result
  | rock, scissors => lose
  | rock, rock => draw
  | rock, paper => win

  | paper, rock => lose
  | paper, paper => draw
  | paper, scissors => win

  | scissors, paper => lose
  | scissors, scissors => draw
  | scissors, rock => win

def Result.to : Result → RPS →RPS
  | lose, rock => scissors
  | lose, paper => rock
  | lose, scissors => paper

  | draw, x => x

  | win, rock => paper
  | win, paper => scissors
  | win, scissors => rock

def Result.points : Result → Nat
  | lose => 0
  | draw => 3
  | win => 6

def part1 (lines:List String) :=
  lines
  |>.map (fun l => l.split (' ' == ·))
  |>.map (fun ls =>
    match ls with
    | [theyS, meS] =>
      let they := RPS.parse! theyS
      let me := RPS.parse! meS
      let result := Result.of they me
      (me, result)
    | _ => panic! "invalid input"
  )
  |>.map (fun (me, result) => me.points + result.points)
  |>.sum

def part2 (lines:List String) :=
  lines
  |>.map (fun l => l.split (' ' == ·))
  |>.map (fun ls =>
    match ls with
    | [theyS, meS] =>
      let they := RPS.parse! theyS
      let result := Result.parse! meS
      let me := result.to they
      (me, result)
    | _ => panic! "invalid input"
  )
  |>.map (fun (me, result) => me.points + result.points)
  |>.sum

def testInput :=
  [ "A Y"
  , "B X"
  , "C Z"
  ]
#eval 15 <== part1 testInput
#eval 12 <== part2 testInput

end Day2