import LeanExt

namespace Day2

inductive RPS where | rock | paper | scissors
deriving Inhabited

def RPS.points : RPS → Nat
  | rock => 1
  | paper => 2
  | scissors => 3

inductive Result where | lose | draw | win
deriving Inhabited

open RPS in
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

open RPS in open Result in
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

namespace Parse
  def rps! : String → RPS
    | "A" | "X" => RPS.rock
    | "B" | "Y" => RPS.paper
    | "C" | "Z" => RPS.scissors
    | _ => panic! "invalid input [they]"

  def result! : String → Result
    | "X" => Result.lose
    | "Y" => Result.draw
    | "Z" => Result.win
    | _ => panic! "invalid input [they]"

  def input! [Inhabited α] [Inhabited β] (fLeft:String → α) (fRight:String → β) (ls:List String) : List (α × β) :=
    ls
    |>.map (fun l => l.split (' ' == ·))
    |>.map (fun ls =>
      match ls with
      | [sLeft, sRight] => (fLeft sLeft, fRight sRight)
      | _ => panic! "invalid input"
    )

  def input1! (ls:List String) := input! rps! rps! ls

  def input2! (ls:List String) := input! rps! result! ls
end Parse

def score (mrs:List (RPS × Result)) :Nat :=
  mrs
  |>.map (fun (me, result) => me.points + result.points)
  |>.sum

def part1 (ls:List String) :=
  ls
  |> Parse.input1!
  |>.map (fun (they, me) =>
      let result := Result.of they me
      (me, result)
  )
  |> score

def part2 (ls:List String) :=
  ls
  |> Parse.input2!
  |>.map (fun (they, result) =>
    let me := result.to they
    (me, result)
  )
  |> score

def testInput :=
  [ "A Y"
  , "B X"
  , "C Z"
  ]
#eval 15 <== part1 testInput
#eval 12 <== part2 testInput

end Day2