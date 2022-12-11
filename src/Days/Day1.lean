import LeanExt

namespace Day1

abbrev Calories := Nat

structure Elve where
  calories : List Calories
  deriving Repr

def Elve.totalCalories (elve:Elve) := elve.calories.sum

namespace Parse
  def calories! (s:String) := s.toNat!

  def elve! (ls:List String) := 
    {calories := ls |> List.map calories! : Elve}

  def elveList! (lines:List String) :=
    lines.splitOn ""
    |> List.map elve!
end Parse

def part1 (input:List String) :=
  input
  |> Parse.elveList!
  |> List.map Elve.totalCalories
  |> List.maximum?
  |> Option.get!

def part2 (input:List String) :=
  input
  |> Parse.elveList!
  |> List.map Elve.totalCalories
  |> List.sort
  |> List.reverse
  |> List.take 3
  |> List.sum

def testInput :=
  [ "1000"
  , "2000"
  , "3000"
  , ""
  , "4000"
  , ""
  , "5000"
  , "6000"
  , ""
  , "7000"
  , "8000"
  , "9000"
  , ""
  , "10000"
  ]
#eval 24000 <== part1 testInput
#eval 45000 <== part2 testInput

end Day1