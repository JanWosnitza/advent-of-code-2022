import LeanExt

namespace Day5

section Crate
  structure Crate where c : Char
  deriving Inhabited, Repr

  def Crate.parse? : Char → Option Crate
    | ' ' => none
    | c => some {c := c}

  def Crate.unparse : Option Crate → Char
    | none => ' '
    | some crate => crate.c
end Crate

section Stack
  structure Stack where crates : List Crate
  deriving Inhabited, Repr

  def Stack.take! (stack:Stack) (n:Nat) : List Crate × Stack :=
    ( stack.crates.take n
    , {crates:=stack.crates.drop n}
    )

  def Stack.put (crates:List Crate) (stack:Stack) : Stack :=
    {crates := crates ++ stack.crates}

  def Stack.move! (n:Nat) (stacks:Stack × Stack) : Stack × Stack :=
    let (crates, source₁) := stacks.1.take! n
    (source₁, stacks.2.put crates)

  def Stack.top? (stack:Stack) : Option Crate := stack.crates.head?
end Stack

inductive StackName where | stackName : String → StackName
deriving Inhabited, Repr, BEq

section Stacks
  abbrev Stacks := List (StackName × Stack)

  def Stacks.update (name₁) (name₂) (updater:(Stack × Stack) → (Stack × Stack)) (stacks:Stacks) :=
    let sourceIdx := stacks.findIndex? (fun (name, _) => name == name₁) |>.get!
    let targetIdx := stacks.findIndex? (fun (name, _) => name == name₂) |>.get!

    let source := stacks[sourceIdx]!.2
    let target := stacks[targetIdx]!.2

    let (source, target) := updater (source, target)

    stacks
    |>.set sourceIdx (name₁, source)
    |>.set targetIdx (name₂, target)

  def Stacks.parse! (ls:List String) : Stacks :=
    ls
    |>.reverse
    |>.map String.toList
    |>.transpose!
    |>.enum
    |>.filterMap (fun (n, cs) => if n % 4 == 1 then some cs else none)
    |>.map (fun
      | [] => panic! "invalid input"
      | name :: crates => 
        ( StackName.stackName s!"{name}"
        , {crates := crates.reverse.filterMap Crate.parse?}
        )
    )
end Stacks

section Move
  structure Move where
    count : Nat
    source : StackName
    target : StackName
  deriving Inhabited, Repr

  def Move.parse! (s:String) : Move :=
    match s.splitOn " " with
    | ["move", count, "from", sourceStack, "to", targetStack] =>
      { count := count.toNat!
      , source := StackName.stackName sourceStack
      , target := StackName.stackName targetStack
      }
    | _ => panic! "invalid input"

  def Move.run1! (stacks:Stacks) (move:Move) : Stacks :=
    stacks
    |> Stacks.update move.source move.target (fun (source, target) =>
      List.range move.count
      |> List.foldl (fun (source, target) _ => Stack.move! 1 (source, target)) (source, target)
    )

  def Move.run2! (stacks:Stacks) (move:Move) : Stacks :=
    stacks.update move.source move.target  (Stack.move! move.count)
end Move

def partX (moveRun) (ls:List String) :=
  match ls.splitOn "" with
  | [sStacks, sMoves] =>
    let stacks := Stacks.parse! sStacks
    let moves := sMoves |> List.map Move.parse!

    moves
    |>.foldl moveRun stacks
    |>.map (Stack.top? ∘ Prod.snd)
    |>.map Crate.unparse
    |>.asString
  | _ => "[invalid input]"

def part1 (ls:List String) :=
  partX Move.run1! ls

def part2 (ls:List String) :=
  partX Move.run2! ls

def testInput :=
  [ "    [D]    "
  , "[N] [C]    "
  , "[Z] [M] [P]"
  , " 1   2   3 "
  , ""
  , "move 1 from 2 to 1"
  , "move 3 from 1 to 3"
  , "move 2 from 2 to 1"
  , "move 1 from 1 to 2"
  ]
#eval "CMZ" <== part1 testInput
#eval "MCD" <== part2 testInput

end Day5