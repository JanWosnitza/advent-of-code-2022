import LeanExt

namespace Day9

structure Vector where
  x : Int
  y : Int
deriving Inhabited, Repr, BEq

instance : Add Vector where
  add | ⟨x₁, y₁⟩, ⟨x₂, y₂⟩ => ⟨x₁ + x₂, y₁ + y₂⟩

instance : Sub Vector where
  sub | ⟨x₁, y₁⟩, ⟨x₂, y₂⟩ => ⟨x₁ - x₂, y₁ - y₂⟩

inductive Direction where | Right | Left | Up | Down
deriving Inhabited, Repr

def Direction.parse! : String → Direction
  | "R" => Direction.Right
  | "L" => Direction.Left
  | "U" => Direction.Up
  | "D" => Direction.Down
  | _ => panic! "invalid input"

def Direction.toVector : Direction → Vector
  | Right => ⟨1, 0⟩
  | Left => ⟨-1, 0⟩
  | Up => ⟨0, 1⟩
  | Down => ⟨0, -1⟩

structure Move where
  direction : Direction
  count : Nat
deriving Inhabited, Repr

def Move.parse! (s:String) : Move :=
  match s.splitOn " " with
  | [direction, count] =>
    { direction := Direction.parse! direction
    , count := count.toNat!
    : Move}
  | _ => panic! "invalid input"

def updateKnot (head:Vector) (tail:Vector) : Vector :=
  let ⟨dx, dy⟩ := tail - head
  if (Int.natAbs dx <= 1) && (Int.natAbs dy <= 1) then
    tail
  else
    { x := tail.x - Int.sign dx
    , y := tail.y - Int.sign dy
    }

structure Rope where
  knots : List Vector
deriving Inhabited, Repr

def Rope.zero (knots:Nat) : Rope := {knots := List.replicate knots ⟨0,0⟩}

def Rope.moveHead (rope:Rope) (dir:Direction) : Rope :=
  match rope.knots with
  | [] => {knots := []}
  | head :: tails =>
    let rec loop (head) : List Vector → List Vector
      | [] => []
      | n :: ns =>
        let n₁ := updateKnot head n
        n₁ :: loop n₁ ns
    let head₁ := head + dir.toVector
    {knots := head₁ :: loop head₁ tails}

def HT.moves (rope:Rope) (moves:List Move) : List Rope :=
  moves
  |>.bind (fun m => List.replicate m.count m.direction)
  |>.foldl (fun (rope, poss) dir =>
    let ht₁ := rope.moveHead dir
    (ht₁, ht₁ :: poss)
    ) (rope, [rope])
  |> Prod.snd
  |>.reverse

def partX (knots) (ls:List String) :=
  ls
  |>.map Move.parse!
  |> HT.moves (Rope.zero knots)
  |>.map (List.getLast! ∘ Rope.knots)
  |>.eraseDups
  |>.length

def part1 (ls:List String) := partX 2 ls

def part2 (ls:List String) := partX 10 ls

def testInput₁ := 
  [ "R 4"
  , "U 4"
  , "L 3"
  , "D 1"
  , "R 4"
  , "D 1"
  , "L 5"
  , "R 2"
  ]
#eval 13 <== part1 testInput₁
#eval 1 <== part2 testInput₁

def testInput₂ :=
  [ "R 5"
  , "U 8"
  , "L 8"
  , "D 3"
  , "R 17"
  , "D 10"
  , "L 25"
  , "U 20"
  ]
#eval 36 <== part2 testInput₂

/-
inductive Cell where
  | empty
  | set (n:Nat)

structure Grid where
  cells : List (List Cell)

instance : Repr Grid where
  reprPrec grid _ :=
    let cells :=
      grid.cells
      |>.map (fun row =>
        row
        |>.map (fun | Cell.empty => '.' | Cell.set n => Char.ofNat ('0'.toNat + n))
        |>.asString
      )
      |>.intersperse "\n"
      |> String.join
    s!"{cells}\n"
    |> Std.Format.text
    |> Std.Format.fill

def x (ls:List String) :=
  ls
  |>.map Move.parse!
  |> HT.moves (Rope.zero 10)
  |>.map (fun rope =>
    {cells :=
      List.range 5
      |>.reverse
      |>.map (fun y =>
        List.range 5
        |>.map (fun x => ⟨x, y⟩)
        |>.map (fun pos =>
          match rope.knots.findIndex? (·==pos) with
          | none => Cell.empty
          | some x => Cell.set x -- Char.ofNat ('0'.toNat + x)
        )
        --|>.toString
      )
    : Grid}
  )

#eval x testInput₁
-/

end Day9