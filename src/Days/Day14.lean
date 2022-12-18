import Std.Data.HashMap
import LeanExt
open Std

namespace Day14

structure Pos where
  x : Int
  y : Nat
deriving Inhabited, Repr, BEq, Hashable

instance : Add Pos where add a b := ⟨a.x + b.x, a.y + b.y⟩

structure Path where
  points : List Pos
deriving Inhabited, Repr

def Path.positions (p:Path) : List Pos :=
  loop p.points
where
  loop : List Pos → List Pos
  | [] => []
  | e :: [] => [e]
  | s :: e :: ps =>
    let my_ps :=
      if s.x == e.x then
        let x := s.x
        let sy := min s.y e.y
        let ey := max s.y e.y
        List.range (ey - sy + 1)
        |>.map (fun y => {x := x, y := sy + y})
      else if s.y == e.y then
        let sx := min s.x e.x
        let ex := max s.x e.x
        let y := s.y
        List.range (ex - sx + 1).toNat
        |>.map (fun (x:Nat) => {x := sx + x, y := y})
      else
        panic! "invalid followup point"
    my_ps ++ loop (e :: ps)

inductive Cell where | wall | sand
deriving Inhabited, Repr

structure Sands where
  cells : HashMap Pos Cell
  maxY : Nat
deriving Inhabited

def Sands.ofPositions (ps:List Pos) : Sands :=
  { cells :=
      ps
      |>.foldl (fun h p => h.insert p Cell.wall) default
    maxY := ps.map Pos.y |>.maximum? |>.getD 0
  }

def Sands.addPositions (ps:List Pos) (s:Sands) : Sands :=
  { cells :=
      ps
      |>.foldl (fun h p => h.insert p Cell.wall) s.cells
    maxY :=
      max s.maxY (ps.map Pos.y |>.maximum? |>.getD s.maxY)
  }

-- debugging obvious errors 🤦
def Sands.toString (s:Sands) (minX maxX:Int) (minY maxY:Nat) :=
  List.range (maxY - minY + 1)
  |>.map (fun dy =>
    let y := minY + dy
    List.range (maxX - minX + 1).toNat
    |>.map (fun dx =>
      let x := minX + Int.ofNat dx
      match s.cells.find? {x:=x, y := y:Pos} with
      | some Cell.wall => '#'
      | some Cell.sand => 'o'
      | none => ' '
    )
    |>.asString
  )
  |>.intersperse "\n"
  |> String.join

def Sands.test (s:Sands) (p:Pos) : Bool := s.cells.contains p

inductive Sands.DropSand where
  | bottomReached
  | fixPoint
  | next (p:Pos)
deriving Inhabited, Repr

def Sands.dropSand (s:Sands) (p:Pos) :=
  let p := p + ⟨0, 1⟩
  if p.y > s.maxY then
    DropSand.bottomReached
  else
  if not (s.test (p + ⟨ 0, 0⟩)) then
    DropSand.next (p + ⟨ 0, 0⟩)
  else
  if not (s.test (p + ⟨-1, 0⟩)) then
    DropSand.next (p + ⟨-1, 0⟩)
  else
  if not (s.test (p + ⟨ 1, 0⟩)) then
    DropSand.next (p + ⟨ 1, 0⟩)
  else
  DropSand.fixPoint

inductive Sands.AddSand where
  | occupied
  | added (s:Sands)
  | bottomReached
deriving Inhabited

instance : Repr Sands.AddSand where
  reprPrec x _ :=
    match x with
    | Sands.AddSand.occupied => "occupied"
    | Sands.AddSand.added _ => "added"
    | Sands.AddSand.bottomReached => "bottomReached"

partial def Sands.addSand (s:Sands) (p:Pos) : AddSand :=
  if not (s.test p)
  then loop s p
  else AddSand.occupied
where
  loop (s:Sands) (p:Pos) : AddSand :=
    match s.dropSand p with
    | DropSand.bottomReached => AddSand.bottomReached
    | DropSand.fixPoint => AddSand.added {s with cells := s.cells.insert p Cell.sand}
    | DropSand.next p => loop s p

namespace Parse
  def pos! (s:String) : Pos :=
    match s.splitOn "," with
    | [sx, sy] =>
      { x := sx.toNat!
      , y := sy.toNat!
      }
    | _ => panic! "invalid pos"

  def path! (s:String) : Path :=
    { points :=
      s.splitOn " -> "
      |>.map pos!
    }

  def input! (ls:List String) : List Path :=
    ls
    |>.map path!
end Parse

partial def addMaxSand (s:Sands) (n:Nat) :=
  match s.addSand ⟨500,0⟩ with
  | Sands.AddSand.added s => addMaxSand s (n + 1)
  | _ => n

def part1 (ls:List String) :=
  let s :=
    ls
    |> Parse.input!
    |>.bind (·.positions)
    |> Sands.ofPositions

  addMaxSand s 0

partial def part2 (ls:List String) :=
  let s :=
    ls
    |> Parse.input!
    |>.bind (·.positions)
    |> Sands.ofPositions
  
  let s :=
    -- add just enough floor to hold all of the stacked sand
    let floorPositions :=
      let floorY := s.maxY + 2
      let minX := Int.ofNat (500 - (floorY - 0))
      List.range (floorY * 2 + 1)
      |> List.map (fun (x:Nat) => {x:= minX + x, y := floorY :Pos})
    s.addPositions floorPositions

  addMaxSand s 0
  --(addMaxSand s 0).toString 480 530 0 12

def testInput :=
  [ "498,4 -> 498,6 -> 496,6"
  , "503,4 -> 502,4 -> 502,9 -> 494,9"
  ]
#eval 24 <== part1 testInput
#eval 93 <== part2 testInput

end Day14