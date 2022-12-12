import LeanExt

namespace Day12

abbrev Pos := Int × Int

def Pos.ofNat (xy:Nat × Nat) : Pos := (xy.1, xy.2)

def Pos.add (p:Pos) (xy:Int × Int) : Pos := (p.1 + xy.1, p.2 + xy.2)

def List.enum₂ (ls:List (List α)) :=
  ls
  |>.enum
  |>.map (fun x => x.2.enum.map (fun y => ((x.1, y.1), y.2)))

inductive Cell where
  | s
  | h (height:Nat)
  | e
deriving Inhabited, Repr, BEq

def Cell.height : Cell → Int
  | s => 0
  | h n => n
  | e => 25

structure Heightmap where
  cells: List (List Cell)
deriving Inhabited, Repr

def Heightmap.findPositions (h:Heightmap) (f:Cell → Bool) : List Pos :=
    h.cells
    |> List.enum₂
    |>.bind (List.filterMap (fun (xy, cell) =>
        if f cell then
          some (Pos.ofNat xy)
        else
          none
    ))

def Heightmap.get? (h:Heightmap) (xy:Pos) : Option Cell := do
  if xy.1 < 0 || xy.2 < 0 then none else
  (← h.cells[xy.1.toNat]?)[xy.2.toNat]?

def Heightmap.getNeighbours (h:Heightmap) (xy:Pos) : List (Pos) :=
  match h.get? xy |>.map Cell.height with
  | none => []
  | some heightSource =>
    [(1, 0), (-1, 0), (0, 1), (0, -1)]
    |>.map (Pos.add xy)
    |>.filter (fun xy =>
      match h.get? xy |>.map Cell.height with
      | some heightTarget => heightTarget <= heightSource + 1 -- (height₁ - height₂).natAbs <= 1
      | none => false
    )

structure Flood where
  cells: List (List Bool)
deriving Inhabited, Repr

instance : ToString Flood where
  toString f :=
    f.cells
    |>.map (fun row =>
      row
      |>.map (fun | true => 'o' | _ => '.')
      |>.asString
    )
    |>.intersperse "\n"
    |> String.join

def Flood.get? (f:Flood) (xy:Pos) : Option Bool := do
  if xy.1 < 0 || xy.2 < 0 then none else
  (← f.cells[xy.1.toNat]?)[xy.2.toNat]?

def Flood.set! (xy:Pos) (f:Flood) : Flood :=
  let x := xy.1.toNat
  let y := xy.2.toNat
  {cells := f.cells.set x (f.cells[x]!.set y true)}

def Heightmap.toFlood (h:Heightmap) (xys:List Pos) : Flood :=
  {
    cells :=
      h.cells
      |> List.enum₂
      |> List.map (List.map (fun (xy₂, _) => xys.elem (Pos.ofNat xy₂)))
  }

namespace Parse
  def cell! : Char → Cell
    | 'S' => Cell.s
    | 'E' => Cell.e
    | c =>
      if 'a' <= c && c <= 'z' then
        Cell.h (c.toNat - 'a'.toNat)
      else
        panic! "invalid cell"

  def input! (ls:List String) : Heightmap :=
    { cells :=
      ls
      |>.map String.toList
      |>.map (List.map cell!)
    }
end Parse

def Flood.step (getNeighbours: Pos → List Pos) (roots:List Pos) (f:Flood) : Flood × List Pos :=
  let validNeighbours :=
    roots
    |>.bind getNeighbours
    |>.eraseDups
    |>.filter (fun xy => f.get? xy == some false)

  let f₁ := validNeighbours.foldr Flood.set! f
  (f₁, validNeighbours)

partial def floodToEnd (ss:List Pos) (e:Pos) (h:Heightmap) :=
  let rec loop (f:Flood) (roots:List Pos) (n:Nat) :=
    match f.get? e with
    | some true => n
    | _ =>
      let (f₁, roots₁) := f |> Flood.step h.getNeighbours roots
      loop f₁ roots₁ (n + 1)
  loop (h.toFlood ss) ss 0

def part1 (ls:List String) :=
  let h := Parse.input! ls
  let e := h.findPositions (Cell.e==·) |>.head!
  let ss := h.findPositions (Cell.s==·)
  floodToEnd ss e h

def part2 (ls:List String) :=
  let h := Parse.input! ls
  let e := h.findPositions (Cell.e==·) |>.head!
  let ss := h.findPositions (fun | Cell.s | Cell.h 0 => true | _ => false)
  floodToEnd ss e h

def testInput :=
  [ "Sabqponm"
  , "abcryxxl"
  , "accszExk"
  , "acctuvwj"
  , "abdefghi"
  ]
#eval 31 <== part1 testInput
#eval 29 <== part2 testInput

end Day12