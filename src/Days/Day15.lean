import LeanExt

namespace Day15

structure Pos where
  x : Int
  y : Int
deriving Inhabited, Repr

def Pos.manhattenTo (a b : Pos) : Nat := (a.x - b.x).natAbs + (a.y - b.y).natAbs

#eval Pos.manhattenTo ⟨0, 0⟩ ⟨2, 1⟩

structure Sensor where
  position : Pos
  closestBeacon : Pos
deriving Inhabited, Repr

def Sensor.getRange? (y:Int) (sensor:Sensor) :=
  let distance := sensor.position.manhattenTo sensor.closestBeacon
  let relativeY := (y - sensor.position.y).natAbs
  let dx := distance - relativeY
  if dx == 0 then none else
  let minX := sensor.position.x - Int.ofNat dx
  let widthAtTestY := dx * 2
  some (minX, minX + widthAtTestY)

namespace Parse
  def pos! (s:String) : Pos :=
    match s.splitOn ", " |>.map (String.splitOn · "=") with
    | [["x", sX], ["y", sY]] =>
      { x := sX.toInt!
      , y := sY.toInt!
      }
    | _ => panic! s!"invalid position \"{s}\""

  def sensor! (s:String) : Sensor :=
    match s.splitOn ": " |>.map (String.splitOn · " at ") with
    | [["Sensor" /-at-/, sSensorPos], ["closest beacon is" /-at-/, sClosestBecon]] =>
      { position := pos! sSensorPos
        closestBeacon := pos! sClosestBecon
      }
    | _ => panic! ""

  def input! (ls:List String) : List Sensor :=
    ls
    |>.map sensor!
end Parse

def range (l r:Int) : List Int :=
  List.range (r - l).toNat
  |>.map (l + Int.ofNat ·)

def part1 (ls:List String) (testY : Int := 2000000) :=
  ls
  |> Parse.input!
  |>.filterMap (Sensor.getRange? testY)
  |>.sortBy (·.1)
  |>.foldl (fun (lastX, sum) (xStart, xEnd) =>
    let start := max lastX xStart
    let dx := (xEnd - start).toNat
    (start + dx, sum + dx)
  ) (Int.negOfNat 10000000000000000, 0)
  |>.2

def part2 (ls:List String) (xyMax : Nat := 4000000) :=
  let sensors :=
    ls
    |> Parse.input!
  ------------------------------------------------
  let yStep := xyMax / 10   -- This is
  List.range 10             -- a workaround
  |>.bind (fun yy =>        -- because List.range
    List.range yStep        -- stackoverflows
    |>.map (yStep * yy + ·) -- with 4000000
    ----------------------------------------------
    |>.bind (fun testY =>
      sensors
      |>.filterMap (Sensor.getRange? (Int.ofNat testY))
      |>.sortBy (·.1)
      |>.foldl (fun (lastX, acc) (xStart, xEnd) =>
        if lastX + 1 < xStart then
          -- found a range wich is not reached by sensors
          let unkownPositions : List Pos :=
            let xStart₁ := lastX + 1
            let xEnd₁ := xStart - 1
            List.range (xEnd₁ - xStart₁ + 1).toNat
            |>.map (fun x => {x := xStart₁ + Int.ofNat x, y := testY})
          (xEnd, unkownPositions ++ acc)
        else
          (max lastX xEnd, acc)
      ) (Int.ofNat 0, [])
      |>.2
    )
  )
  |>.map (fun p => p.x * 4000000 + p.y)
  |>.sum

def testInput :=
  [ "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
  , "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
  , "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
  , "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
  , "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
  , "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
  , "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
  , "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
  , "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
  , "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
  , "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
  , "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
  , "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
  , "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
  ]
#eval 26 <== part1 testInput 10
#eval (Int.ofNat 56000011) <== part2 testInput 20

end Day15