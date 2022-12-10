import LeanExt

namespace Day4

abbrev SectionId := Nat

structure SectionRange where
  left : SectionId
  right : SectionId
  ordered : left ≤ right
deriving Repr

instance : Inhabited SectionRange where
  default := {left:=0, right:=0, ordered := by simp}

def SectionRange.mk? (left:SectionId) (right:SectionId) : Option SectionRange :=
  -- "if h:p then ... else ..." is syntax sugar for h.dite (fun ¬h -> ..) (fun h -> ..)
  if ordered:left <= right then
    some {left:=left, right:=right, ordered:=ordered : SectionRange}
  else
    none

def SectionRange.parse! (s:String) : SectionRange :=
  s.splitOn "-"
  |> List.filterMap String.toNat?
  |> fun
    | [left, right] =>
      SectionRange.mk? left right
      |>.get!
    | _ => panic! "oups"

def SectionRange.length (a:SectionRange) : Nat :=
  a.right - a.left + 1

def SectionRange.isSubset (a:SectionRange) (b:SectionRange) : Bool :=
  (a.left <= b.left) && (b.right <= a.right)

def SectionRange.intersect? (a:SectionRange) (b:SectionRange) : Option SectionRange :=
  let left := max a.left b.left
  let right := min b.right a.right
  SectionRange.mk? left right

structure ElvenPair where
  left : SectionRange
  right : SectionRange
deriving Inhabited, Repr

def ElvenPair.parse! (s:String) : ElvenPair :=
  s.splitOn ","
  |> fun
    | [left, right] => {left := SectionRange.parse! left, right := SectionRange.parse! right : ElvenPair}
    | _ => panic! "oups"

def part1 (ls:List String) :=
  ls
  |>.map ElvenPair.parse!
  |>.filter (fun ep => ep.left.isSubset ep.right || ep.right.isSubset ep.left)
  |>.length

def part2 (ls:List String) :=
  ls
  |>.map ElvenPair.parse!
  |>.filterMap (fun ep => ep.left.intersect? ep.right)
  |>.length

def testInput :=
  [ "2-4,6-8"
  , "2-3,4-5"
  , "5-7,7-9"
  , "2-8,3-7"
  , "6-6,4-6"
  , "2-6,4-8"
  ]
#eval 2 <== part1 testInput
#eval 5 <== part2 testInput

end Day4