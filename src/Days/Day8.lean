import LeanExt

namespace Day8

structure Tree where
  size : Nat
  pos : Nat × Nat
deriving Repr, Inhabited

def Tree.visibleOutside : List Tree → List Tree
  | [] => []
  | t :: ts =>
    let (_, ts) :=
      List.foldl
        (fun (tMax, ts) (t:Tree) =>
          if t.size > tMax then (t.size, t :: ts) else (tMax, ts)
        )
        (t.size, [t])
        ts
    ts.reverse

theorem Tree.visibleOutside_idempotent {ls:List Tree}
  : Tree.visibleOutside ls = Tree.visibleOutside (Tree.visibleOutside ls) := by
  simp [Tree.visibleOutside]
  split
  case h_1 => rfl
  case h_2 =>
    sorry

def Tree.treeHouseVisible (acc:List (Tree × List Tree)) : List Tree → List (Tree × List Tree)
  | [] => acc.reverse
  | t₁ :: ts₁ =>
    let rec visible (acc:List Tree) : List Tree → List Tree
      | [] => acc.reverse
      | t₂ :: ts₂ =>
        let acc := t₂ :: acc
        if t₂.size < t₁.size then
          visible acc ts₂
        else
          acc |>.reverse
    Tree.treeHouseVisible ((t₁, visible [] ts₁) :: acc) ts₁

def Tree.allSides (tss:List (List Tree)) : List (List Tree) :=
  let left := tss
  let right := left |>.map List.reverse
  let top := tss.transpose!
  let bottom := top |>.map List.reverse
  [left, right, top, bottom].join

namespace Parse
  def tree! (ls:List String) : List (List Tree) :=
    ls
    |>.enum
    |>.map (fun (x, l) =>
      l.toList
      |>.map (fun c => c.toNat - '0'.toNat)
      |>.enum
      |>.map (fun (y, size) => {size := size, pos := (x, y) :Tree})
    )
end Parse

def part1 (ls:List String) :=
  ls
  |> Parse.tree!
  |> Tree.allSides
  |>.map Tree.visibleOutside
  |>.join
  |>.map Tree.pos
  |>.eraseDups
  |>.length

def part2 (ls:List String) :=
  ls
  |> Parse.tree!
  |> Tree.allSides
  |>.bind (Tree.treeHouseVisible [])
  |>.groupByEx (fun (t, ts) => (t.pos, ts.length))
  |>.map (List.foldl (·*·) 1 ∘ Prod.snd)
  |>.maximum?
  |>.get!

def testInput :=
  [ "30373"
  , "25512"
  , "65332"
  , "33549"
  , "35390"
  ]
#eval 21 <== part1 testInput
#eval 8 <== part2 testInput

end Day8