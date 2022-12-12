import LeanExt

namespace Day8

structure Tree where
  size : Nat
  pos : Nat × Nat
deriving Repr, Inhabited

def Tree.visibleOutside : List Tree → List Tree
  | [] => []
  | t :: ts =>
    t :: loop t ts
  where
  loop (tMax:Tree) : List Tree → List Tree
    | [] => []
    | t :: ts =>
      if t.size > tMax.size
      then t :: loop t ts
      else loop tMax ts

theorem Tree.visibleOutside.loop_idempotent {t:Tree} {ts:List Tree}
  : visibleOutside.loop t ts = visibleOutside.loop t (visibleOutside.loop t ts)
  := by
  cases ts <;> simp [loop]
  simp [loop]
  split <;> simp [loop, *] <;> apply loop_idempotent

theorem Tree.visibleOutside_idempotent {ts:List Tree}
  : Tree.visibleOutside ts = Tree.visibleOutside (Tree.visibleOutside ts)
  := by
  cases ts <;> simp [visibleOutside]
  apply Tree.visibleOutside.loop_idempotent
  -- 🎉 finally, just took a rewrite of Tree.visibleOutside

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