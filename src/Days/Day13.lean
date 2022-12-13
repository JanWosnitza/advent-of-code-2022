import LeanExt

namespace Day13

inductive Paket where
  | nat (n:Nat)
  | sub (pakets:List Paket)
deriving Inhabited, Repr, BEq

partial def Paket.toString : Paket → String
  | Paket.nat n => s!"{n}"
  | Paket.sub ps =>
    let body :=
      ps
      |>.map toString
      |>.intersperse ","
      |> String.join
    s!"[{body}]"

instance : ToString Paket where
  toString := Paket.toString

partial def Paket.compare : Paket × Paket → Ordering
  | (n₁@(nat _), s₂@(sub _)) => compare (sub [n₁], s₂)
  | (s₁@(sub _), n₂@(nat _)) => compare (s₁, sub [n₂])

  | (nat n₁, nat n₂) =>
    if n₁ < n₂ then Ordering.lt
    else if n₁ > n₂ then Ordering.gt
    else Ordering.eq

  | (sub s₁, sub s₂) =>
    let rec loop : List Paket → List Paket → Ordering
      | [], [] => Ordering.eq -- 😓
      | [], _ => Ordering.lt
      | _, [] => Ordering.gt
      | l :: ls, r :: rs =>
        match compare (l, r) with
        | Ordering.eq => loop ls rs
        | x => x
    loop s₁ s₂

instance : Ord Paket where compare a b := Paket.compare (a, b)

namespace Parse
  partial def paket! : List Char → Paket × List Char
    | [] => panic! ""
    | '[' :: ']' :: cs => (Paket.sub [], cs)
    | '[' :: cs =>
      let (ps, cs) := iterList (',' :: cs)
      (Paket.sub ps, cs)
    | cs =>
      match cs.span (·.isDigit) with
      | ([], _) => panic! "not a number"
      | (number, cs₁) => (Paket.nat (number.asString.toNat!), cs₁)
  where
    iterList : List Char → List Paket × List Char
      | ']' :: cs => ([], cs)
      | ',' :: cs =>
        let (p, cs₁) := paket! cs
        let (ps, cs₂) := iterList cs₁
        (p :: ps, cs₂)
      | _ => panic! "asdf"

  def input! (ls:List String) : List (Paket × Paket) :=
    ls
    |>.splitOn ""
    |>.map (fun
      | [l₁, l₂] =>
        ((paket! l₁.toList).1, (paket! l₂.toList).1)
      | _ =>
        panic! ""
    )

end Parse

def part1 (ls:List String) :=
  ls
  |> Parse.input!
  |>.enum
  |>.filter (fun (_, pp) => Paket.compare pp == Ordering.lt)
  |>.map (1 + ·.1)
  |>.sum

def part2 (ls:List String) :=
  let p1 := Paket.sub [Paket.sub [Paket.nat 2]]
  let p2 := Paket.sub [Paket.sub [Paket.nat 6]]
  let ps :=
    (p1, p2) :: Parse.input! ls
    |>.bind (fun (a, b) => [a, b])
    |>.sort

  let idx1 := ps.findIndex? (·==p1) |>.get!
  let idx2 := ps.findIndex? (·==p2) |>.get!

  (idx1 + 1) * (idx2 + 1)

def testInput :=
  [ "[1,1,3,1,1]"
  , "[1,1,5,1,1]"
  , ""
  , "[[1],[2,3,4]]"
  , "[[1],4]"
  , ""
  , "[9]"
  , "[[8,7,6]]"
  , ""
  , "[[4,4],4,4]"
  , "[[4,4],4,4,4]"
  , ""
  , "[7,7,7,7]"
  , "[7,7,7]"
  , ""
  , "[]"
  , "[3]"
  , ""
  , "[[[]]]"
  , "[[]]"
  , ""
  , "[1,[2,[3,[4,[5,6,7]]]],8,9]"
  , "[1,[2,[3,[4,[5,6,0]]]],8,9]"
  ]
#eval 13 <== part1 testInput
#eval 140 <== part2 testInput

end Day13