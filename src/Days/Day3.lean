import LeanExt

namespace Day3

inductive Item where | item : Char -> Item
deriving Inhabited, BEq, Repr

def Item.priority : Item -> Nat
  | Item.item c =>
    if c >= 'a' && c <= 'z' then
      c.toNat - 'a'.toNat + 1
    else
      c.toNat - 'A'.toNat + 27

structure Rucksack where
  items : List Item

def Rucksack.leftCompartment (r:Rucksack) : List Item :=
  r.items.take (r.items.length / 2)

def Rucksack.rightCompartment (r:Rucksack) : List Item :=
  r.items.drop (r.items.length / 2)

namespace Parse
  def rucksack (l:String) : Rucksack :=
    {items := l |>.toList |>.map Item.item}

  def input (ls:List String) : List Rucksack :=
    ls
    |>.map rucksack
end Parse

def part1 (ls:List String) :=
  ls
  |> Parse.input
  |>.map (fun r =>
    r.leftCompartment
    |>.intersect r.rightCompartment
    |>.eraseDups
  )
  |>.join
  |>.map Item.priority
  |>.sum

def part2 (ls:List String) :=
  let groupInto3 (rs:List _) :=
    rs
    |>.enum
    |>.map (fun (idx, r) => (idx / 3, r))
    |>.groupBy (fun (group₁, _) (group₂, _) => group₁ == group₂)
    |>.map (List.map (fun (_, r) => r))

  ls
  |> Parse.input
  |> groupInto3
  |>.map (fun group =>
    group
    |>.map Rucksack.items
    |>.reducel! List.intersect
    |>.eraseDups
  )
  |>.join
  |>.map Item.priority
  |>.sum

def testInput :=
  [ "vJrwpWtwJgWrhcsFMMfFFhFp"
  , "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
  , "PmmdzqPrVvPwwTWBwg"
  , "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
  , "ttgJtRGJQctTZtZT"
  , "CrZsJsPPZsGzwwsLwLmpwMDw"
  ]
#eval 157 <== part1 testInput
#eval  70 <== part2 testInput

end Day3