import Days.Day1
import Days.Day2
import Days.Day3
import Days.Day4
import Days.Day5
import Days.Day6
import Days.Day7
import Days.Day8
import Days.Day9
import Days.Day10
import Days.Day11
import Days.Day12
import Days.Day13
import Days.Day14
import Days.Day15
import Days.Day16

structure Solution where
  day : Nat
  part : Nat
  out : Type
  toString : ToString out
  f : List String → out

def solution day part [toString: ToString α] (f:List String → α) :=
  { day := day
  , part := part
  , out := α
  , toString := toString
  , f := f : Solution
  }

instance : Inhabited Solution where
  default := solution 0 0 (f := fun _ => ())

def days : List Solution :=
  [ solution 1 1 Day1.part1
  , solution 1 2 Day1.part2
  , solution 2 1 Day2.part1
  , solution 2 2 Day2.part2
  , solution 3 1 Day3.part1
  , solution 3 2 Day3.part2
  , solution 4 1 Day4.part1
  , solution 4 2 Day4.part2
  , solution 5 1 Day5.part1
  , solution 5 2 Day5.part2
  , solution 6 1 Day6.part1
  , solution 6 2 Day6.part2
  , solution 7 1 Day7.part1
  , solution 7 2 Day7.part2
  , solution 8 1 Day8.part1
  , solution 8 2 Day8.part2
  , solution 9 1 Day9.part1
  , solution 9 2 Day9.part2
  , solution 10 1 Day10.part1
  , solution 10 2 Day10.part2
  , solution 11 1 Day11.part1
  , solution 11 2 Day11.part2
  , solution 12 1 Day12.part1
  , solution 12 2 Day12.part2
  , solution 13 1 Day13.part1
  , solution 13 2 Day13.part2
  , solution 14 1 Day14.part1
  , solution 14 2 Day14.part2
  , solution 15 1 Day15.part1
  , solution 15 2 Day15.part2
  , solution 16 1 Day16.part1
  , solution 16 2 Day16.part2
  ]
