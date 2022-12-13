import Lake
open Lake DSL

package aoc { srcDir := "src" }

lean_lib LeanExt

lean_lib Days.Day1
lean_lib Days.Day2
lean_lib Days.Day3
lean_lib Days.Day4
lean_lib Days.Day5
lean_lib Days.Day6
lean_lib Days.Day7
lean_lib Days.Day8
lean_lib Days.Day9
lean_lib Days.Day10
lean_lib Days.Day11
lean_lib Days.Day12
lean_lib Days.Day13
lean_lib Days

@[defaultTarget]
lean_exe aoc {
  root := `Main
}
