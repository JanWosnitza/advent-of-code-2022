import LeanExt

namespace Day10

inductive Instruction where
  | noop : Instruction
  | addx : Int → Instruction
deriving Inhabited, Repr

def Instruction.parse! (s:String) : Instruction :=
  match s.splitOn " " with
  | ["noop"] => noop
  | ["addx", sv] => addx (sv.toInt!)
  | _ => panic! "unknown instruction"

abbrev Cycle := Nat

structure CPU where
  regX : Int
deriving Repr

def CPU.initial : CPU := {regX := 1}

instance : Inhabited CPU where
  default := CPU.initial

def CPU.exec (cpu:CPU) : Instruction → Nat × CPU
  | Instruction.noop => (1, cpu)
  | Instruction.addx v => (2, {regX := cpu.regX + v})

def CPU.execs (cpu:CPU) (history:List CPU) : List Instruction → CPU × List CPU
  | [] => (cpu, history.reverse)
  | i :: is =>
    let (cycles, cpu₁) := cpu.exec i
    execs cpu₁ (List.replicate cycles cpu ++ history) is

def part1 (ls:List String) :=
  ls
  |>.map Instruction.parse!
  |> CPU.execs CPU.initial []
  |> Prod.snd
  |>.enum
  |>.map (fun (n, cpu) => (n + 1, cpu))
  |>.filter (fun (cycle, _) => cycle % 40 == 20) -- match n with | 20 | 60 | 100 | 140 | 180 | 220 => true | _ => false)
  |>.map (fun (cycle, cpu) => Int.ofNat cycle * cpu.regX)
  |>.sum

def part2 (ls:List String) :=
  ls
  |>.map Instruction.parse!
  |> CPU.execs CPU.initial []
  |> Prod.snd
  |>.enum
  |>.groupByFixed (fun (cycle₁, _) (cycle₂, _) => cycle₁ / 40 == cycle₂ / 40)
  |>.map (fun row =>
    row
    |> List.map (fun (cycle, cpu) =>
      let ray := cycle % 40
      let spriteMin := cpu.regX - 1
      let spriteMax := cpu.regX + 1
      if spriteMin <= ray && ray <= spriteMax then '#' else '.'
    )
    |>.asString
  )
  |>.intersperse "\n"
  |> String.join

def testInput :=
  [ "addx 15"
  , "addx -11"
  , "addx 6"
  , "addx -3"
  , "addx 5"
  , "addx -1"
  , "addx -8"
  , "addx 13"
  , "addx 4"
  , "noop"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx -35"
  , "addx 1"
  , "addx 24"
  , "addx -19"
  , "addx 1"
  , "addx 16"
  , "addx -11"
  , "noop"
  , "noop"
  , "addx 21"
  , "addx -15"
  , "noop"
  , "noop"
  , "addx -3"
  , "addx 9"
  , "addx 1"
  , "addx -3"
  , "addx 8"
  , "addx 1"
  , "addx 5"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "addx -36"
  , "noop"
  , "addx 1"
  , "addx 7"
  , "noop"
  , "noop"
  , "noop"
  , "addx 2"
  , "addx 6"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "addx 1"
  , "noop"
  , "noop"
  , "addx 7"
  , "addx 1"
  , "noop"
  , "addx -13"
  , "addx 13"
  , "addx 7"
  , "noop"
  , "addx 1"
  , "addx -33"
  , "noop"
  , "noop"
  , "noop"
  , "addx 2"
  , "noop"
  , "noop"
  , "noop"
  , "addx 8"
  , "noop"
  , "addx -1"
  , "addx 2"
  , "addx 1"
  , "noop"
  , "addx 17"
  , "addx -9"
  , "addx 1"
  , "addx 1"
  , "addx -3"
  , "addx 11"
  , "noop"
  , "noop"
  , "addx 1"
  , "noop"
  , "addx 1"
  , "noop"
  , "noop"
  , "addx -13"
  , "addx -19"
  , "addx 1"
  , "addx 3"
  , "addx 26"
  , "addx -30"
  , "addx 12"
  , "addx -1"
  , "addx 3"
  , "addx 1"
  , "noop"
  , "noop"
  , "noop"
  , "addx -9"
  , "addx 18"
  , "addx 1"
  , "addx 2"
  , "noop"
  , "noop"
  , "addx 9"
  , "noop"
  , "noop"
  , "noop"
  , "addx -1"
  , "addx 2"
  , "addx -37"
  , "addx 1"
  , "addx 3"
  , "noop"
  , "addx 15"
  , "addx -21"
  , "addx 22"
  , "addx -6"
  , "addx 1"
  , "noop"
  , "addx 2"
  , "addx 1"
  , "noop"
  , "addx -10"
  , "noop"
  , "noop"
  , "addx 20"
  , "addx 1"
  , "addx 2"
  , "addx 2"
  , "addx -6"
  , "addx -11"
  , "noop"
  , "noop"
  , "noop"
  ]
#eval (13140:Int) <== part1 testInput

def part2Expected :=
  "##..##..##..##..##..##..##..##..##..##..\n" ++
  "###...###...###...###...###...###...###.\n" ++
  "####....####....####....####....####....\n" ++
  "#####.....#####.....#####.....#####.....\n" ++
  "######......######......######......####\n" ++
  "#######.......#######.......#######....."
#eval part2Expected <== part2 testInput

end Day10