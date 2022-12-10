import LeanExt

namespace Day6

def partX (width:Nat) (ls:List String) :=
  ls
  |>.head!
  |>.toList
  |>.windowed width
  |>.takeWhile (fun w => w.length ≠ w.eraseDups.length)
  |>.length
  |> (· + width)

def part1 := partX 4

def part2 := partX 14

#eval 7 <== part1 ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"]
#eval 5 <== part1 ["bvwbjplbgvbhsrlpgdmjqwftvncz"]
#eval 6 <== part1 ["nppdvjthqldpwncqszvftbrmjlhg"]
#eval 10 <== part1 ["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"]
#eval 11 <== part1 ["zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]

#eval 19 <== part2 ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"]
#eval 23 <== part2 ["bvwbjplbgvbhsrlpgdmjqwftvncz"]
#eval 23 <== part2 ["nppdvjthqldpwncqszvftbrmjlhg"]
#eval 29 <== part2 ["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"]
#eval 26 <== part2 ["zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]

end Day6