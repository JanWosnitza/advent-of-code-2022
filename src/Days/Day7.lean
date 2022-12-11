import LeanExt

namespace Day7

abbrev Path := List String
abbrev Size := Nat

structure File where
  path : Path
  size : Size
deriving Repr

def extractDirectorySizes (fs:List File) :=
  fs
  |>.bind (fun file =>
    let rec loop (sizes:List (Path × Size)) : Path → List (Path × Size)
      | [] => sizes
      | _ :: ps => loop ((ps, file.size) :: sizes) ps
    loop [] file.path
  )
  |>.groupByEx id
  |>.map (fun (path, sizes) => (path, List.sum sizes))
  -- optional: convert into string path
  --|>.map (fun (path, size) => ("/" ++ (path.reverse |>.intersperse "/" |> String.join), size))

namespace Parse
  def input! (ls:List String) : List File :=
    loop [] [] ls
    where
    loop (path:Path) (files:List File) : List String → List File
    | [] => files
    | l :: ls =>
      match l.splitOn " " with
      | ["$", "cd", "/"] => loop [] files ls
      | ["$", "cd", ".."] => loop (path.tail!) files ls
      | ["$", "cd", dir] => loop (dir :: path) files ls
      | ["$", "ls"] => loop path files ls -- 🤷
      | ["dir", _] => loop path files ls  -- 🤷
      | [size, file] => loop path ({path := file :: path, size := size.toNat!} :: files) ls
      | o => panic! s!"unkown output: {o}"
end Parse

def part1 (ls:List String) :=
  ls
  |> Parse.input!
  |> extractDirectorySizes
  |>.filter (fun (_, size) => size <= 100000)
  |>.map (fun (_, size) => size)
  |>.sum

def part2 (ls:List String) :=
  let files :=
    ls
    |> Parse.input!
  let fileSize := files.sumBy (fun x => x.size)
  let requiredSize := fileSize - (70000000 - 30000000)

  files
  |> extractDirectorySizes
  |>.filter (fun (_, size) => size >= requiredSize)
  |>.map (fun (_, size) => size)
  |>.minimum?
  |>.get!

def testInput :=
  ["$ cd /"
  , "$ ls"
  , "dir a"
  , "14848514 b.txt"
  , "8504156 c.dat"
  , "dir d"
  , "$ cd a"
  , "$ ls"
  , "dir e"
  , "29116 f"
  , "2557 g"
  , "62596 h.lst"
  , "$ cd e"
  , "$ ls"
  , "584 i"
  , "$ cd .."
  , "$ cd .."
  , "$ cd d"
  , "$ ls"
  , "4060174 j"
  , "8033020 d.log"
  , "5626152 d.ext"
  , "7214296 k"
  ]
#eval 95437 <== part1 testInput
#eval 24933642 <== part2 testInput

end Day7