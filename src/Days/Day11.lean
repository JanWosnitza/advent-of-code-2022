import LeanExt

namespace Day11

structure Item where worryLevel : Nat
deriving Inhabited, Repr

inductive Operand where
  | const : Nat → Operand
  | old : Operand
deriving Inhabited, Repr

instance : ToString Operand where
  toString
    | Operand.const n => s!"{n}"
    | Operand.old => "old"

def Operand.value (item:Item) : Operand → Nat
  | const n => n
  | old => item.worryLevel

inductive Operation where
  | add : Operand → Operand → Operation
  | multiply : Operand → Operand → Operation
deriving Inhabited, Repr

instance : ToString Operation where
  toString
    | Operation.add l r => s!"{l} + {r}"
    | Operation.multiply l r => s!"{l} * {r}"

def Operation.update (op:Operation) (item:Item) : Item :=
  let value :=
    match op with
    | add l r => l.value item + r.value item
    | multiply l r => l.value item * r.value item

  {worryLevel := value}

structure MonkeyName where name : String
deriving Inhabited, Repr, BEq

inductive Test where | divisibleBy : Nat → Test
deriving Inhabited, Repr

def Test.test (item:Item) : Test → Bool
  | divisibleBy n => item.worryLevel % n == 0

inductive Command where | throwToMonkey : MonkeyName → Command
deriving Inhabited, Repr

structure Monkey where
  items : List Item
  operation : Operation
  test : Test
  trueCommand : Command
  falseCommand : Command
  inspectedCount : Nat
deriving Inhabited, Repr

def Monkey.pass (beforeInspect:Item → Item) (m:Monkey) : Monkey × List (Command × Item) :=
  ( {m with
      items := []
      inspectedCount := m.inspectedCount + m.items.length
    }
  , m.items
    |> List.map (fun item =>
      let item₁ :=
        m.operation.update item
        |> beforeInspect
      if m.test.test item₁
      then (m.trueCommand, item₁)
      else (m.falseCommand, item₁)
    ) 
  )

def Monkey.receiv (item:Item) (m:Monkey) : Monkey :=
  {m with items := m.items ++ [item]}

structure Monkeys where
  names : List MonkeyName
  monkeys : List Monkey
  sameLength : names.length = monkeys.length
deriving Repr

instance : Inhabited Monkeys where
  default :=
    { names := []
    , monkeys := []
    , sameLength := by simp
    }

def Monkeys.update! (name:MonkeyName) (updater:Monkey → Monkey × α) (ms:Monkeys) : Monkeys × α :=
  let idx := ms.names.findIndex? (·==name) |>.get!
  let (m, ret) := updater ms.monkeys[idx]! 
  ( { names := ms.names
      monkeys := ms.monkeys.set idx m
      sameLength := by
        simp [List.set]
        exact ms.sameLength
    }
  , ret
  )

def Monkeys.doCommand! (ms:Monkeys) (cmdItem: Command × Item) : Monkeys :=
  match cmdItem with
  | (Command.throwToMonkey name, item) =>
    ms.update! name (⟨Monkey.receiv item ·, ()⟩)
    |>.fst

def Monkeys.pass! (beforeInspect:Item → Item) (name:MonkeyName) (ms:Monkeys) : Monkeys × List (Command × Item) :=
  ms.update! name (Monkey.pass beforeInspect)

def Monkeys.round! (beforeInspect:Item → Item) (ms:Monkeys) : Monkeys :=
  loop ms ms.names
  where
  loop (ms:Monkeys) : List MonkeyName → Monkeys
  | [] => ms
  | name :: names =>
    let (ms₁, commands) := ms.pass! beforeInspect name
    loop (commands.foldl Monkeys.doCommand! ms₁) names

namespace Parse
  def item! (s:String) : Item := {worryLevel := s.toNat!}

  def itemList! (s:String) : List Item :=
    s.splitOn ", "
    |> List.map item!

  def operand! : String → Operand
    | "old" => Operand.old
    | n => Operand.const (n.toNat!)

  def operation! (s:String) : Operation :=
    match s.splitOn " " with
    | ["new", "=", l, "+", r] => Operation.add (operand! l) (operand! r)
    | ["new", "=", l, "*", r] => Operation.multiply (operand! l) (operand! r)
    | _ => panic! "invalid operation"

  def monkeyName! (s:String) : MonkeyName :=
    match s.splitOn " " with
    | ["Monkey", name] => {name := name}
    | _ => panic! "invalid monkey name"

  def test! (s:String) : Test :=
    match s.splitOn " " with
    | ["divisible", "by", n] => Test.divisibleBy (n.toNat!)
    | _ => panic! "invalid test"

  def command! (s:String) : Command :=
    match s.splitOn " " with
    | ["throw", "to", "monkey", name] => Command.throwToMonkey {name := name}
    | _ => panic! "invalid command"

  def monkey! (ls:List String) : MonkeyName × Monkey :=
    match ls.map (fun s => s.splitOn ":") with
    | [ [name, ""]
      , ["  Starting items", items]
      , ["  Operation", operation]
      , ["  Test", test]
      , ["    If true", trueCommand]
      , ["    If false", falseCommand]
      ] =>
        ( monkeyName! name
        , { items := itemList! items.trimLeft
          , operation := operation! operation.trimLeft
          , test := test! test.trimLeft
          , trueCommand := command! trueCommand.trimLeft
          , falseCommand := command! falseCommand.trimLeft
          , inspectedCount := 0
          }
        )
    | _ => panic! "inavlid monkey"

  def monkeys! (ls:List String) : Monkeys :=
    let monkeys :=
      ls
      |>.splitOn ""
      |>.filter (·≠[])
      |>.map monkey!

    {
      names := monkeys |>.map Prod.fst
      monkeys := monkeys |>.map Prod.snd
      sameLength := by simp
    }
end Parse

def partX rounds beforeInspect (ms:Monkeys) :=
  List.range rounds
  |>.foldl (fun ms₁ _ => Monkeys.round! beforeInspect ms₁) ms
  |>.monkeys
  |>.map (fun m => m.inspectedCount)
  |>.sort
  |>.reverse
  |>.take 2
  |>.foldl (·*·) 1

def part1 (ls:List String) :=
  ls
  |> Parse.monkeys!
  |> partX 20 (fun item => {worryLevel := item.worryLevel / 3})

def part2 (ls:List String) :=
  let monkeys := Parse.monkeys! ls

  let lcm a b := a * b / Nat.gcd a b

  let mod :=
    monkeys.monkeys
    |>.map (fun m => match m.test with | Test.divisibleBy n => n)
    |>.foldr lcm 1

  monkeys
  |> partX 10000 (fun item => {worryLevel := item.worryLevel % mod})

def testInput :=
  [ ""
  , "Monkey 0:"
  , "  Starting items: 79, 98"
  , "  Operation: new = old * 19"
  , "  Test: divisible by 23"
  , "    If true: throw to monkey 2"
  , "    If false: throw to monkey 3"
  , ""
  , "Monkey 1:"
  , "  Starting items: 54, 65, 75, 74"
  , "  Operation: new = old + 6"
  , "  Test: divisible by 19"
  , "    If true: throw to monkey 2"
  , "    If false: throw to monkey 0"
  , ""
  , "Monkey 2:"
  , "  Starting items: 79, 60, 97"
  , "  Operation: new = old * old"
  , "  Test: divisible by 13"
  , "    If true: throw to monkey 1"
  , "    If false: throw to monkey 3"
  , ""
  , "Monkey 3:"
  , "  Starting items: 74"
  , "  Operation: new = old + 3"
  , "  Test: divisible by 17"
  , "    If true: throw to monkey 0"
  , "    If false: throw to monkey 1"
  ]
#eval 10605 <== part1 testInput
#eval 2713310158 <== part2 testInput

end Day11