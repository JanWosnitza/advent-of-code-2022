
section Test
  class Test (α:Type) (β:Type) where
    get : α → β → IO Unit

  instance [ToString β] : Test α β where
    get (_) (result) := IO.println s!"Not solved. {result}"

  instance [Repr β] : Test α β where
    get (_) (result) := do
      IO.println "Not solved."
      IO.println (reprStr result)

  instance [BEq α] [ToString α] : Test α α where
    get (expected) (result) :=
      if expected == result then
        IO.println "Hurray!!"
      else do
        IO.println s!"Not solved expected"
        IO.println s!"{expected}"
        IO.println "but got"
        IO.println s!"{result}"

  infix:50 " <== " => Test.get
end Test

namespace List
  def sum [inhab:Inhabited α] [Add α] : List α → α
    | [] => inhab.default
    | x :: xs => Add.add x (sum xs)

  def sumBy (f:α → Nat) := sum ∘ List.map f

  theorem sum_sums (n:Nat) (ns:List Nat) : sum (n :: ns) = n + sum ns := by rfl

  theorem filter_size (ls:List α) (f:α → Bool) : (ls.filter f).length <= ls.length := by
    cases ls
    case nil => simp [List.filter]
    case cons =>
      simp [List.filter]
      split
      case h_1 =>
        apply Nat.add_le_add_right
        simp [filter_size]
      case h_2 =>
        apply Nat.le_step
        simp [filter_size]

  def sortBy_Filter [LT β] [DecidableRel (@LT.lt β _)] (selector:α → β) (ls:List α) :=
    match ls with
    | [] => []
    | [x] => [x]
    | x :: xs =>
      --let (left, right) := xs.partition (fun y => selector x > selector y)
      let sx := selector x
      let left := xs.filter (fun y => sx > selector y)
      let right := xs.filter (fun y => selector y > sx)
      let rightSorted := right.sortBy_Filter selector
      let leftSorted := left.sortBy_Filter selector
      leftSorted ++ x :: rightSorted
  termination_by sortBy_Filter _ ls => ls.length
  decreasing_by
    simp_wf
    apply Nat.lt_succ_of_le
    simp [filter_size]

  theorem partitionAux_length (f:α → Bool) (ls left right:List α)
    : (ls.partitionAux f (left, right)).1.length + (ls.partitionAux f (left, right)).2.length = ls.length + left.length + right.length
    := by
    cases ls
    case nil => simp [List.partitionAux]
    case cons =>
      simp [List.partitionAux]
      split
      case h_1 head tail _ _ =>
        have : Nat.succ (length tail) + length left + length right
               = length tail + Nat.succ (length left) + length right
          := by simp [Nat.add_succ, Nat.add_comm]
        rw [this]
        have : Nat.succ (length left) = length (head :: left) := by rfl
        rw [this]
        apply partitionAux_length
      case h_2 head tail _ _ =>
        have : Nat.succ (length tail) + length left + length right
               = length tail + length left + Nat.succ (length right)
          := by simp [Nat.add_succ, Nat.add_comm]
        rw [this]
        have : Nat.succ (length right) = length (head :: right) := by rfl
        rw [this]
        apply partitionAux_length

  theorem partition_size {f:α → Bool} {ls:List α}
    : (ls.partition f).1.length + (ls.partition f).2.length = ls.length
    := by
    simp [List.partition]
    have : length ls = length ls + length ([]:List α) + length ([]:List α) := by rfl
    rw [this]
    exact partitionAux_length f ls [] []

  theorem partition_fst_size {f:α → Bool} {ls:List α}
    : (ls.partition f).1.length ≤ ls.length
    := by
    have : (ls.partition f).1.length + (ls.partition f).2.length = ls.length := partition_size
    apply Nat.le.intro this

  theorem partition_snd_size {f:α → Bool} {ls:List α}
    : (ls.partition f).2.length ≤ ls.length
    := by
    have : (ls.partition f).1.length + (ls.partition f).2.length = ls.length := partition_size
    rw [Nat.add_comm] at this
    apply Nat.le.intro this

  def sortBy [LT β] [DecidableRel (@LT.lt β _)] (selector:α → β) (ls:List α) :=
    match ls with
    | [] => []
    | [x] => [x]
    | x :: xs =>
      let sx := selector x
      let split := xs.partition (fun y => sx > selector y)
      let leftSorted := split.1.sortBy selector
      let rightSorted := split.2.sortBy selector
      leftSorted ++ x :: rightSorted
  termination_by sortBy _ ls => ls.length
  decreasing_by
    simp_wf
    apply Nat.lt_succ_of_le
    simp [partition_fst_size, partition_snd_size]

  def sort [LT α] [DecidableRel (@LT.lt α _)] (ls:List α) := ls.sortBy_Filter id

  #eval [3,2,1].sort == [1,2,3]

  def splitOn [eq:BEq α] (x:α) (ls:List α) :=
    let rec loop : List α → List (List α) → List (List α)
      | [], accs =>
        accs
        |> List.reverse
        |> List.map List.reverse
      | l :: ls, acc :: accs =>
        if eq.beq x l then
          loop ls ([] :: acc :: accs)
        else
          loop ls ((l :: acc) :: accs)
      | _, _ => panic! "unreachable"
    loop ls [[]]

  #eval ["1","","2"].splitOn "" == [["1"], ["2"]]

  def keep [BEq α] (left:List α) (right:List α) :=
    left |> List.filter right.contains

  #eval [0, 1, 2].keep [2, 3, 4] == [2]

  def reducel! [Inhabited α] : (α → α → α) → List α → α
    | reducer, x :: xs =>
      xs
      |>.foldl reducer x
    | _, [] => panic! "Empty list"

  def reducer! [Inhabited α] : (α → α → α) → List α → α
    | reducer, x :: xs =>
      xs
      |>.foldr reducer x
    | _, [] => panic! "Empty list"

  def transpose! [Inhabited α] (xs:List (List α)) :=
    List.range (xs[0]!.length)
    |> List.map (fun n => xs |> List.map (fun ys => ys[n]!))

  #eval [[1, 2], [3, 4]] = transpose! [[1, 3], [2, 4]]

  def tail : (as : List α) → as ≠ [] → List α
    | _::as, _ => as

  def findIndex? (isEq:α → Bool) (ls:List α) :=
    let rec loop idx : List α -> Option Nat
      | [] => none
      | x :: xs =>
        if isEq x then
          some idx
        else
          loop (idx + 1) xs
    loop 0 ls

  -- improve with proofs
  def windowedAux (ls:List α) (width:Nat) (acc:List (List α)) : List (List α) :=
    if width <= ls.length then
      windowedAux ls.tail! width (ls.take width :: acc)
    else
      acc.reverse
  decreasing_by sorry -- obviously todo
  
  def windowed (width:Nat) (ls:List α) :=
    windowedAux ls width []

  #eval windowed 3 [1, 2, 3, 4, 5]

  def groupByEx [Inhabited (β × γ)] [BEq β] [LT β] [DecidableRel (@LT.lt β _)] (get:α → β × γ) (ls:List α) : List (β × List γ) :=
    ls
    |>.map get
    |>.sortBy (fun x => x.1)
    |>.groupBy (fun x y => x.1 == y.1)
    |>.map (fun xs => (xs[0]!.1, xs.map (fun x => x.2) |>.reverse))

  #eval [("a", 1), ("a", 2), ("b", 3)].groupByEx id = [("a", [1, 2]), ("b", [3])]
  #eval [(["a"], 1), (["a"], 2), (["b"], 3)].groupByEx id = [(["a"], [1, 2]), (["b"], [3])]
  #eval [([], 1), (["b", "a"], 3), (["a"], 2), (["b", "a"], 4)].groupByEx id = [([], [1]), (["a"], [2]), (["b", "a"], [3, 4])]

  @[specialize] def groupByFixed (R : α → α → Bool) : List α → List (List α)
    | []    => []
    | a::as => loop as [[a]]
  where
    @[specialize] loop : List α → List (List α) → List (List α)
    | a::as, (ag::g)::gs => match R ag a with
      | true  => loop as ((a::ag::g)::gs)
      | false => loop as ([a]::(ag::g)::gs)
    | _, gs => gs.map List.reverse |>.reverse

  #eval [0, 1, 2, 3].groupByFixed (fun a b => a/2 == b/2) == [[0,1],[2,3]]
end List

namespace Option

  def toIO (error:IO.Error) : Option α → IO α
    | some x => do return x
    | none => do throw error

end Option

namespace String

  def splitByWidthAndGap (width) (gap) (s:String) :=
    let count := (s.length + gap) / (width + gap)
    List.range count
    |> List.map (fun n => s.drop (n*(width+gap)) |>.take width)

  #eval ["[a]", "[b]"] == "[a] [b]".splitByWidthAndGap 3 1

end String

namespace Int

  def sign (i:Int) :=
    if i > 0 then 1
    else if i < 0 then -1
    else 0

  theorem sign_zero : 0 = sign 0 := by rfl
  theorem sign_positive (n:Int) (gt:n > 0) : sign n = 1 := by
    simp [sign]
    simp [gt]

  theorem sign_negative (n:Int) (lt:n < 0) : sign n = -1 := by
    have ngt : ¬(n > 0) := by intro gt; cases n; repeat contradiction
    simp [sign]
    simp [ngt, lt]

end Int