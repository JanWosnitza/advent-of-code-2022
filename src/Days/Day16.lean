import LeanExt
import Std

namespace Day16

abbrev ValveRate := Nat
abbrev ValveName := String

structure Cave where
  flowRatePerMinute : ValveRate
  tunnels : List ValveName
deriving Inhabited, Repr

abbrev Caves := Std.HashMap ValveName Cave

namespace Parse
  def cave! (s:String) : ValveName × Cave :=
    let flatSplit (ls:List String) (s:String) := ls.bind  (·.splitOn s)
    match [";", " ", "=", ","].foldl flatSplit [s] |>.filter (·!="") with
    | "Valve" :: sName :: "has" :: "flow" :: "rate" :: sRate :: "tunnel" :: "leads" :: "to" :: "valve" :: sTunnel :: []
      =>
      ( sName
      , { flowRatePerMinute := sRate.toNat!
        , tunnels := [sTunnel]
        }
      )

    | "Valve" :: sName :: "has" :: "flow" :: "rate" :: sRate :: "tunnels" :: "lead" :: "to" :: "valves" :: sTunnels
      =>
      ( sName
      , { flowRatePerMinute := sRate.toNat!
        , tunnels := sTunnels
        }
      )
    | _ => panic! ""

  def input! (ls:List String) : Caves :=
    ls
    |>.map cave!
    |> Std.HashMap.ofList

end Parse

abbrev Minutes := Nat
abbrev FlowRate := Nat

partial def findShortestDistance (caves:Caves) (start:ValveName) : List (ValveName × Minutes) :=
  let rec loop (minute:Minutes) (closedSet:Std.HashMap ValveName Minutes) : List ValveName → List (ValveName × Minutes)
    | [] => closedSet.toList
    | openSet =>
      let closedSet := openSet.foldl (·.insert · minute) closedSet

      openSet
      |>.bind (Cave.tunnels ∘ caves.find!)
      |>.filter (!closedSet.contains ·)
      |> loop (minute + 1) closedSet

  [start]
  |> loop 0 Std.HashMap.empty

partial def partX (workers:List Minutes) (ls:List String) :=
  let caves := ls |> Parse.input!

  let interestingValveNames :=
    caves.toList
    |>.filter (·.2.flowRatePerMinute > 0)
    |>.map (·.1)
    |>.sort

  let shortestDistances : Std.HashMap (ValveName × ValveName) Minutes :=
    ("AA" :: interestingValveNames)
    |>.bind (fun (sourceValveName:ValveName) =>
      sourceValveName
      |> findShortestDistance caves
      |>.filter (interestingValveNames.contains ·.1)
      |>.map (fun (targetValveName, minutes) => ((sourceValveName, targetValveName), minutes) )
    )
    |> Std.HashMap.ofList

  let rec loop (totalFlowRate:FlowRate) (visitedValves:List ValveName) : List (Minutes × ValveName) → FlowRate
    | (minutesLeft, currentValveName) :: restValveNames =>
      interestingValveNames
      |>.filter (!visitedValves.contains ·)
      |>.map (fun valveName =>
        let minutesMeContinue :=
          let cave := caves.find! valveName
          let distance := shortestDistances.find! (currentValveName, valveName)
          let minutesLeft := minutesLeft - (distance + 1)

          if minutesLeft = 0 then
            -- this worker is done
            totalFlowRate
          else
            loop
              (totalFlowRate + cave.flowRatePerMinute * minutesLeft)
              (valveName :: visitedValves)
              ((minutesLeft, valveName) :: restValveNames)

        let minutesMeStop := loop totalFlowRate visitedValves restValveNames

        max minutesMeContinue minutesMeStop
      )
      |>.foldl max totalFlowRate
    | _ => totalFlowRate
  
  workers
  |>.map (·, "AA")
  |> loop 0 []

partial def part1 := partX [30]

partial def part2 := partX [26, 26]

def testInput :=
  [ "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
  , "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
  , "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
  , "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
  , "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
  , "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
  , "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
  , "Valve HH has flow rate=22; tunnel leads to valve GG"
  , "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
  , "Valve JJ has flow rate=21; tunnel leads to valve II"
  ]
#eval 1651 <== part1 testInput
#eval 1707 <== part2 testInput

end Day16