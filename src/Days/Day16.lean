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

-- Dijkstra's algorithm & saving iteration count
partial def findDistancesToAll (caves:Caves) (start:ValveName) : Std.HashMap ValveName Minutes :=
  loop 0 Std.HashMap.empty [start]
  where
  loop (minute:Minutes) (closedSet:Std.HashMap ValveName Minutes)
    | [] => closedSet
    | openSet =>
      let closedSet := openSet.foldl (·.insert · minute) closedSet

      openSet
      |>.bind (Cave.tunnels ∘ caves.find!)
      |>.filter (!closedSet.contains ·)
      |> loop (minute + 1) closedSet

partial def getBestFlowRate
  (getDistance:ValveName × ValveName → Minutes)
  (getValveRate:ValveName → ValveRate)
  (relevantValves:List ValveName)
  (minutesLeft:Minutes)
  (currentValveName:ValveName)
  : FlowRate :=
  loop 0 [] minutesLeft currentValveName
  where
    loop (totalFlowRate:FlowRate) (visitedValves:List ValveName) (minutesLeft:Minutes) (currentValveName:ValveName) : FlowRate :=
      relevantValves
      |>.filter (!visitedValves.contains ·)
      |>.map (fun valveName =>
        let distance := getDistance (currentValveName, valveName)
        let minutesLeft := minutesLeft - (distance + 1)

        if minutesLeft = 0 then
          totalFlowRate
        else
          loop
            (totalFlowRate + getValveRate valveName * minutesLeft)
            (valveName :: visitedValves)
            minutesLeft
            valveName
      )
      |>.foldl max totalFlowRate

def mkGetBestFlowRate (caves:Caves) : Minutes → ValveName → List ValveName → FlowRate :=
  let shortestDistances : Std.HashMap (ValveName × ValveName) Minutes :=
    caves.toList
    |>.map (·.1)
    |>.bind (fun (sourceValveName:ValveName) =>
      sourceValveName
      |> findDistancesToAll caves
      |>.toList
      |>.map (fun (targetValveName, minutes) => ((sourceValveName, targetValveName), minutes) )
    )
    |> Std.HashMap.ofList

  let getDistance := shortestDistances.find!
  let getValveFlowRate := (caves.find! · |>.flowRatePerMinute)
  let get := getBestFlowRate getDistance getValveFlowRate
  fun minutes current relevantValves => get relevantValves minutes current

def part1 (ls:List String) :=
  let caves := ls |> Parse.input!

  let getBestFlowRate := mkGetBestFlowRate caves 30 "AA"

  caves.toList
  |>.filter (·.2.flowRatePerMinute > 0)
  |>.map (·.1)
  |>.sort
  |> getBestFlowRate

def part2 (ls:List String) : FlowRate :=
  let caves := ls |> Parse.input!

  let getBestFlowRate := mkGetBestFlowRate caves 26 "AA"

  let relevantValveNames :=
    caves.toList
    |>.filter (·.2.flowRatePerMinute > 0)
    |>.map (·.1)
    |>.sort

  List.range relevantValveNames.length
  |>.map (fun n =>
    relevantValveNames
    |>.allSubsetsN n
    |>.map (fun (valveNames:List ValveName) =>
      (valveNames, relevantValveNames.removeAll valveNames)
    )
    |>.map (fun (myVales, elephantsValves) =>
      getBestFlowRate myVales + getBestFlowRate elephantsValves
    )
    |>.maximum?
    |>.get!
  )
  |>.maximum?
  |>.get!

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