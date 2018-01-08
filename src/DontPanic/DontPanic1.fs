module DontPanic1

open System

(* nbFloors: number of floors *)
(* width: width of the area *)
(* nbRounds: maximum number of rounds *)
(* exitFloor: floor on which the exit is found *)
(* exitPos: position of the exit on its floor *)
(* nbTotalClones: number of generated clones *)
(* nbAdditionalElevators: ignore (always zero) *)
(* nbElevators: number of elevators *)

type Direction =  | Left | Right | NoDirection | Unknown

type ElevatorInfo = {floor:int; pos:int}
type BlockedCloneInfo = {floor:int}
type CloneInfo = {floor:int; pos:int; direction:Direction}

let token = (Console.In.ReadLine()).Split [|' '|]
let nbFloors = int(token.[0])
let width = int(token.[1])
let nbRounds = int(token.[2])
let exitFloor = int(token.[3])
let exitPos = int(token.[4])
let nbTotalClones = int(token.[5])
let nbAdditionalElevators = int(token.[6])
let nbElevators = int(token.[7])

Console.Error.WriteLine("nbFloors {0}", nbFloors)
Console.Error.WriteLine("width {0}", width)
Console.Error.WriteLine("nbRounds {0}", nbRounds)
Console.Error.WriteLine("exitFloor {0}", exitFloor)
Console.Error.WriteLine("exitPos {0}", exitPos)

let elevators = Array.init nbElevators (fun _ ->
                let token1 = (Console.In.ReadLine()).Split [|' '|]
                {ElevatorInfo.floor = int(token1.[0]); pos = int(token1.[1])})


let parseDirection dirstring = 
    match dirstring with 
    | "LEFT" -> Left
    | "RIGHT" -> Right
    | "NONE" -> NoDirection
    | _ -> Unknown

let cloneInputs = Seq.initInfinite (fun _ -> 
    let t = (Console.In.ReadLine()).Split [|' '|]
    {floor = int(t.[0]); pos = int(t.[1]) ; direction = parseDirection t.[2]})

let goForTarget (targetfloor:int) (targetpos:int) cloneinfo = 
    match cloneinfo.floor with
    | f when f = targetfloor->
        match cloneinfo.direction with
        | Right when targetpos >= cloneinfo.pos -> Some "WAIT" 
        | Left  when targetpos <= cloneinfo.pos -> Some "WAIT" 
        | NoDirection -> Some "WAIT"
        | _ -> Some "BLOCK"
    | _ -> None

let oneLoop (blockedClones:List<BlockedCloneInfo>) cloneinfo =     
    Console.Error.WriteLine("floor {0}, pos {1}, direction {2}", cloneinfo.floor, cloneinfo.pos, cloneinfo.direction)

    let isCloneFloor = fun f -> f = cloneinfo.floor
    let hasBlockedOnFloor = blockedClones |> List.map (fun x -> x.floor) |> List.exists isCloneFloor 
    let elevatorOnFloor = elevators |> Array.tryFind (fun x -> isCloneFloor x.floor)

    let out =
        match hasBlockedOnFloor with 
        | true -> 
            Console.Error.WriteLine("Already blocked on this floor")
            "WAIT"
        | false ->
            match goForTarget exitFloor exitPos cloneinfo with
            | Some s -> 
                Console.Error.WriteLine("On exit floor")
                s
            | None ->
                match elevatorOnFloor with
                | None -> 
                    Console.Error.WriteLine("No elevator on floor")
                    "WAIT"
                | Some elevatorinfo -> 
                    match goForTarget elevatorinfo.floor elevatorinfo.pos cloneinfo with
                    | Some s -> 
                        Console.Error.WriteLine("Elevator on this floor")
                        s
                    | None -> "WAIT"
    
    printfn "%s" out (* action: WAIT or BLOCK *)
    if out = "WAIT" then blockedClones else {BlockedCloneInfo.floor=cloneinfo.floor}::blockedClones


let gameLoops = Seq.fold oneLoop (List<BlockedCloneInfo>.Empty) cloneInputs