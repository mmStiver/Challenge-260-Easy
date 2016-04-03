//[2016-03-28] Challenge #260 [Easy] Garage Door Opener

//We will assume the initial state is CLOSED
// Undefined states not defined in the challenge will throw a runtime error.
type GarageStatus = | Open | Closed | Opening | Closing | StoppedClosing | StoppedOpening | EmergencyOpening | OpenBlocked

let ClickButton gs =
    match gs with
    | Open | StoppedOpening ->  GarageStatus.Closing
    | Opening ->   GarageStatus.StoppedOpening
    | Closed| StoppedClosing ->   GarageStatus.Opening
    | Closing ->   GarageStatus.StoppedClosing

let FinishCycle gs = 
    match gs with
    | Opening ->  GarageStatus.Open
    | Closing ->  GarageStatus.Closed
    | EmergencyOpening -> GarageStatus.OpenBlocked

let InputGarageCommand gs cmd = 
    match cmd with
    | "button_clicked" -> match gs with
                          | EmergencyOpening | OpenBlocked -> gs
                          |_ -> ClickButton gs
    | "cycle_complete" -> FinishCycle  gs
    | "block_detected" -> GarageStatus.EmergencyOpening
    | "block_cleared"  -> GarageStatus.Open

let printGarageStatus gs =
    match gs with
    | Open -> printfn "Door: OPEN"
    | Opening -> printfn "Door: OPENING"
    | Closed-> printfn "Door: CLOSED"
    | Closing -> printfn "Door: CLOSING"
    | StoppedClosing -> printfn "Door: STOPPED_WHILE_CLOSING"
    | StoppedOpening -> printfn "Door: STOPPED_WHILE_OPENING"
    | EmergencyOpening -> printfn "Door: EMERGENCY_OPENING"
    | OpenBlocked -> printfn "Door: OPEN_BLOCKED"

let printGarageCommand (c:string) = 
    match c with
    | "button_clicked" -> printfn "> Button clicked."
    | "cycle_complete" -> printfn "> Cycle complete."
    | "block_detected" -> printfn "> Block detected!"
    | "block_cleared"  -> printfn "> Block cleared"

let handleCommands loopfn list gs =
        printGarageStatus gs
        loopfn list gs

let rec baseLoop cmds gs =
    match cmds with
    |[] -> null
    | head::tail -> 
        printGarageCommand head
        let ns = InputGarageCommand gs head
        printGarageStatus ns
        baseLoop tail ns

[<EntryPoint>]
let main argv = 
    let commands = ["button_clicked"; "cycle_complete"; "button_clicked"; "button_clicked"; "button_clicked"; "button_clicked"; "button_clicked"; "cycle_complete"; ] 
    let bonusCommands = ["button_clicked";"cycle_complete";"button_clicked";"block_detected";"button_clicked";"cycle_complete";"button_clicked";"block_cleared";"button_clicked";"cycle_complete";]
    handleCommands baseLoop commands GarageStatus.Closed       
    
    printfn ""
    System.Console.ReadKey() |> ignore
    printfn "Bonus: "

    
    handleCommands baseLoop bonusCommands GarageStatus.Closed
    System.Console.ReadKey() |> ignore
    
    0 // return an integer exit code
