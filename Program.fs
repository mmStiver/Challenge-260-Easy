//[2016-03-28] Challenge #260 [Easy] Garage Door Opener

//You just got a new garage door installed by the Automata™ Garage Door Company. 
//You are having a lot of fun playing with the remote clicker, 
//opening and closing the door, scaring your pets and annoying the neighbors.
//The clicker is a one-button remote that works like this:
//If the door is OPEN or CLOSED, clicking the button will cause the door to move,
// until it completes the cycle of opening or closing.
//Door: 
//Closed -> Button clicked -> 
//Door: Opening -> Cycle complete 
//-> Door: Open.
//If the door is currently opening or closing, clicking the button will make the 
//door stop where it is. When clicked again, the door will go the opposite
// direction, until complete or the button is clicked again.

//We will assume the initial state is CLOSED
type GarageStatus = | Open | Closed | Opening | Closing | StoppedClosing | StoppedOpening

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

let InputGarageCommand gs cmd = 
    match cmd with
    | "button_clicked" -> ClickButton gs
    | "cycle_complete" -> FinishCycle  gs

let printGarageStatus gs =
    match gs with
    | Open -> printfn "Door: OPEN"
    | Opening -> printfn "Door: OPENING"
    | Closed-> printfn "Door: CLOSED"
    | Closing -> printfn "Door: CLOSING"
    | StoppedClosing -> printfn "Door: STOPPED_WHILE_CLOSING"
    | StoppedOpening -> printfn "Door: STOPPED_WHILE_OPENING"

let printGarageCommand (c:string) = 
    match c with
    |"button_clicked" -> printfn "> Button clicked."
    |"cycle_complete" -> printfn "> Cycle complete."
    
let handleCommands list gs =
        let rec loop cmds gs =
            match cmds with
            |[] -> null
            | head::tail -> 
                printGarageCommand head
                let ns = InputGarageCommand gs head
                printGarageStatus ns
                loop tail ns
        printGarageStatus gs
        loop list gs

[<EntryPoint>]
let main argv = 
    let commands = ["button_clicked"; "cycle_complete"; "button_clicked"; "button_clicked"; "button_clicked"; "button_clicked"; "button_clicked"; "cycle_complete"; ] 
    handleCommands commands GarageStatus.Closed       
    
    System.Console.ReadKey() |> ignore
    
    0 // return an integer exit code
