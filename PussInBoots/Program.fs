open System

type mapSquare = Beach | Bridge | Forest | KingsCastle | Mill | OgreCastle | River | Road
type Item = Boots | Castle | Clothes | Dragon | Rabbit | WildShroom | WoodChoppers
type Action = GoNorth | GoSouth | GoWest | GoEast | EatItem | TakeItem 
type gameState =  { x:int; y:int
                    item: Item option
                    hasBoots:bool
                    hasRabbit:bool
                    hasDeliveredRabbit:bool
                    hasStolenMastersClothes:bool
                    isMasterInCoach: bool
                    hasKilledOgre:bool
                    hasTakenCastle:bool
                    isOnShroom:bool
                    hasFailed:bool
                     }

let readInput input =
    match input with
    | "N" -> Some GoNorth
    | "S" -> Some GoSouth
    | "W" -> Some GoWest
    | "E" -> Some GoEast
    | "TAKE" -> Some TakeItem
    | "EAT" -> Some EatItem
    | _   -> None

let takeItem gameState item =
    match item with
    | Boots -> { gameState with hasBoots = true }
    | Castle ->
        printfn "You claim the castle for your master. The king arrives at the castle and, impressed with your master and his estate, gives the lad the princess in marriage. Thereafter, you enjoy life as a great lord who runs after mice only for your own amusement."
        { gameState with hasTakenCastle = true }
    | Clothes -> { gameState with hasStolenMastersClothes = true }
    | Dragon ->
        printfn "This dragon is pretty heavy. You decide not to take it with you after all."
        gameState
    | Rabbit -> { gameState with hasRabbit = true }
    | WildShroom ->
        printfn "You take the shrooms to deal to students later, but the king's guard catch up with you and throw you in jail. That was a pretty bad idea."
        { gameState with hasFailed = true }
    | WoodChoppers ->
        printfn "These guys don't want to come with you. Put them down right now!"
        gameState

let eatItem gameState item =
    match item with
    | Boots -> 
        printf "You have eaten the boots. You make me (and yourself) sick. You will never be Puss in Boots now, well done. Go back to sleep you lazy cat!"
        { gameState with hasFailed = true }
    | Castle ->
        printfn "Don't be silly, you can't eat a castle."
        gameState
    | Clothes ->
        printfn "Yuck these clothes taste like your master. You are a pretty sick cat, you know that?"
        gameState
    | Dragon -> 
        printf "You have eaten a dragon. It took you a week."
        gameState
    | Rabbit -> 
        printf "You have eaten a rabbit. Next time you might want to kill it first."
        gameState
    | WildShroom -> 
        printf "You have eaten a wild mushroom. Watch out for dragons."
        gameState
    | WoodChoppers ->
        printfn "Don't mess with these guys, they are pretty tough!"
        gameState
let getValueOrDefault defaultValue optionValue = defaultArg optionValue defaultValue

let takeAction gameState action =
    match action with
    | GoNorth -> if gameState.x > 0 then {gameState with x = gameState.x-1} else gameState
    | GoSouth -> if gameState.x < 9 then {gameState with x = gameState.x+1} else gameState
    | GoWest  -> if gameState.y > 0 then {gameState with y = gameState.y-1} else gameState
    | GoEast  -> if gameState.y < 6 then {gameState with y = gameState.y+1} else gameState
    | TakeItem -> gameState.item |> Option.map (takeItem gameState) |> getValueOrDefault gameState
    | EatItem -> gameState.item |> Option.map (eatItem gameState) |> getValueOrDefault gameState

let map = [[Forest;Forest;Forest;Forest;    Forest;Road;Forest]
           [Forest;Forest;Forest;Forest;    Forest;Road;Forest]
           [Forest;Forest;Forest;OgreCastle;Forest;Road;Forest]
           [Forest;Forest;Forest;Forest;    Forest;Road;Forest]
           [Forest;Forest;Forest;Forest;    Forest;Road;Forest]
           [Forest;River; River; River;    River;Bridge;Forest]
           [Forest;Forest;Mill;  Forest;     Beach;Road;Forest]
           [Forest;Forest;Forest;Forest;    Forest;Road;Forest]
           [Forest;KingsCastle;     Road;Road;Road;Road;Forest]
           [Forest;Forest;Forest;Forest;    Forest;Road;Forest]]

let printMap (x: int, y: int): unit =
    Console.ForegroundColor <- ConsoleColor.Yellow 
    let printCell (isHere, cell) =
        match cell with 
        | Beach -> "    Beach    "
        | Bridge -> "   Bridge    "
        | Forest -> "   Forest    "
        | KingsCastle -> "King's Castle"
        | Mill -> "    Mill     "
        | OgreCastle -> "Ogre's Castle"
        | River -> "    River    "
        | Road -> "    Road     "
        |> fun s -> 
            if isHere then
                Console.ForegroundColor <- ConsoleColor.Green 
                printf "%s" (s.ToUpper()) 
                Console.ForegroundColor <- ConsoleColor.Yellow
            else 
                printf "%s" s

    let printRow (row: (bool * mapSquare) list) = 
        row |> List.iter (fun elem -> (printCell elem))

    map 
    |> List.mapi (fun i row ->
        row |> List.mapi (fun j cell -> if i = x && j = y then (true, cell) else (false, cell))
    )
    |> List.iter (fun row -> 
        printRow row
        printfn ""
        )


let randomNumberGenerator = System.Random()
let randomNumber _ = randomNumberGenerator.Next(100)

let describeItem gameState =
    match gameState.item with
    | None              -> ""
    | Some Boots        -> "There are some boots."
    | Some Castle       -> "This castle doesn't seem to belong to anyone now..."
    | Some Clothes      -> "There are some clothes. They belong to your master, he is having a swim."
    | Some Dragon       -> "There is a dragon!"
    | Some Rabbit       -> "There is a rabbit."
    | Some WildShroom   -> "There are some wild mushrooms."
    | Some WoodChoppers -> "There are some woodcutters."

let describeRoom gameState =
    match map.[gameState.x].[gameState.y] with
    | Forest -> "\nYou are in a forest." 
    | Mill -> "\nYou are in a mill."
    | Road -> "\nYou are on a road."
    | OgreCastle -> "\nYou are in the ogre castle."
    | KingsCastle -> "\nYou are in the king's castle."
    | River -> "\nYou are in a river."
    | Beach -> "\nYou are on a beach."
    | Bridge -> "\nYou are on a bridge."

let setupForest gameState =
    let r = randomNumber()
    let item =   
        match r with
        | x when x < 40 -> None
        | x when x < 65 -> Some Rabbit
        | x when x < 75 -> Some WildShroom
        | _ ->
            if gameState.isOnShroom then Some Dragon else Some WoodChoppers
    { gameState with item = item }

let setupMill gameState = 
    if gameState.hasBoots then { gameState with item = None }
    else { gameState with item = Some Boots }

let setupRoom gameState =
    match map.[gameState.x].[gameState.y] with
    | Forest -> setupForest gameState
    | Mill -> setupMill gameState
    | Road -> { gameState with item = None }
    | OgreCastle -> if gameState.hasKilledOgre && not gameState.hasTakenCastle then { gameState with item = Some Castle } else { gameState with item = None }
    | KingsCastle -> { gameState with item = None }
    | River -> { gameState with item = None }
    | Beach -> if gameState.hasStolenMastersClothes then { gameState with item = None } else { gameState with item = Some Clothes }
    | Bridge -> { gameState with item = None }

let autoStoryAction gameState =
    match map.[gameState.x].[gameState.y] with
    | KingsCastle ->
        match gameState.hasBoots, gameState.hasRabbit with
        | true, true -> 
            printfn "You give the King the rabbit. He is well pleased."
            { gameState with hasDeliveredRabbit = true }
        | _, false -> 
            printfn "If only you had something to give the king as a gift..."
            gameState
        | false, true -> 
            printfn "You are not properly dressed! The king does not like to see naked cats. The guards take you away to the cells."
            { gameState with hasFailed = true }

    | Beach -> 
        match gameState.hasStolenMastersClothes, gameState.hasDeliveredRabbit, gameState.isMasterInCoach with
        | true, true, false ->
            printfn "The king is driving past the river. You call out to him and tell him your master's clothes have been stolen by brigands. He remembers your kind gift and stops to help."
            { gameState with isMasterInCoach = true }
        | true, false, _ ->
            printfn "Your master is very angry with you. He drowns you in the river and puts his clothes back on. He also nicks your boots. They look good on him."
            { gameState with hasFailed = true }
        | _, _, _ ->
            gameState
    
    | OgreCastle ->
        match gameState.isMasterInCoach, gameState.hasKilledOgre with
        | true,  false -> printfn "You see an ogre. He's showing off his metamorphosis ability. You trick him into turning into a mouse. You then eat the mouse."
                          {gameState with hasKilledOgre = true; item = Some Castle}
        | false, false -> printfn "You see an ogre. He's showing off his metamorphosis ability. You trick him into turning into a mouse. He turns into a lion instead and eats you."
                          {gameState with hasFailed = true}
        | _, _ -> gameState
    | _ -> gameState

let mutable playerPos = { x=6
                          y=2
                          item = Some Boots
                          hasBoots = false
                          hasRabbit = false
                          hasDeliveredRabbit = false
                          hasStolenMastersClothes = false
                          hasKilledOgre = false
                          isMasterInCoach = false
                          hasTakenCastle = false
                          isOnShroom = false
                          hasFailed = false }

printfn "BUMP! You have been woken up from sleep... by the old miller tripping over you and falling off the mill's rooftop. You are annoyed. Still sleepy, you slowly make your way downstairs. The will is being read. The eldest son gets the mill, the middle son gets the mules and the youngest son - your master - gets you. It's time for some action."
while not (playerPos.hasTakenCastle || playerPos.hasFailed) do
    Console.Clear()
    printMap (playerPos.x, playerPos.y)
    Console.ForegroundColor <- ConsoleColor.Cyan

    printfn "%s" (playerPos |> describeRoom)
    playerPos <- (playerPos |> setupRoom)
    playerPos <-  playerPos |> autoStoryAction
    printfn "%s" (playerPos |> describeItem)
    printfn "What do you want to do? You can go in a direction (N, W, E, S) or you can TAKE or EAT."

    let x = Console.ReadLine().ToUpper() |> readInput |> Option.map (takeAction playerPos)
    
    match x with
    | None -> printfn "I'm confused."
    | Some gs -> playerPos <- gs

printfn "Press any key..."
Console.ReadLine() |> ignore
if playerPos.hasFailed then printfn "\nYou LOSE. I hope you learned your lesson."
else printfn "\nBUMP! You have been woken up from sleep... by a speed bump. You are a cat in a boot of a car. You are annoyed with your master - Dr. Schrödinger - and his shitty driving skill."
Console.ReadLine() |> ignore