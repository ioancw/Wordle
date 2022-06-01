module Lit.Wordle

open System
open Lit
open Wordles
open Fable.Import
open Fable.Core

Fable.Core.JsInterop.importSideEffects "./index.css"

type KeyBoard =
    { Top: string list
      Middle: string list
      Bottom: string list }

type Status =
    | Green
    | Yellow
    | Grey
    | Black
    | Invalid

type GuessLetter =
    { Letter: string option
      Status: Status }

type LocalStorageGameState =
    {
        Guesses: (int * (string * string) [])[]
        Solution: string
        Round: int
        State: string
    }

type Guess =
    { Letters: GuessLetter list }

    member t.AsWord() =
        let guess =
            t.Letters
            |> List.map (fun gl -> defaultArg gl.Letter "")
            |> Seq.fold (+) ""

        guess.ToUpper()

type Position = int

type GameState =
    | NotStarted
    | Won
    | Lost
    | Started

type State =
    { Wordle: string
      Guesses: (Position * Guess) list
      UsedLetters: Map<string, Status>
      State: GameState
      Round: int }

let rounds = 5

let letters = 5

let validLetterPosition n = n >= 0 && n < letters

let validRound state =
    if state.State = Lost || state.State = Won then false
    else (state.Round >= 0 && state.Round < rounds)

let emptyGuesses =
    let emptyGuess =
        0, { Letters = List.init letters (fun _ -> { Letter = None; Status = Black }) }

    List.init rounds (fun _ -> emptyGuess)

let allValidLetters n (guesses: (Position * Guess) list) =
    let (_, guess) = List.item n guesses
    let fiveLetterWords =
        words
        |> Seq.filter (fun (l: string) -> l.Length = 5)
        |> Seq.map (fun s -> s.ToUpper())
        |> Seq.toList
    let guessWord : string = guess.AsWord()

    guess.Letters |> List.forall (fun gl -> gl.Letter <> None)
    && List.contains guessWord fiveLetterWords

let keyBoard =
    { Top = [ "q"; "w"; "e"; "r"; "t"; "y"; "u"; "i"; "o"; "p" ]
      Middle = [ "a"; "s"; "d"; "f"; "g"; "h"; "j"; "k"; "l" ;]
      Bottom = [ "Ent"; "z"; "x"; "c"; "v"; "b"; "n"; "m"; "Del" ] }

// persisted game state stuff.
let gameStateKey = "gameState"

let saveGameStateLocalStorage (state: State) =
    let statusToString status = 
        match status with 
        | Yellow -> "Yellow"
        | Grey -> "Grey"
        | Black -> "Black"
        | Green -> "Green"
        | Invalid -> "Invalid"

    let stateToString state = 
        match state with 
        | NotStarted -> "Not Started"
        | Won -> "Won"
        | Lost -> "Lost"
        | Started -> "Started"

    let guessedWords =
        state.Guesses
        |> List.map (fun (position, guess) ->
            position,
            guess.Letters
            |> List.map (fun gl -> defaultArg gl.Letter "", statusToString gl.Status)
            |> List.toArray)
        |> List.toArray

    // create a local state which doesn't use record types as they don't round trip.
    Browser.WebStorage.localStorage.setItem(gameStateKey, JS.JSON.stringify {Guesses = guessedWords; Solution = state.Wordle; Round = state.Round; State  = stateToString state.State})

let loadGameStateLocalStorage () =
    Console.WriteLine("Now reading from local storoage")
    Browser.WebStorage.localStorage.getItem(gameStateKey)
    |> JS.JSON.parse :?> LocalStorageGameState


let startNewGame =
    Console.WriteLine("Starting a new game")
    let loadedStorage = loadGameStateLocalStorage ()
    let guesses = emptyGuesses

    let wordle () =
        let today = DateTime.Now
        let startDate = DateTime(2022, 5, 21)
        let todayDate = DateTime(today.Year, today.Month, today.Day)
        let differenceMilli = (todayDate - startDate).TotalMilliseconds
        let millisInDay = 60 * 60 * 24 * 1000 |> double
        let differenceDays = round (differenceMilli / millisInDay) |> int
        let index = differenceDays % wordles.Length
        wordles.[index]

    let todaysWordle = wordle ()
    
    let localState = 
        match loadedStorage.State with 
        | "Not Started" -> NotStarted
        | "Won" -> Won
        | "Lost" -> Lost 
        | "Started" -> Started




    if todaysWordle = loadedStorage.Solution && localState <> NotStarted then
        let localGuesses =
            loadedStorage.Guesses
            |> Array.map (fun (position, letters) ->
                position,
                {Letters =
                    letters |> Array.toList
                    |> List.map (fun (guessLetter, guessStatus) ->
                        let letterOption = if guessLetter = "" then None else Some guessLetter
                        let status =
                            match guessStatus with
                            | "Yellow" -> Yellow
                            | "Grey" -> Grey
                            | "Black" -> Black
                            | "Green" -> Green
                            | _ -> Invalid
                        {Letter = letterOption; Status = status})
                })
            |> Array.toList

        // for (pos, guess) in localGuesses do
            
        //     let letters =
        //         guess.Letters
        //         |> List.map (fun gl -> defaultArg gl.Letter "", gl.Status)
        //         |> List.filter (fun (l, s) -> s = Green ||
        //                 (s = Grey && not <| state.UsedLetters.ContainsKey l) ||
        //                 (s = Yellow && not <| state.UsedLetters.ContainsKey l))
        //         |> Map.ofList
        //     letters

        { Wordle = todaysWordle
          Guesses = localGuesses
          Round = loadedStorage.Round
          State = localState
          UsedLetters = Map.empty
        }
    else
        { Wordle = todaysWordle
          Guesses = guesses
          Round = 0
          State = NotStarted
          UsedLetters = Map.empty
        }

// this can be improved quite a bit - need tests though
let getAnswerMask (actual: string) (guess: string) =
    let removeFirstInstance remove fromList =
        let rec removeFirst predicate =
            function
            | [] -> []
            | h :: t when predicate h -> t //terminates
            | h :: t -> h :: removeFirst predicate t

        removeFirst (fun i -> i = remove) fromList

    let getCounts letters matchOn =
        letters
        |> List.filter (fun i -> i = matchOn)
        |> List.length

    let rec masker ls count mask =
        match (ls, count) with
        | [], _ -> mask
        | (a, g) :: t, cs ->
            if a = g then
                masker t cs (Green :: mask)
            else if Seq.contains g actual && getCounts cs g > 0 then
                masker t (removeFirstInstance g cs) (Yellow :: mask)
            else
                masker t cs (Grey :: mask)

    let notMatched zipped =
        zipped
        |> List.filter (fun (a, g) -> a <> g)
        |> List.map fst

    let letters = Seq.zip actual guess |> Seq.toList
    let masked = masker letters (notMatched letters) [] |> List.rev

    { Letters =
        Seq.zip guess masked
        |> Seq.toList
        |> List.map (fun (a, m) -> { Letter = Some(string a); Status = m }) }

let listSet list value pos =
    list |> List.mapi (fun i v -> if i = pos then value else v)

let applyLetterUpdate updateFunction state =
    if validRound state then
        let item = List.item state.Round state.Guesses
        let updated = updateFunction item
        { state with Guesses = listSet state.Guesses updated state.Round }
    else state

let submitLetter input state =
    let getNewWordStateAdd newLetter (position, guessLetters) =
        if validLetterPosition position
        then position + 1, { Letters = listSet guessLetters.Letters newLetter position }
        else position, { Letters = guessLetters.Letters }

    let addLetterToWord = getNewWordStateAdd { Letter = Some input; Status = Black }

    applyLetterUpdate addLetterToWord state

let submitDelete state =
    let getNewWordStateDelete (position, guessLetters) =
        let deletePosition = position - 1

        if validLetterPosition deletePosition
        then deletePosition, { Letters = listSet guessLetters.Letters { Letter = None; Status = Black } deletePosition }
        else position, { Letters = guessLetters.Letters }

    applyLetterUpdate getNewWordStateDelete state

let submitEnter state =
    let join p q =
        Map(Seq.concat [ (Map.toSeq p); (Map.toSeq q) ])

    let updateRoundStatus ((position, guess): Position * Guess) =
        let guessWord = guess.AsWord()
        let guessMask = guessWord |> getAnswerMask state.Wordle

        let letters =
            guessMask.Letters
            |> List.map (fun gl -> defaultArg gl.Letter "", gl.Status)
            |> List.filter (fun (l, s) -> s = Green ||
                 (s = Grey && not <| state.UsedLetters.ContainsKey l) ||
                 (s = Yellow && not <| state.UsedLetters.ContainsKey l))
            |> Map.ofList

        let updatedGuess = (position, guessMask)

        let updatedState =
            if guessWord = state.Wordle then Won
            elif state.Round = (rounds - 1) then Lost
            else Started

        let updatedUsedLetters = letters |> join state.UsedLetters
        updatedGuess, updatedState, updatedUsedLetters

    if validRound state then
        if allValidLetters state.Round state.Guesses then
            let updatedGuess, updatedState, updatedUsedLetters =
                updateRoundStatus (List.item state.Round state.Guesses)

            { state with
                Guesses = listSet state.Guesses updatedGuess state.Round
                UsedLetters = updatedUsedLetters
                State = updatedState
                Round = if updatedState = Won || updatedState = Lost then state.Round else state.Round + 1 }
        else
            let (position, letters) = state.Guesses |> List.item state.Round
            let updated = {letters with Letters = letters.Letters |> List.map (fun ls -> {ls with Status = Invalid})}
            { state with Guesses = listSet state.Guesses (position, updated) state.Round }
    else state

let boxedChar (c, status) =
    // https://tailwindcss.com/docs/border-style
    let colour =
        match status with
        | Black -> "bg-black"
        | Grey -> "bg-slate-600"
        | Green -> "bg-green-600"
        | Yellow -> "bg-yellow-400"
        | Invalid -> "bg-pink-400"

    html
        $"""
        <div class="border-solid border-transparent flex border-2 items-center rounded">
            <button class="w-14 h-14 {colour} text-center leading-none text-3xl font-bold text-white border-2 border-gray-400">{c}</button>
        </div>
    """

let keyboardChar usedLetters handler (c: string) =
    let colour =
        let letterStatus =
            match Map.tryFind (c.ToUpper()) usedLetters with
            | Some x -> x
            | None -> Black

        match letterStatus with
        | Black -> "bg-gray-400"
        | Yellow -> "bg-yellow-400"
        | Grey -> "bg-slate-600"
        | Green -> "bg-green-600"
        | Invalid -> "bg-gray-400"

    html
        $"""
        <div class="w-11 px-1 mb-2">
            <button
                @click={handler c}

                  class="flex w-full h-12 rounded {colour} items-center justify-center uppercase"
            >{c}</button>
        </div>
    """

[<LitElement("wordle-app")>]
let MatchComponent () =
    let _ = LitElement.init (fun cfg -> cfg.useShadowDom <- false)
    let startedGame = startNewGame
    let gameState, setGameState = Hook.useState startedGame

    let writeState state =
        Console.WriteLine("About to write the state")
            // or persist game state here after every loop of the game?
        saveGameStateLocalStorage gameState |> ignore
        let letterToDisplayBox =
            let getLetter (_, word) =
                let letter l =
                    let letterString = defaultArg l.Letter ""
                    letterString.ToUpper(), l.Status

                word.Letters |> List.map letter

            getLetter >> List.map boxedChar

        let onKeyClick (c: string) =
            Ev (fun ev ->
                ev.preventDefault ()

                let submitEntry =
                    match c with
                    | "Ent" -> submitEnter
                    | "Del" -> submitDelete
                    | _ -> submitLetter c

                state |> submitEntry |> setGameState
                // would we persist the game state here?
                )

        let keyboardKey = keyboardChar state.UsedLetters onKeyClick

        let outputText =
            match state.Round, state.State with
            | 0, Won -> "Jedi Knight you are"
            | 1, Won -> "Feel the force."
            | 2, Won -> "A Jedi's strength flows from the force."
            | 3, Won -> "The greatest teacher, failure is."
            | 4, Won -> "Fear is the path to the dark side."
            | _, Lost -> "This is why you must fail"
            |_ -> "Do or do not, there is no try."

        html
            $"""
            <div class="min-h-screen space-y-3 bg-black">
                <div class="flex justify-center mb-1 font-mono text-3xl text-white">
                    Hardle
                </div>
                <div class="flex justify-center mb-1">
                    {List.item 0 state.Guesses |> letterToDisplayBox}
                </div>
                <div class="flex justify-center mb-1">
                    {List.item 1 state.Guesses |> letterToDisplayBox}
                </div>
                <div class="flex justify-center mb-1">
                    {List.item 2 state.Guesses |> letterToDisplayBox}
                </div>
                <div class="flex justify-center mb-1">
                    {List.item 3 state.Guesses |> letterToDisplayBox}
                </div>
                <div class="flex justify-center mb-1">
                    {List.item 4 state.Guesses |> letterToDisplayBox}
                </div>
                <div class="flex justify-center mb-1">
                    {keyBoard.Top |> List.map keyboardKey}
                </div>
                <div class="flex justify-center mb-1">
                    {keyBoard.Middle |> List.map keyboardKey}
                </div>
                <div class="flex justify-center mb-1">
                    {keyBoard.Bottom |> List.map keyboardKey}
                </div>
                <div class="flex justify-center font-mono text-white">
                    {outputText}
                </div>
            </div>
        """


    // do we always do the same thing irrespective of state?
    match gameState.State with
    | NotStarted -> gameState |> writeState
    | Started -> gameState |> writeState
    | Won -> gameState |> writeState
    | Lost -> gameState |> writeState //then stats?
