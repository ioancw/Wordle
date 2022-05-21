module Lit.Wordle

open System
open Lit

Fable.Core.JsInterop.importSideEffects "./index.css"

type KeyBoard = {Top: string list; Middle: string list; Bottom: string list}

type LetterStatus =
    | Green
    | Yellow
    | Grey
    | Black

// is it better to have as an array intead?
type Guess =
    { Letter1: string option * LetterStatus
      Letter2: string option * LetterStatus
      Letter3: string option * LetterStatus
      Letter4: string option * LetterStatus
      Letter5: string option * LetterStatus }

    member t.AsWord() = 
        let letter (word, _) = defaultArg word ""

        let guess = 
            [| letter t.Letter1
               letter t.Letter2
               letter t.Letter3
               letter t.Letter4
               letter t.Letter5 |]
            |> Seq.fold (+) ""
        guess.ToUpper()

type Position = int

type StartedState =
    { Wordle: string
      Guess1: Position * Guess
      Guess2: Position * Guess
      Guess3: Position * Guess
      Guess4: Position * Guess
      Guess5: Position * Guess
      UsedLetters: Map<string, LetterStatus>
      Round: int }

type GameState =
    | NotStarted
    | Won
    | Lost
    | Started of StartedState

let emptyGuess =
    1,
    { Letter1 = None, Black
      Letter2 = None, Black
      Letter3 = None, Black
      Letter4 = None, Black
      Letter5 = None, Black }

let keyBoard =
    {
        Top = [ "q"; "w"; "e"; "r"; "t"; "y"; "u"; "i"; "o"; "p" ]
        Middle = [ "a"; "s"; "d"; "f"; "g"; "h"; "j"; "k"; "l" ]
        Bottom = [ "Ent"; "z"; "x"; "c"; "v"; "b"; "n"; "m"; "Del" ]
    }

let startNewGame () =
    let wordles =
        [
            "BORED"
            "FLAME"
            "TRIAL"
            "CRIME"
            "BLAZE"
            "SUCKS"
            "QUACK"
            "NIGHT"
            "GRIME"
            "ARISE"
            "UNLIT"
            "BRINE"
            "WRUNG"
            "POTTY"
            "LANDS"
            "BRAVE"
            "FERAL"
            "HORSE"
            "MAISE"
            "GRAZE"
            "FRAME"
            "BLUNT"
            "GRUNT"
            "BRAIN"
            "FRONT"
            "BROKE"
            "FLIPS"
            "MAPLE"
            "WHITE"
            "HOVER"
            "TRAIL"
            "ROVER"
            "RIGHT"
            "WRONG"
        ]

    let wordle = 
        let today = DateTime.Now
        let startDate = DateTime(2022, 5, 21)
        let todayDate = DateTime(today.Year, today.Month, today.Day)
        let differenceMilli = (todayDate - startDate).Milliseconds
        let millisInDay = 60 * 60 * 24 * 1000
        let differenceDays = differenceMilli / millisInDay
        let index = differenceDays % wordles.Length
        wordles.[index]

    { Wordle = wordle
      Guess1 = emptyGuess
      Guess2 = emptyGuess
      Guess3 = emptyGuess
      Guess4 = emptyGuess
      Guess5 = emptyGuess
      Round = 1
      UsedLetters = Map.empty }

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
    let associated = Seq.zip guess masked |> Seq.toList

    let associated' =
        associated
        |> List.map (fun (a, m) -> Some(string a), m)

    { Letter1 = associated'.[0]
      Letter2 = associated'.[1]
      Letter3 = associated'.[2]
      Letter4 = associated'.[3]
      Letter5 = associated'.[4] }

let submitLetter input x =
    let getNewWordStateAdd newLetter (position, word) =
        let advancedPosition = position + 1
        match position with
        | 1 -> advancedPosition, { word with Letter1 = newLetter }
        | 2 -> advancedPosition, { word with Letter2 = newLetter }
        | 3 -> advancedPosition, { word with Letter3 = newLetter }
        | 4 -> advancedPosition, { word with Letter4 = newLetter }
        | 5 -> advancedPosition, { word with Letter5 = newLetter }
        | _ -> position, word

    let addLetterToWord = getNewWordStateAdd (Some input, Black)

    match x.Round with
    | 1 -> { x with Guess1 = addLetterToWord x.Guess1 }
    | 2 -> { x with Guess2 = addLetterToWord x.Guess2 }
    | 3 -> { x with Guess3 = addLetterToWord x.Guess3 }
    | 4 -> { x with Guess4 = addLetterToWord x.Guess4 }
    | 5 -> { x with Guess5 = addLetterToWord x.Guess5 }
    | _ -> failwithf "There is no higher purpose"

let submitDelete x =
    let getNewWordStateDelete (position, word) =
        let deletePosition = position - 1
        // when adding or deleting, the Status will always be Black, as it's yet to be evaluated
        match deletePosition with
        | 1 -> deletePosition, { word with Letter1 = None, Black }
        | 2 -> deletePosition, { word with Letter2 = None, Black }
        | 3 -> deletePosition, { word with Letter3 = None, Black }
        | 4 -> deletePosition, { word with Letter4 = None, Black }
        | 5 -> deletePosition, { word with Letter5 = None, Black }
        | _ -> position, word

    match x.Round with
    | 1 -> { x with Guess1 = getNewWordStateDelete x.Guess1 }
    | 2 -> { x with Guess2 = getNewWordStateDelete x.Guess2 }
    | 3 -> { x with Guess3 = getNewWordStateDelete x.Guess3 }
    | 4 -> { x with Guess4 = getNewWordStateDelete x.Guess4 }
    | 5 -> { x with Guess5 = getNewWordStateDelete x.Guess5 }
    | _ -> failwithf "There is no higher purpose"

let submitEnter x =
    let join p q = Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])
    let advanceRound = x.Round + 1
    let updateRoundStatus ((position, guess): Position * Guess) =
        let guessWord = guess.AsWord()
        let guessMask = guessWord |> getAnswerMask x.Wordle
        let letters =
            let letterStatus (word, status) = defaultArg word "", status
            [
                letterStatus guessMask.Letter1
                letterStatus guessMask.Letter2
                letterStatus guessMask.Letter3
                letterStatus guessMask.Letter4
                letterStatus guessMask.Letter5
            ]
            |> List.filter (fun (l, s) -> s = Green || s = Grey && not <| x.UsedLetters.ContainsKey l)
            |> Map.ofList
        let updatedGuess = (position, guessMask)
        let updatesState = if guessWord = x.Wordle then Won else Started x
        let updatedUsedLetters = letters |> join x.UsedLetters
        updatedGuess, updatesState, updatedUsedLetters

    match x.Round with
    | 1 ->
        let updatedGuess, updatesState, updatedUsedLetters = updateRoundStatus x.Guess1
        {x with Guess1 = updatedGuess; UsedLetters = updatedUsedLetters; Round = advanceRound}
    | 2 ->
        let updatedGuess, updatesState, updatedUsedLetters = updateRoundStatus x.Guess2
        {x with Guess2 = updatedGuess; UsedLetters = updatedUsedLetters; Round = advanceRound}
    | 3 ->
        let updatedGuess, updatesState, updatedUsedLetters = updateRoundStatus x.Guess3
        {x with Guess3 = updatedGuess; UsedLetters = updatedUsedLetters; Round = advanceRound}
    | 4 ->
        let updatedGuess, updatesState, updatedUsedLetters = updateRoundStatus x.Guess4
        {x with Guess4 = updatedGuess; UsedLetters = updatedUsedLetters; Round = advanceRound}
    | 5 ->
        let updatedGuess, updatesState, updatedUsedLetters = updateRoundStatus x.Guess5
        {x with Guess5 = updatedGuess; UsedLetters = updatedUsedLetters; Round = advanceRound}
    | _ -> x

let boxedChar (c, status) =
    // https://tailwindcss.com/docs/border-style
    let colour =
        match status with
        | Black -> "bg-slate-800"
        | Grey -> "bg-slate-600"
        | Green -> "bg-green-600"
        | Yellow -> "bg-yellow-400"
    html
        $"""
        <div class="border-solid border-transparent flex border-2 items-center rounded">
            <button class="w-14 h-14 {colour} text-center leading-none text-3xl font-bold text-white">{c}</button>
        </div>
    """

let keyboardChar usedLetters handler c =
    // class="block w-full h-12 rounded bg-slate-300 hover:bg-slate-300 active:bg-slate-400 text-center leading-none text-white"
    let colour =
        let letterStatus =
            match Map.tryFind c usedLetters with
            | Some x -> x
            | None -> Black
        match letterStatus with
        | Black | Yellow  -> "bg-gray-300"
        | Grey -> "bg-slate-600"
        | Green -> "bg-green-600"

    html
        $"""
        <div class="w-11 px-1 mb-2">
            <button
                @click={handler c}
                class="block w-full h-12 rounded {colour} items-center justify-center uppercase"
            >{c}</button>
        </div>
    """

[<LitElement("math-app")>]
let MatchComponent () =
    let _ = LitElement.init (fun cfg -> cfg.useShadowDom <- false)
    let gameState, setGameState = Hook.useState NotStarted

    let writeState state = 
        let letterToDisplayBox =
            let getLetter (_, word) =
                let letter (word, status) =
                    let letterString = defaultArg word ""
                    letterString.ToUpper(), status

                [ letter word.Letter1
                  letter word.Letter2
                  letter word.Letter3
                  letter word.Letter4
                  letter word.Letter5 ]

            getLetter >> List.map boxedChar

        let onKeyClick state (c: string) =
            Ev (fun ev ->
                ev.preventDefault ()
                let submitEntry =
                    match c with
                    | "Ent" -> submitEnter
                    | "Del" -> submitDelete
                    | _ -> submitLetter c

                state |> submitEntry |> Started |> setGameState)

        let keyboardKey = keyboardChar state.UsedLetters (onKeyClick state)
        
        html
            $"""
            <div class="space-y-4">
                <div class="flex flex-row justify-center">
                    "Wordle"
                </div>
                <div class="flex flex-row justify-center">
                    {state.Guess1 |> letterToDisplayBox}
                </div>
                <div class="flex flex-row justify-center">
                    {state.Guess2 |> letterToDisplayBox}
                </div>
                <div class="flex flex-row justify-center">
                    {state.Guess3 |> letterToDisplayBox}
                </div>
                <div class="flex flex-row justify-center">
                    {state.Guess4 |> letterToDisplayBox}
                </div>
                <div class="flex flex-row justify-center">
                    {state.Guess5 |> letterToDisplayBox}
                </div>
                <div class="flex flew-row justify-center">
                    {keyBoard.Top |> List.map keyboardKey}
                </div>
                <div class="flex flew-row justify-center mb-1">
                    {keyBoard.Middle |> List.map keyboardKey}
                </div>
                <div class="flex flew-row justify-center">
                    {keyBoard.Bottom |> List.map keyboardKey}
                </div>
            </div>
        """

    match gameState with
    | NotStarted ->
        let newGame = startNewGame ()
        newGame |> Started |> setGameState
        newGame |> writeState 

    | Started startedState ->
        writeState startedState
    | Won ->
        startNewGame () |> writeState
    | Lost -> 
        startNewGame () |> writeState

