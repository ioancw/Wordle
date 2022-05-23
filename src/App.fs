module Lit.Wordle

open System
open Lit

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

type GuessLetter =
    { Letter: string option
      Status: Status }

type Guess =
    { Letters: GuessLetter list }

    member t.AsWord() =
        let guess =
            t.Letters
            |> List.map (fun gl -> defaultArg gl.Letter "")
            |> Seq.fold (+) ""

        guess.ToUpper()

let rounds = 5

let letters = 5

let validLetterPosition n = n >= 0 && n < letters

let validRoundNumber n = n >= 0 && n < rounds

let allValidLetters n guesses =
    let (_, guess) = List.item n guesses

    guess.Letters
    |> List.forall (fun gl -> gl.Letter <> None)

let emptyGuesses =
    let emptyGuess =
        0, { Letters = List.init letters (fun _ -> { Letter = None; Status = Black }) }

    List.init rounds (fun _ -> emptyGuess)

type Position = int

type GameState =
    | NotStarted
    | Won
    | Lost
    | Started

type StartedState =
    { Wordle: string
      Guesses: (Position * Guess) list
      UsedLetters: Map<string, Status>
      State: GameState
      Round: int }

let keyBoard =
    { Top =
        [ "q"
          "w"
          "e"
          "r"
          "t"
          "y"
          "u"
          "i"
          "o"
          "p" ]
      Middle =
        [ "a"
          "s"
          "d"
          "f"
          "g"
          "h"
          "j"
          "k"
          "l" ]
      Bottom =
        [ "Ent"
          "z"
          "x"
          "c"
          "v"
          "b"
          "n"
          "m"
          "Del" ] }

let startNewGame () =
    let wordles =
        [ "BORED"
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
          "WRONG" ]

    let wordle () =
        let today = DateTime.Now
        let startDate = DateTime(2022, 5, 21)
        let todayDate = DateTime(today.Year, today.Month, today.Day)
        let differenceMilli = (todayDate - startDate).TotalMilliseconds
        let millisInDay = 60 * 60 * 24 * 1000 |> double
        let differenceDays = round (differenceMilli / millisInDay) |> int
        let index = differenceDays % wordles.Length
        wordles.[index]

    { Wordle = wordle ()
      Guesses = emptyGuesses
      Round = 0
      State = NotStarted
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

    { Letters =
        Seq.zip guess masked
        |> Seq.toList
        |> List.map (fun (a, m) -> { Letter = Some(string a); Status = m }) }

let listSet list value pos =
    list
    |> List.mapi (fun i v -> if i = pos then value else v)

let applyLetterUpdate updateFunction x =
    match validRoundNumber x.Round with
    | true ->
        let item = List.item x.Round x.Guesses
        let updated = updateFunction item
        { x with Guesses = listSet x.Guesses updated x.Round }
    | false -> failwithf "There is no higher purpose"

let submitLetter input x =
    let getNewWordStateAdd newLetter (position, guessLetters) =
        if validLetterPosition position then
            position + 1, { Letters = listSet guessLetters.Letters newLetter position }
        else
            position, { Letters = guessLetters.Letters }

    let addLetterToWord = getNewWordStateAdd { Letter = Some input; Status = Black }

    applyLetterUpdate addLetterToWord x

let submitDelete x =
    let getNewWordStateDelete (position, guessLetters) =
        let deletePosition = position - 1
        let deletedLetter = { Letter = None; Status = Black }

        if validLetterPosition deletePosition then
            deletePosition, { Letters = listSet guessLetters.Letters deletedLetter deletePosition }
        else
            position, { Letters = guessLetters.Letters }

    applyLetterUpdate getNewWordStateDelete x

let submitEnter x =
    let join p q =
        Map(
            Seq.concat [ (Map.toSeq p)
                         (Map.toSeq q) ]
        )

    let advanceRound = x.Round + 1

    let updateRoundStatus ((position, guess): Position * Guess) =
        let guessWord = guess.AsWord()
        let guessMask = guessWord |> getAnswerMask x.Wordle

        let letters =
            guessMask.Letters
            |> List.map (fun gl -> defaultArg gl.Letter "", gl.Status)
            |> List.filter (fun (l, s) ->
                s = Green
                || s = Grey && not <| x.UsedLetters.ContainsKey l)
            |> Map.ofList

        let updatedGuess = (position, guessMask)

        let updatesState =
            if guessWord = x.Wordle then
                Won
            else
                Started

        let updatedUsedLetters = letters |> join x.UsedLetters
        updatedGuess, updatesState, updatedUsedLetters

    if validRoundNumber x.Round
       && allValidLetters x.Round x.Guesses then
        let updatedGuess, updatesState, updatedUsedLetters =
            updateRoundStatus (List.item x.Round x.Guesses)

        { x with
            Guesses = listSet x.Guesses updatedGuess x.Round
            UsedLetters = updatedUsedLetters
            State = updatesState
            Round = advanceRound }
    else
        x

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

let keyboardChar usedLetters handler (c: string) =
    let colour =
        let letterStatus =
            match Map.tryFind (c.ToUpper()) usedLetters with
            | Some x -> x
            | None -> Black

        match letterStatus with
        | Black | Yellow -> "bg-gray-300"
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
    let gameState, setGameState = Hook.useState (startNewGame ())

    let writeState state =
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

                state |> submitEntry |> setGameState)

        let keyboardKey = keyboardChar state.UsedLetters onKeyClick
        let unusedLets = state.UsedLetters |> Map.toSeq |> Seq.map fst
        html
            $"""
            <div class="space-y-4">
                <div class="flex flex-row justify-center">
                    "Wordle: " {unusedLets}
                </div>
                <div class="flex flex-row justify-center">
                    {List.item 0 state.Guesses |> letterToDisplayBox}
                </div>
                <div class="flex flex-row justify-center">
                    {List.item 1 state.Guesses |> letterToDisplayBox}
                </div>
                <div class="flex flex-row justify-center">
                    {List.item 2 state.Guesses |> letterToDisplayBox}
                </div>
                <div class="flex flex-row justify-center">
                    {List.item 3 state.Guesses |> letterToDisplayBox}
                </div>
                <div class="flex flex-row justify-center">
                    {List.item 4 state.Guesses |> letterToDisplayBox}
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

    match gameState.State with
    | NotStarted -> gameState |> writeState
    | Started -> writeState gameState
    | Won -> gameState |> writeState
    | Lost -> startNewGame () |> writeState
