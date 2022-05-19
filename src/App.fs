module Lit.Wordle

open System
open Lit

Fable.Core.JsInterop.importSideEffects "./index.css"

type LetterStatus =
    | Green
    | Yellow
    | Grey
    | Black

type Word =
    { Letter1: string option * LetterStatus
      Letter2: string option * LetterStatus
      Letter3: string option * LetterStatus
      Letter4: string option * LetterStatus
      Letter5: string option * LetterStatus }

type StartedState =
    { Wordle: string
      Word1: int * Word
      Word2: int * Word
      Word3: int * Word
      Word4: int * Word
      Word5: int * Word
      Round: int
      State: GameState }

and GameState =
    | NotStarted
    | Won
    | Lost
    | Started of StartedState

let emptyWord =
    { Letter1 = None, Black
      Letter2 = None, Black
      Letter3 = None, Black
      Letter4 = None, Black
      Letter5 = None, Black }

let startNewGame =
    let wordles =
        [
            "BRINE"
            "CRIME"
            "DUCKS"
            "QUACK"
            "NIGHT"
            "PRIME"
            "ARISE"
            "BOZOS"
            "DICKS"
            "POTTY"
            "LANDS"
        ]

    { Wordle = "CRIME"
      Word1 = 1, emptyWord
      Word2 = 1, emptyWord
      Word3 = 1, emptyWord
      Word4 = 1, emptyWord
      Word5 = 1, emptyWord
      Round = 1
      State = NotStarted }

let getMask2 (actual: string) (guess: string) =
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

    let letters = Seq.zip actual (guess.ToUpper()) |> Seq.toList
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

let getNewWordStateAdd newLetter (position, word) =
    let advancedPosition = position + 1
    match position with
    | 1 -> advancedPosition, { word with Letter1 = newLetter }
    | 2 -> advancedPosition, { word with Letter2 = newLetter }
    | 3 -> advancedPosition, { word with Letter3 = newLetter }
    | 4 -> advancedPosition, { word with Letter4 = newLetter }
    | 5 -> advancedPosition, { word with Letter5 = newLetter }
    | _ -> position, word

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

let submitLetter input x =
    let addLetterToWord = getNewWordStateAdd (Some input, Black)
    match x.Round with
    | 1 -> { x with Word1 = addLetterToWord x.Word1 }
    | 2 -> { x with Word2 = addLetterToWord x.Word2 }
    | 3 -> { x with Word3 = addLetterToWord x.Word3 }
    | 4 -> { x with Word4 = addLetterToWord x.Word4 }
    | 5 -> { x with Word5 = addLetterToWord x.Word5 }
    | _ -> failwithf "There is no higher purpose"

let submitDelete x =
    match x.Round with
    | 1 -> { x with Word1 = getNewWordStateDelete x.Word1 }
    | 2 -> { x with Word2 = getNewWordStateDelete x.Word2 }
    | 3 -> { x with Word3 = getNewWordStateDelete x.Word3 }
    | 4 -> { x with Word4 = getNewWordStateDelete x.Word4 }
    | 5 -> { x with Word5 = getNewWordStateDelete x.Word5 }
    | _ -> failwithf "There is no higher purpose"

let getWord (_, word) =
    let letter (word, _) = defaultArg word ""

    [| letter word.Letter1
       letter word.Letter2
       letter word.Letter3
       letter word.Letter4
       letter word.Letter5 |]
    |> Seq.fold (+) ""

// check the word against the wordle, then set whether each letter is green, grey, yellow
let submitEnter x =
    let guess =
        match x.Round with
        | 1 -> {x with Word1 = fst x.Word1, x.Word1 |> getWord |> getMask2 (x.Wordle); Round = x.Round + 1}
        | 2 -> {x with Word2 = fst x.Word2, x.Word2 |> getWord |> getMask2 (x.Wordle); Round = x.Round + 1}
        | 3 -> {x with Word3 = fst x.Word3, x.Word3 |> getWord |> getMask2 (x.Wordle); Round = x.Round + 1}
        | 4 -> {x with Word4 = fst x.Word4, x.Word4 |> getWord |> getMask2 (x.Wordle); Round = x.Round + 1}
        | 5 -> {x with Word5 = fst x.Word5, x.Word5 |> getWord |> getMask2 (x.Wordle); Round = x.Round}
    guess

let isGameFinished (x: StartedState) =
    match x.State with
    | Lost -> true
    | _ -> false

let boxedChar ((c, status): string * LetterStatus) =
    match status with
    | Black ->
        // all this gumph here is tailwind style css
        // https://tailwindcss.com/docs/border-style
        html
            $"""
            <div class="border-solid border-transparent flex border-2 items-center rounded">
                <button class="w-14 h-14 bg-slate-500 text-center leading-none text-3xl font-bold text-white">{c}</button>
            </div>
        """
    | Grey ->
        html
            $"""
            <div class="border-solid border-transparent flex border-2 items-center rounded">
                <button class="w-14 h-14 bg-slate-800 text-center leading-none text-3xl font-bold text-white">{c}</button>
            </div>
        """
    | Green ->
        html
            $"""
            <div class="border-solid border-transparent flex border-2 items-center rounded">
                <button class="w-14 h-14 bg-green-600 text-center leading-none text-3xl font-bold text-white">{c}</button>
            </div>
        """
    | Yellow ->
        html
            $"""
            <div class="border-solid border-transparent flex border-2 items-center rounded">
                <button class="w-14 h-14 bg-yellow-400 text-center leading-none text-3xl font-bold text-white">{c}</button>
            </div>
        """

let startGameFromFinished setGameState =
    html
        $"""
        <div class="space-y-4">
            <div class="flex flex-row justify-center">
                "bla"
            </div>
            <div class="flex flex-row justify-center">
                "bla"
            </div>
            <div class="flex flex-row justify-center">
                <button
                    class="block p-6 rounded bg-gray-200 text-center leading-none font-6xl font-mono"
                    @click={Ev(fun _ -> startNewGame |> Started |> setGameState)}
                >
                    Go again
                </button>
            </div>
        </div>
    """

[<LitElement("math-app")>]
let MatchComponent () =
    let _ = LitElement.init (fun cfg -> cfg.useShadowDom <- false)

    let gameState, setGameState = Hook.useState NotStarted

    match gameState with
    | GameState.NotStarted ->
        html
            $"""
            <div class="flex flex-row justify-center mt-20">
                <button
                    class="block p-6 rounded bg-gray-300 text-center leading-none font-6xl font-mono"
                    @click={Ev(fun _ -> startNewGame |> Started |> setGameState)}
                >
                    Start
                </button>
            </div>
        """
    | Started startedState ->
        let onKeyClick (c: string) =
            Ev (fun ev ->
                ev.preventDefault ()

                startedState
                |> submitLetter c
                |> Started
                |> setGameState)

        let enter c =
            Ev (fun ev ->
                ev.preventDefault ()

                startedState
                |> submitEnter
                |> Started
                |> setGameState)

        let delete c =
            Ev (fun ev ->
                ev.preventDefault ()

                startedState
                |> submitDelete
                |> Started
                |> setGameState)

        let keyboardChar handler c =
            html
                $"""
                <div class="w-11 px-1 mb-2">
                    <button
                        @click={handler c}
                        class="block w-full h-12 rounded bg-slate-300 hover:bg-slate-300 active:bg-slate-400 text-center leading-none text-white"
                    >{c}</button>
                </div>
            """

        let keyboardKey = keyboardChar onKeyClick
        let enterChar = keyboardChar enter
        let deleteChar = keyboardChar delete

        //let firstRow = startedState.Word1 |> Array.map boxedChar
        let getLetter (_, word) =
            let letter (word, status) =
                let letterString = defaultArg word ""
                letterString.ToUpper(), status

            [ letter word.Letter1
              letter word.Letter2
              letter word.Letter3
              letter word.Letter4
              letter word.Letter5 ]

        let secondRow =
            startedState.Word2
            |> getLetter
            |> List.map boxedChar

        let thirdRow =
            startedState.Word3
            |> getLetter
            |> List.map boxedChar

        let fourthRow =
            startedState.Word4
            |> getLetter
            |> List.map boxedChar

        let fifthRow =
            startedState.Word5
            |> getLetter
            |> List.map boxedChar

        let top =
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
            |> List.map keyboardKey

        let middle =
            [ "a"
              "s"
              "d"
              "f"
              "g"
              "h"
              "j"
              "k"
              "l" ]
            |> List.map keyboardKey

        let bottom =
            let bottomChars =
                [ "z"; "x"; "c"; "v"; "b"; "n"; "m" ]
                |> List.map keyboardKey

            [ enterChar "Ent" ]
            @ bottomChars @ [ deleteChar "Del" ]

        html
            $"""
            <div class="space-y-4">
                <div class="flex flex-row justify-center">
                        My little wordle game
                </div>
                <div class="flex flex-row justify-center">
                    {startedState.Word1
                     |> getLetter
                     |> List.map boxedChar}
                </div>
                <div class="flex flex-row justify-center">
                    {secondRow}
                </div>
                <div class="flex flex-row justify-center">
                    {thirdRow}
                </div>
                <div class="flex flex-row justify-center">
                    {fourthRow}
                </div>
                <div class="flex flex-row justify-center">
                    {fifthRow}
                </div>
                <div class="flex flew-row justify-center">
                    {top}
                </div>
                <div class="flex flew-row justify-center mb-1">
                    {middle}
                </div>
                <div class="flex flew-row justify-center">
                    {bottom}
                </div>
            </div>
        """
