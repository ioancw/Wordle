module Lit.Wordle

open System
open Lit

Fable.Core.JsInterop.importSideEffects "./index.css"

type LetterStatus =
    | Green
    | Yellow
    | Grey
    | Black

type Guess =
    { Letter1: string option * LetterStatus
      Letter2: string option * LetterStatus
      Letter3: string option * LetterStatus
      Letter4: string option * LetterStatus
      Letter5: string option * LetterStatus }

type Position = int

type StartedState =
    { Wordle: string
      Guess1: Position * Guess
      Guess2: Position * Guess
      Guess3: Position * Guess
      Guess4: Position * Guess
      Guess5: Position * Guess
      Round: int
      State: GameState }

and GameState =
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
      Guess1 = emptyGuess
      Guess2 = emptyGuess
      Guess3 = emptyGuess
      Guess4 = emptyGuess
      Guess5 = emptyGuess
      Round = 1
      State = NotStarted }

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
    | 1 -> { x with Guess1 = addLetterToWord x.Guess1 }
    | 2 -> { x with Guess2 = addLetterToWord x.Guess2 }
    | 3 -> { x with Guess3 = addLetterToWord x.Guess3 }
    | 4 -> { x with Guess4 = addLetterToWord x.Guess4 }
    | 5 -> { x with Guess5 = addLetterToWord x.Guess5 }
    | _ -> failwithf "There is no higher purpose"

let submitDelete x =
    match x.Round with
    | 1 -> { x with Guess1 = getNewWordStateDelete x.Guess1 }
    | 2 -> { x with Guess2 = getNewWordStateDelete x.Guess2 }
    | 3 -> { x with Guess3 = getNewWordStateDelete x.Guess3 }
    | 4 -> { x with Guess4 = getNewWordStateDelete x.Guess4 }
    | 5 -> { x with Guess5 = getNewWordStateDelete x.Guess5 }
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
// refactor this, bit of a mess
let submitEnter x =
    let advanceRound = x.Round + 1
    let guess =
        match x.Round with
        | 1 -> {x with 
                    Guess1 = fst x.Guess1, x.Guess1 |> getWord |> getAnswerMask (x.Wordle); 
                    Round = advanceRound
                    State = if (x.Guess1 |> getWord) = x.Wordle then Won else Started x}
        | 2 -> {x with Guess2 = fst x.Guess2, x.Guess2 |> getWord |> getAnswerMask (x.Wordle); Round = advanceRound}
        | 3 -> {x with Guess3 = fst x.Guess3, x.Guess3 |> getWord |> getAnswerMask (x.Wordle); Round = advanceRound}
        | 4 -> {x with Guess4 = fst x.Guess4, x.Guess4 |> getWord |> getAnswerMask (x.Wordle); Round = advanceRound}
        | 5 -> {x with Guess5 = fst x.Guess5, x.Guess5 |> getWord |> getAnswerMask (x.Wordle); Round = x.Round}
        | _ -> {x with State = Lost}
    guess

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

let keyboardChar handler c =
    html
        $"""
        <div class="w-11 px-1 mb-2">
            <button
                @click={handler c}
                class="rounded-m m-px flex h-10 w-10 items-center justify-center uppercase bg-gray-300"
            >{c}</button>
        </div>
    """
[<LitElement("math-app")>]
let MatchComponent () =
    let _ = LitElement.init (fun cfg -> cfg.useShadowDom <- false)
    let gameState, setGameState = Hook.useState NotStarted

    let onKeyClick state (c: string) =
        Ev (fun ev ->
            ev.preventDefault ()
            let submitEntry = 
                match c with 
                | "Ent" -> submitEnter
                | "Del" -> submitDelete
                | _ -> submitLetter c
    
            state |> submitEntry |> Started |> setGameState)
    
    match gameState with
    | NotStarted ->
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
        // class="block w-full h-12 rounded bg-slate-300 hover:bg-slate-300 active:bg-slate-400 text-center leading-none text-white"            
        let keyboardKey = keyboardChar (onKeyClick startedState)

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

        let top =
            [ "q"; "w"; "e"; "r"; "t"; "y"; "u"; "i"; "o"; "p" ]
            |> List.map keyboardKey

        let middle =
            [ "a"; "s"; "d"; "f"; "g"; "h"; "j"; "k"; "l" ]
            |> List.map keyboardKey

        let bottom =
            [ "Ent"; "z"; "x"; "c"; "v"; "b"; "n"; "m"; "Del" ]
            |> List.map keyboardKey

        html
            $"""
            <div class="space-y-4">
                <div class="flex flex-row justify-center">
                        My little wordle game
                </div>
                <div class="flex flex-row justify-center">
                    {startedState.Guess1 |> letterToDisplayBox}
                </div>
                <div class="flex flex-row justify-center">
                    {startedState.Guess2 |> letterToDisplayBox}
                </div>
                <div class="flex flex-row justify-center">
                    {startedState.Guess3 |> letterToDisplayBox}
                </div>
                <div class="flex flex-row justify-center">
                    {startedState.Guess4 |> letterToDisplayBox}
                </div>
                <div class="flex flex-row justify-center">
                    {startedState.Guess5 |> letterToDisplayBox}
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
