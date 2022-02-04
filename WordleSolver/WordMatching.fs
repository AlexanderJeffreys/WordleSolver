module WordleSolver.WordMatching

open System.Text.RegularExpressions

let wordLength = 5

type Guess = Guess of string
type Answer = Answer of string

type LetterClue =
    | MatchCorrectPosition
    | MatchWrongPosition
    | NoMatch

type Clue = Clue of LetterClue list

let prettyPrint (Clue letterClues) =
    letterClues
    |> List.map
        (function
        | MatchCorrectPosition -> "G"
        | MatchWrongPosition -> "Y"
        | NoMatch -> "R")
    |> String.concat ""

let matchPattern (Guess guess) (Answer actual) =
    let pairedLetters = Seq.zip guess actual

    // TODO: The logic for MatchWrongPosition is wrong. The real Wordle handles duplicates differently
    pairedLetters
    |> Seq.map
        (function
        | guessLetter, actualLetter when guessLetter = actualLetter -> MatchCorrectPosition
        | guessLetter, _ when actual.Contains(guessLetter) -> MatchWrongPosition
        | _ -> NoMatch)
    |> List.ofSeq
    |> Clue

let clueRegex = Regex(@"^[RYG]{5}")

let tryClueFromString (str: string) : Clue option =
    if clueRegex.IsMatch(str) then
        str
        |> Seq.map
            (function
            | 'G' -> MatchCorrectPosition
            | 'Y' -> MatchWrongPosition
            | 'R' -> NoMatch)
        |> List.ofSeq
        |> Clue
        |> Some
    else
        None

let clueFromString (str: string) =
    match tryClueFromString str with
    | Some clue -> clue
    | None -> failwith "Expected a valid clue string"
