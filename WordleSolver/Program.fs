open System
open System.IO
let wordList = File.ReadLines("WordList.txt")

type Word = Word of string

type LetterClue =
    | MatchCorrectPosition
    | MatchWrongPosition
    | NoMatch
type Clue = Clue of LetterClue list

let prettyPrint (Clue letterClues) =
    letterClues
    |> List.map (
        fun x ->
            match x with
            | MatchCorrectPosition -> "G"
            | MatchWrongPosition -> "Y"
            | NoMatch -> "R")
    |> String.concat ""

let pairUp (string1:string) (string2:string) =

    let rec pairUpLists list1 list2 =
        match list1, list2 with
        | [], [] -> []
        | head1::tail1, head2::tail2 -> (head1, head2)::(pairUpLists tail1 tail2)
        | _ -> invalidOp "Lists have mismatching number of elements"

    pairUpLists (string1 |> Seq.toList) (string2 |> Seq.toList)

let matchPattern (Word guess) (Word actual) =
    let pairedLetters = pairUp guess actual
    pairedLetters
    |> List.map (
        fun (guessLetter, actualLetter) ->
        if guessLetter = actualLetter then MatchCorrectPosition
        elif actual.Contains(guessLetter) then MatchWrongPosition
        else NoMatch
        )
    |> Clue


