module WordleSolver.WordMatching

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

let pairUp (string1: string) (string2: string) =

    let rec pairUpLists list1 list2 =
        match list1, list2 with
        | [], [] -> []
        | head1 :: tail1, head2 :: tail2 -> (head1, head2) :: (pairUpLists tail1 tail2)
        | _ -> invalidOp "Lists have mismatching number of elements"

    pairUpLists (string1 |> Seq.toList) (string2 |> Seq.toList)

let matchPattern (Guess guess) (Answer actual) =
    let pairedLetters = pairUp guess actual

    pairedLetters
    |> List.map
        (function
        | guessLetter, actualLetter when guessLetter = actualLetter -> MatchCorrectPosition
        | guessLetter, _ when actual.Contains(guessLetter) -> MatchWrongPosition
        | _ -> NoMatch)
    |> Clue

let clueFromString (str:string) : Clue =
    if str.Length <> wordLength then failwith $"Not a valid clue: %s{str}"
    else str
         |> Seq.map (function
             | 'G' -> MatchCorrectPosition
             | 'Y' -> MatchWrongPosition
             | 'R' -> NoMatch)
         |> List.ofSeq
         |> Clue
