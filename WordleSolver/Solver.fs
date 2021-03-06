module WordleSolver.Solver

open WordMatching

type GuessAction =
    | MakeGuess of GuessStep
    | Gloat
    | GiveUp

and GuessStep =
    { NextGuess: Guess
      ResponseHandler: Clue -> GuessAction }

let scoreAverage possibleAnswers guess =
    possibleAnswers
    |> Seq.groupBy (matchPattern guess)
    |> Seq.map (fun (_, values) -> Seq.length values)
    |> Seq.map (fun x -> x * x)
    |> Seq.sum

let scoreMax possibleAnswers guess =
    possibleAnswers
    |> Seq.groupBy (matchPattern guess)
    |> Seq.map (fun (_, values) -> Seq.length values)
    |> Seq.max

let bestGuess possibleAnswers guessOptions =
    match (List.ofSeq possibleAnswers) with
    | [] -> None
    | [ (Answer word) ] -> Some(Guess word)
    | _ ->
        guessOptions
        |> Seq.minBy (scoreAverage possibleAnswers)
        |> Some

let matchingAnswers possibleAnswers guess clue =
    possibleAnswers
    |> Seq.where (fun answer -> matchPattern guess answer = clue)

let allGoodClue = clueFromString "GGGGG"

let rec guessForAnswers possibleAnswers guessOptions =
    printfn $"%i{Seq.length possibleAnswers} possibilities remaining"
    let guessAttempt = bestGuess possibleAnswers guessOptions

    match guessAttempt with
    | None -> GiveUp
    | Some guess ->
        MakeGuess
            { NextGuess = guess
              ResponseHandler = fun clue ->
                  match clue with
                  | clue when clue = allGoodClue -> Gloat
                  | _ -> guessForAnswers (matchingAnswers possibleAnswers guess clue) guessOptions }

let guessFor wordList =
    guessForAnswers (wordList |> Seq.map Answer) (wordList |> Seq.map Guess)
