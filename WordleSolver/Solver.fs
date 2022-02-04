module WordleSolver.Solver

open WordMatching

type GuessStep = {
    Guess: Guess
    ResponseHandler: Clue -> GuessStep
}

let score possibleAnswers guess =
    possibleAnswers
    |> Seq.groupBy (matchPattern guess)
    |> Seq.map (fun (_, values) -> Seq.length values)
    |> Seq.max


let bestGuess (possibleAnswers: Answer seq) (guessOptions: Guess seq) : Guess =
    match (List.ofSeq possibleAnswers) with
    | [ (Answer word) ] -> Guess word
    | _ -> guessOptions |> Seq.minBy (score possibleAnswers)

let matchingAnswers possibleAnswers guess clue =
    possibleAnswers
    |> Seq.where (fun answer -> matchPattern guess answer = clue)

let rec guessFor possibleAnswers guessOptions =
    let guess = bestGuess possibleAnswers guessOptions

    let responseHandler clue = guessFor (matchingAnswers possibleAnswers guess clue) guessOptions

    {
        Guess = guess
        ResponseHandler = responseHandler
    }


