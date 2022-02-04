open System
open System.IO
open WordleSolver
open WordMatching
open WordleSolver.Solver

let wordListPath = "WordListLong.txt"
let possibleGuesses = File.ReadLines(wordListPath) |> Seq.map Guess
let possibleAnswers = File.ReadLines(wordListPath) |> Seq.map Answer

let allGoodClue = clueFromString "GGGGG"

let rec getResponseUntilValid () =
    let response = Console.ReadLine()
    let clue = tryClueFromString (response.ToUpper())
    match clue with
    | Some clue -> clue
    | None ->
        printfn "Invalid response, please try again"
        getResponseUntilValid ()

let rec guessLoop guessAttempt guessCount =

    match guessAttempt with
    | GiveUp -> printfn "I give up!"
    | MakeGuess
        {
            Guess=(Guess guess)
            ResponseHandler=handler
        } ->

        printfn $"Guess %i{guessCount}: %s{guess}"
        let clue = getResponseUntilValid ()

        if clue = allGoodClue then (printfn "I win!")
        else guessLoop (handler clue) (guessCount + 1)

guessLoop (guessFor possibleAnswers possibleGuesses) 1
