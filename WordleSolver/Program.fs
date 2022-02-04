open System
open System.IO
open WordleSolver
open WordMatching
open WordleSolver.Solver

let wordListPath = "WordListLong.txt"
let possibleGuesses = File.ReadLines(wordListPath) |> Seq.map Guess
let possibleAnswers = File.ReadLines(wordListPath) |> Seq.map Answer

let rec guessLoop guessStep guessCount =
    let {Guess=(Guess word); ResponseHandler = responseHandler} = guessStep
    printfn $"Guess %i{guessCount}: %s{word}"
    let response = Console.ReadLine()

    match response with
    | "GGGGG" -> printfn "I win!"
    | _ -> guessLoop (responseHandler (clueFromString response)) (guessCount + 1)

guessLoop (guessFor possibleAnswers possibleGuesses) 1


