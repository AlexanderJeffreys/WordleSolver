open System
open System.IO
open WordleSolver
open WordMatching
open WordleSolver.Solver

let wordListPath = "WordListLong.txt"
let wordList = File.ReadLines(wordListPath)

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
    | Gloat -> printfn "I win!"
    | MakeGuess { NextGuess = (Guess word)
                  ResponseHandler = responseHandler } ->

        printfn $"Guess %i{guessCount}: %s{word}"
        let clue = getResponseUntilValid ()
        guessLoop (responseHandler clue) (guessCount + 1)

guessLoop (guessFor wordList) 1
