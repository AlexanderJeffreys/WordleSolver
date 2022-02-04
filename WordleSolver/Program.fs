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

let rec guessUntilDone guessAction guessCount =

    match guessAction with
    | GiveUp -> printfn "I give up!"
    | Gloat -> printfn "I win!"
    | MakeGuess { NextGuess = (Guess word)
                  ResponseHandler = responseHandler } ->

        printfn $"Guess %i{guessCount}: %s{word}"
        let clue = getResponseUntilValid ()
        guessUntilDone (responseHandler clue) (guessCount + 1)

guessUntilDone (guessFor wordList) 1
