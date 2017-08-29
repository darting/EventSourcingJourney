// Learn more about F# at http://fsharp.org

open System

open Uno.Domain
open CommandHandler
open Deck
open Game 
open EventHandler

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let eventHandler = EventHandler ()

    

    0 // return an integer exit code
