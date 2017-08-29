module Uno.Persistence.InMemory.MemoryStore

open Microsoft.Extensions.Caching.Memory
open Uno.Domain.Game

let cacheOptions = MemoryCacheOptions ()

let private streams = new MemoryCache(cacheOptions)


let readStream<'T> (streamID : string) (version : int) (count : int) = 
    async {
        match streams.TryGetValue<'T list> streamID with
        | true, stream ->
            let events = stream |> List.skip version |> List.truncate count
            let last = version + events.Length
            let rest = stream.Length - last
            return  events, last, if rest > 0 then Some rest else None
        | false, _ ->
            return [], -1, Some 0
    }

let appendToStream<'T> (streamID : string) (expectedVersion : int) (event : 'T list) = 
    async {
        match streams.TryGetValue<'T list> streamID with
        | true, stream when stream.Length = expectedVersion ->
            let newStream = stream @ event
            streams.Set (streamID, newStream) |> ignore
        | false, _ when expectedVersion = -1 -> 
            streams.Set (streamID, event) |> ignore
        | _ -> 
            raise (exn "wrong expectedversion")
    }

let subscribe (projection : Event -> unit) =
    async {
        /// ???
    }