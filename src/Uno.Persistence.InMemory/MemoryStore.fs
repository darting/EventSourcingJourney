module Uno.Persistence.InMemory.MemoryStore

open Microsoft.Extensions.Caching.Memory
open Uno.Domain.Game


type Store<'T> = {
    streams : MemoryCache
    projection : 'T -> unit
}

let cacheOptions = MemoryCacheOptions ()

let create () = {
    streams = new MemoryCache(cacheOptions)
    projection = fun _ -> ()
}

let readStream<'T> (store : Store<'T>) (streamID : string) (version : int) (count : int) = 
    async {
        match store.streams.TryGetValue<'T list> streamID with
        | true, stream ->
            let events = stream |> List.skip version |> List.truncate count
            let last = version + events.Length
            let rest = stream.Length - last
            return  events, last, if rest > 0 then Some rest else None
        | false, _ ->
            return [], -1, None
    }

let appendToStream<'T> (store : Store<'T>) (streamID : string) (expectedVersion : int) (events : 'T list) = 
    async {
        match store.streams.TryGetValue<'T list> streamID with
        | true, stream when stream.Length = expectedVersion ->
            let newStream = List.append stream events
            store.streams.Set<'T list> (streamID, newStream) |> ignore
            events |> Seq.iter store.projection
        | false, _ when expectedVersion = 0 -> 
            store.streams.Set<'T list> (streamID, events) |> Seq.iter store.projection
        | x -> 
            printfn "wrong : %A v:%d" x expectedVersion
            raise (exn "wrong expectedversion")
    }

let subscribe<'T> (projection : 'T -> unit) store =
    { store with projection = projection }