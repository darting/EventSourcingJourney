module Uno.Domain.CommandHandler

open Game

module Game = 
    type Agent<'T> = MailboxProcessor<'T>

    let create readStream appendToStream = 
        let streamId gameId = sprintf "Gamme-%O" gameId
        let load gameId = 
            let rec fold state version = 
                async {
                    let! events, lastEvent, nextEvent =
                        readStream (streamId gameId) version 500

                    let state = List.fold State.evolve state events
                    match nextEvent with
                    | None -> return lastEvent, state
                    | Some n -> return! fold state n
                }
            fold State.InitialState 0

        let save gameId expectedVersioin events =
            appendToStream (streamId gameId) expectedVersioin events

        let start gameId =
            Agent.Start 
            <| fun inbox -> 
                let rec loop version state =
                    async {
                        let! command = inbox.Receive() 
                        let events = handle command state
                        do! save gameId version events

                        let newState = List.fold State.evolve state events
                        return! loop (version + List.length events) newState
                    }
                async {
                    let! version, state = load gameId
                    return! loop version state
                }

        let forward (agent : Agent<_>) command = agent.Post command

        let dispatcher =
            Agent.Start 
            <| fun inbox ->
                let rec loop aggregates =
                    async {
                        let! command = inbox.Receive()
                        let id = gameId command 
                        match Map.tryFind id aggregates with 
                        | Some aggregate ->
                            forward aggregate command 
                        | None ->
                            let aggregate = start id
                            forward aggregate command 
                            return! loop (Map.add id aggregate aggregates)
                    }
                loop Map.empty
        
        fun command -> dispatcher.Post command